;;; org-files-db-cache.el --- Async predefined-view cache -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Hubmann

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; In-memory pre-caching for predefined query and search views.  File
;; notifications are only wake-up signals.  orgfdb remains authoritative for
;; the database identity and committed generation.  Each rebuild runs in one
;; child Emacs, prepares complete completion candidates there, and transports
;; the finished representation through one short-lived readable Lisp dump.

;;; Code:

(require 'async)
(require 'cl-lib)
(require 'filenotify nil t)
(require 'org-files-db-core)
(require 'seq)
(require 'subr-x)

(defvar org-files-db-cache-mode)

(defconst org-files-db-cache--format-version 1
  "Version of the Phase 1 predefined-view cache representation.")

(defconst org-files-db-cache--library-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the loaded cache module.")

(cl-defstruct org-files-db-cache--entry
  "One atomically published predefined-view cache entry."
  view-name
  view-token
  cache-key
  config-file
  config-key
  database-id
  generation
  candidates
  lookup
  presentation
  result-count
  candidate-count
  published-at)

(cl-defstruct org-files-db-cache--job
  "One complete asynchronous predefined-view rebuild."
  id
  view-name
  view
  view-token
  cache-key
  config-file
  config-key
  database-id
  generation
  reason
  event-info
  requested-at
  started-at
  process
  dump-file
  status)

(cl-defstruct org-files-db-cache--watch
  "One SQLite directory watcher for an effective configuration."
  config-file
  config-key
  database-path
  directory
  descriptor
  debounce-timer
  pending-event-count
  first-event-at)

(defvar org-files-db-cache--entries (make-hash-table :test #'equal)
  "Published entries keyed by predefined view name.")

(defvar org-files-db-cache--states (make-hash-table :test #'equal)
  "Last authoritative orgfdb status keyed by effective configuration.")

(defvar org-files-db-cache--watchers (make-hash-table :test #'equal)
  "Active `org-files-db-cache--watch' objects keyed by configuration.")

(defvar org-files-db-cache--expected-jobs (make-hash-table :test #'equal)
  "Newest expected job ID for each predefined view.")

(defvar org-files-db-cache--failures (make-hash-table :test #'equal)
  "Last cache worker failure keyed by predefined view name.")

(defvar org-files-db-cache--owned-dumps (make-hash-table :test #'equal)
  "Temporary dump paths currently owned by the cache.")

(defvar org-files-db-cache--queue nil
  "FIFO queue of pending `org-files-db-cache--job' objects.")

(defvar org-files-db-cache--current-job nil
  "Currently running `org-files-db-cache--job', or nil.")

(defvar org-files-db-cache--current-worker nil
  "Current async.el child process, or nil.")

(defvar org-files-db-cache--job-counter 0
  "Monotonic cache job identifier.")

(defvar org-files-db-cache--async-library nil
  "Cached absolute path of async.el used by child Emacs processes.")

(defvar org-files-db-cache--defer-worker-start nil
  "Non-nil temporarily defers starting the next queued cache worker.")

(defvar org-files-db-cache--file-events nil
  "Raw file-notify events, newest first.")

(defvar org-files-db-cache--benchmarks nil
  "Structured cache lifecycle benchmark records, newest first.")

(defvar org-files-db-cache--superseded-count 0
  "Number of jobs superseded by newer required work.")

(defvar org-files-db-cache--cancelled-count 0
  "Number of jobs cancelled for reasons other than supersession.")

(defun org-files-db-cache--debug (format-string &rest arguments)
  "Report FORMAT-STRING with ARGUMENTS when cache debugging is enabled."
  (when org-files-db-cache-debug
    (apply #'message
           (concat "org-files-db cache: " format-string)
           arguments)))

(defun org-files-db-cache--elapsed (started-at)
  "Return elapsed seconds since STARTED-AT."
  (- (float-time) started-at))

(defun org-files-db-cache--config-key (config-file)
  "Return stable cache key for effective CONFIG-FILE."
  (or config-file :no-config))

(defun org-files-db-cache--required-value (object key context)
  "Return OBJECT KEY or signal a CONTEXT error when absent."
  (unless (org-files-db--has-key-p object key)
    (signal 'org-files-db-error
            (list (format "%s is missing required field %s" context key))))
  (org-files-db--get object key))

(defun org-files-db-cache--string-value (value field context)
  "Validate VALUE as non-empty string FIELD in CONTEXT."
  (unless (and (stringp value) (not (string-empty-p value)))
    (signal 'org-files-db-error
            (list (format "%s has invalid %s: %S" context field value))))
  value)

(defun org-files-db-cache--integer-value (value field context)
  "Validate VALUE as non-negative integer FIELD in CONTEXT."
  (unless (and (integerp value) (>= value 0))
    (signal 'org-files-db-error
            (list (format "%s has invalid %s: %S" context field value))))
  value)

(defun org-files-db-cache--normalize-index-state (response)
  "Return normalized authoritative state from orgfdb status RESPONSE."
  (let* ((context "orgfdb status response")
         (database-id
          (org-files-db-cache--string-value
           (org-files-db-cache--required-value response 'database_id context)
           "database_id" context))
         (generation
          (org-files-db-cache--integer-value
           (org-files-db-cache--required-value response 'generation context)
           "generation" context))
         (database-path
          (org-files-db-cache--string-value
           (org-files-db-cache--required-value response 'database_path context)
           "database_path" context)))
    (unless (file-name-absolute-p database-path)
      (signal 'org-files-db-error
              (list (format "%s has non-canonical database_path: %S"
                            context database-path))))
    (list :database-id database-id
          :generation generation
          :database-path (expand-file-name database-path)
          :schema-version (org-files-db--get response 'schema_version)
          :last-changed-at (org-files-db--get response 'last_changed_at))))

(defun org-files-db-cache--status-arguments (config-file)
  "Return status arguments for effective CONFIG-FILE."
  (append (org-files-db--config-arguments config-file "View cache status")
          '("--format" "json")))

(defun org-files-db-cache--read-index-state (config-file)
  "Read authoritative database state for effective CONFIG-FILE."
  (org-files-db-cache--normalize-index-state
   (org-files-db--call
    "status" (org-files-db-cache--status-arguments config-file))))

(defun org-files-db-cache--same-state-p (left right)
  "Return non-nil when LEFT and RIGHT identify the same committed state."
  (and left right
       (equal (plist-get left :database-id)
              (plist-get right :database-id))
       (= (plist-get left :generation)
          (plist-get right :generation))))

(defun org-files-db-cache--view-pre-cache-p (view)
  "Return non-nil when VIEW opts into pre-caching."
  (eq (plist-get (cdr view) :pre-cache) t))

(defun org-files-db-cache--view-command (view)
  "Return VIEW command, validating the supported Phase 1 commands."
  (let ((command (plist-get (cdr view) :command)))
    (unless (memq command '(query search))
      (user-error "View `%s' has invalid :command %S" (car view) command))
    command))

(defun org-files-db-cache--view-config-file (view)
  "Return the effective validated configuration file for VIEW."
  (let ((properties (cdr view)))
    (org-files-db--resolve-config-file
     (plist-get properties :config-file)
     (not (null (plist-member properties :config-file)))
     (format "View `%s'" (car view)))))

(defun org-files-db-cache--configured-pre-cache-views ()
  "Return copies of configured views marked with :pre-cache t."
  (when (fboundp 'org-files-db-views--validate)
    (org-files-db-views--validate))
  (let (views)
    (dolist (view (and (boundp 'org-files-db-views) org-files-db-views))
      (when (org-files-db-cache--view-pre-cache-p view)
        (push (copy-tree view) views)))
    (nreverse views)))

(defun org-files-db-cache--worker-view-data (view)
  "Return serializable worker-relevant data copied from VIEW."
  (let* ((properties (cdr view))
         (data
          (list (car view)
                :command (org-files-db-cache--view-command view)
                :query (copy-tree (plist-get properties :query))
                :expression (plist-get properties :expression)
                :scope (or (plist-get properties :scope) 'all)
                :columns (copy-tree (plist-get properties :columns)))))
    (when (plist-member properties :sort)
      (setq data (plist-put data :sort (copy-tree (plist-get properties :sort)))))
    data))

(defun org-files-db-cache--presentation-options ()
  "Return serializable presentation options affecting prepared candidates."
  (list :heading-columns (copy-tree org-files-db-heading-columns)
        :file-columns (copy-tree org-files-db-file-columns)
        :link-columns (copy-tree org-files-db-link-columns)
        :search-columns (copy-tree org-files-db-search-columns)
        :heading-sort (copy-tree org-files-db-heading-sort)
        :file-sort (copy-tree org-files-db-file-sort)
        :link-sort (copy-tree org-files-db-link-sort)
        :search-sort (copy-tree org-files-db-search-sort)
        :outline-separator org-files-db-outline-path-separator
        :outline-include-root org-files-db-outline-path-include-root
        :outline-include-match org-files-db-outline-path-include-match
        :truncate-position org-files-db-truncate-position
        :truncate-marker org-files-db-truncate-marker))

(defun org-files-db-cache--view-token (view config-file)
  "Return immutable presentation token for VIEW and CONFIG-FILE."
  (list :format-version org-files-db-cache--format-version
        :view (org-files-db-cache--worker-view-data view)
        :config-file config-file
        :presentation-options (org-files-db-cache--presentation-options)))

(defun org-files-db-cache--cache-key (view config-file state)
  "Return complete logical cache key for VIEW, CONFIG-FILE, and STATE."
  (list :view-token (org-files-db-cache--view-token view config-file)
        :database-id (plist-get state :database-id)
        :generation (plist-get state :generation)))

(defun org-files-db-cache--current-view (name)
  "Return a copy of currently configured predefined view NAME, or nil."
  (when-let* ((view (and (boundp 'org-files-db-views)
                         (assoc name org-files-db-views #'string=))))
    (copy-tree view)))

(defun org-files-db-cache--entry-valid-p (entry view config-file state)
  "Return non-nil when ENTRY is warm for VIEW, CONFIG-FILE, and STATE."
  (and entry state
       (equal (org-files-db-cache--entry-view-token entry)
              (org-files-db-cache--view-token view config-file))
       (equal (org-files-db-cache--entry-cache-key entry)
              (org-files-db-cache--cache-key view config-file state))
       (equal (org-files-db-cache--entry-database-id entry)
              (plist-get state :database-id))
       (= (org-files-db-cache--entry-generation entry)
          (plist-get state :generation))))

(defun org-files-db-cache--views-for-config (config-file)
  "Return pre-cached views using effective CONFIG-FILE."
  (seq-filter
   (lambda (view)
     (condition-case nil
         (equal (org-files-db-cache--view-config-file view) config-file)
       (error nil)))
   (org-files-db-cache--configured-pre-cache-views)))

(defun org-files-db-cache--record-raw-file-event (watch event)
  "Record raw file-notify EVENT received for WATCH."
  (let ((record
         (list :timestamp (float-time)
               :config-key (org-files-db-cache--watch-config-key watch)
               :descriptor (car-safe event)
               :action (nth 1 event)
               :path (nth 2 event)
               :second-path (nth 3 event))))
    (push record org-files-db-cache--file-events)
    record))

(defun org-files-db-cache--sqlite-event-p (watch event)
  "Return non-nil when WATCH EVENT concerns SQLite database files."
  (or (eq (nth 1 event) 'stopped)
      (let* ((directory (org-files-db-cache--watch-directory watch))
             (database-path (org-files-db-cache--watch-database-path watch))
             (wal-path (concat database-path "-wal"))
             (shm-path (concat database-path "-shm")))
        (seq-some
         (lambda (path)
           (when (stringp path)
             (let ((expanded (expand-file-name path directory)))
               (member expanded (list database-path wal-path shm-path)))))
         (list (nth 2 event) (nth 3 event))))))

(defun org-files-db-cache--remove-watch (config-key)
  "Remove active file-notify watch for CONFIG-KEY."
  (when-let* ((watch (gethash config-key org-files-db-cache--watchers)))
    (when-let* ((timer (org-files-db-cache--watch-debounce-timer watch)))
      (when (timerp timer)
        (cancel-timer timer))
      (setf (org-files-db-cache--watch-debounce-timer watch) nil))
    (when-let* ((descriptor (org-files-db-cache--watch-descriptor watch)))
      (when (fboundp 'file-notify-rm-watch)
        (condition-case nil
            (file-notify-rm-watch descriptor)
          (error nil))))
    (remhash config-key org-files-db-cache--watchers)))

(defun org-files-db-cache--file-event-callback (config-key event)
  "Handle raw file-notify EVENT for CONFIG-KEY."
  (when (and org-files-db-cache-mode
             (gethash config-key org-files-db-cache--watchers))
    (let ((watch (gethash config-key org-files-db-cache--watchers)))
      (org-files-db-cache--record-raw-file-event watch event)
      (when (eq (nth 1 event) 'stopped)
        ;; A stopped descriptor is dead even if its path is unchanged.
        (setf (org-files-db-cache--watch-descriptor watch) nil))
      (when (org-files-db-cache--sqlite-event-p watch event)
        (cl-incf (org-files-db-cache--watch-pending-event-count watch))
        (unless (org-files-db-cache--watch-first-event-at watch)
          (setf (org-files-db-cache--watch-first-event-at watch) (float-time)))
        (when-let* ((timer (org-files-db-cache--watch-debounce-timer watch)))
          (when (timerp timer)
            (cancel-timer timer)))
        (setf
         (org-files-db-cache--watch-debounce-timer watch)
         (run-at-time
          (max 0.0 org-files-db-cache-file-notify-debounce) nil
          #'org-files-db-cache--debounced-status-check config-key))))))

(defun org-files-db-cache--ensure-watcher (config-file state)
  "Ensure CONFIG-FILE has a watcher for canonical database STATE."
  (unless (and (featurep 'filenotify)
               (fboundp 'file-notify-add-watch))
    (user-error "File notification support is required for org-files-db-cache-mode"))
  (let* ((config-key (org-files-db-cache--config-key config-file))
         (database-path (plist-get state :database-path))
         (directory (file-name-directory database-path))
         (existing (gethash config-key org-files-db-cache--watchers)))
    (when (file-remote-p database-path)
      (user-error "View cache database must be local: %s" database-path))
    (unless (file-directory-p directory)
      (user-error "View cache database directory does not exist: %s" directory))
    (unless (and existing
                 (equal database-path
                        (org-files-db-cache--watch-database-path existing))
                 (org-files-db-cache--watch-descriptor existing))
      (org-files-db-cache--remove-watch config-key)
      (let* ((watch
              (make-org-files-db-cache--watch
               :config-file config-file
               :config-key config-key
               :database-path database-path
               :directory directory
               :pending-event-count 0))
             (descriptor
              (file-notify-add-watch
               directory '(change attribute-change)
               (lambda (event)
                 (org-files-db-cache--file-event-callback config-key event)))))
        (setf (org-files-db-cache--watch-descriptor watch) descriptor)
        (puthash config-key watch org-files-db-cache--watchers)
        (org-files-db-cache--debug
         "watch installed config=%s path=%s" config-file database-path)))
    (gethash config-key org-files-db-cache--watchers)))

(defun org-files-db-cache--remove-config-entries (config-key)
  "Remove published cache entries belonging to CONFIG-KEY."
  (let (names)
    (maphash
     (lambda (name entry)
       (when (equal config-key (org-files-db-cache--entry-config-key entry))
         (push name names)))
     org-files-db-cache--entries)
    (dolist (name names)
      (remhash name org-files-db-cache--entries))))

(defun org-files-db-cache--event-info (watch old-state new-state status-check-at)
  "Return refresh trace data for WATCH from OLD-STATE to NEW-STATE.
STATUS-CHECK-AT is the timestamp of the authoritative status check."
  (let ((info
         (list :raw-event-count
               (org-files-db-cache--watch-pending-event-count watch)
               :first-event-at (org-files-db-cache--watch-first-event-at watch)
               :status-check-at status-check-at
               :old-generation (and old-state
                                    (plist-get old-state :generation))
               :new-generation (plist-get new-state :generation)
               :generation-detected-at (float-time))))
    (setf (org-files-db-cache--watch-pending-event-count watch) 0
          (org-files-db-cache--watch-first-event-at watch) nil)
    info))

(defun org-files-db-cache--record-generation-check (watch old-state new-state info)
  "Record WATCH generation transition from OLD-STATE to NEW-STATE using INFO."
  (push
   (append
    (list :type 'generation-check
          :status (if (org-files-db-cache--same-state-p old-state new-state)
                      'unchanged
                    'changed)
          :config-key (org-files-db-cache--watch-config-key watch)
          :database-id (plist-get new-state :database-id))
    info)
   org-files-db-cache--benchmarks))

(defun org-files-db-cache--delete-dump (file)
  "Delete owned transport FILE and return deletion duration."
  (let ((started (float-time)))
    (when (stringp file)
      (condition-case nil
          (when (file-exists-p file)
            (delete-file file))
        (error nil))
      (remhash file org-files-db-cache--owned-dumps))
    (org-files-db-cache--elapsed started)))

(defun org-files-db-cache--cleanup-job-dump (job)
  "Delete JOB transport dump and clear its ownership."
  (when job
    (let ((file (org-files-db-cache--job-dump-file job)))
      (setf (org-files-db-cache--job-dump-file job) nil)
      (org-files-db-cache--delete-dump file))))

(defun org-files-db-cache--cleanup-process-buffers (process)
  "Kill the main async.el buffer associated with PROCESS when possible."
  (when (processp process)
    (when-let* ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun org-files-db-cache--job-expected-p (job)
  "Return non-nil when JOB is still the expected rebuild for its view."
  (equal (gethash (org-files-db-cache--job-view-name job)
                  org-files-db-cache--expected-jobs)
         (org-files-db-cache--job-id job)))

(defun org-files-db-cache--record-cancellation (job status replacement-generation)
  "Record JOB cancellation STATUS and optional REPLACEMENT-GENERATION."
  (let* ((started (or (org-files-db-cache--job-started-at job)
                      (org-files-db-cache--job-requested-at job)))
         (dump (org-files-db-cache--job-dump-file job))
         (dump-created-p
          (and dump
               (ignore-errors
                 (and (file-regular-p dump)
                      (> (file-attribute-size (file-attributes dump)) 0)))))
         (cleanup-duration (org-files-db-cache--cleanup-job-dump job)))
    (if (eq status 'superseded)
        (cl-incf org-files-db-cache--superseded-count)
      (cl-incf org-files-db-cache--cancelled-count))
    (push
     (list :type 'rebuild
           :status status
           :job-id (org-files-db-cache--job-id job)
           :view (org-files-db-cache--job-view-name job)
           :reason (org-files-db-cache--job-reason job)
           :database-id (org-files-db-cache--job-database-id job)
           :generation (org-files-db-cache--job-generation job)
           :replacement-generation replacement-generation
           :time-before-cancellation (org-files-db-cache--elapsed started)
           :dump-created-p dump-created-p
           :cleanup-duration cleanup-duration)
     org-files-db-cache--benchmarks)))

(defun org-files-db-cache--cancel-job (job status &optional replacement-generation)
  "Cancel JOB with STATUS and optional REPLACEMENT-GENERATION."
  (when job
    (setf (org-files-db-cache--job-status job) status)
    (let ((process (org-files-db-cache--job-process job)))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (org-files-db-cache--cleanup-process-buffers process))
    (org-files-db-cache--record-cancellation
     job status replacement-generation)))

(defun org-files-db-cache--cancel-current-job (status &optional replacement-generation)
  "Cancel the current job using STATUS and REPLACEMENT-GENERATION."
  (when org-files-db-cache--current-job
    (let ((job org-files-db-cache--current-job))
      (setq org-files-db-cache--current-job nil
            org-files-db-cache--current-worker nil)
      (org-files-db-cache--cancel-job job status replacement-generation))))

(defun org-files-db-cache--remove-queued-view-jobs (name status replacement-generation)
  "Remove queued jobs for NAME using STATUS and REPLACEMENT-GENERATION."
  (let (retained)
    (dolist (job org-files-db-cache--queue)
      (if (equal (org-files-db-cache--job-view-name job) name)
          (org-files-db-cache--cancel-job job status replacement-generation)
        (push job retained)))
    (setq org-files-db-cache--queue (nreverse retained))))

(defun org-files-db-cache--supersede-config-jobs (config-key state)
  "Supersede stale jobs for CONFIG-KEY using newest STATE."
  (let ((replacement-generation (plist-get state :generation)))
    (when (and org-files-db-cache--current-job
               (equal config-key
                      (org-files-db-cache--job-config-key
                       org-files-db-cache--current-job))
               (or (not (equal (plist-get state :database-id)
                               (org-files-db-cache--job-database-id
                                org-files-db-cache--current-job)))
                   (/= replacement-generation
                       (org-files-db-cache--job-generation
                        org-files-db-cache--current-job))))
      (org-files-db-cache--cancel-current-job
       'superseded replacement-generation))
    (let (retained)
      (dolist (job org-files-db-cache--queue)
        (if (and (equal config-key
                        (org-files-db-cache--job-config-key job))
                 (or (not (equal (plist-get state :database-id)
                                 (org-files-db-cache--job-database-id job)))
                     (/= replacement-generation
                         (org-files-db-cache--job-generation job))))
            (org-files-db-cache--cancel-job
             job 'superseded replacement-generation)
          (push job retained)))
      (setq org-files-db-cache--queue (nreverse retained)))))

(defun org-files-db-cache--request-rebuild
    (view config-file state reason &optional event-info force)
  "Queue complete VIEW rebuild for CONFIG-FILE at STATE.
REASON and EVENT-INFO describe why the rebuild was requested.  FORCE replaces
an equivalent queued or running job."
  (let* ((name (car view))
         (config-key (org-files-db-cache--config-key config-file))
         (view-token (org-files-db-cache--view-token view config-file))
         (cache-key (org-files-db-cache--cache-key view config-file state))
         (expected-id (gethash name org-files-db-cache--expected-jobs))
         (current org-files-db-cache--current-job)
         (equivalent-current
          (and current expected-id
               (= expected-id (org-files-db-cache--job-id current))
               (equal name (org-files-db-cache--job-view-name current))
               (equal cache-key (org-files-db-cache--job-cache-key current))))
         (equivalent-queued
          (seq-find
           (lambda (job)
             (and expected-id
                  (= expected-id (org-files-db-cache--job-id job))
                  (equal name (org-files-db-cache--job-view-name job))
                  (equal cache-key (org-files-db-cache--job-cache-key job))))
           org-files-db-cache--queue)))
    (if (and (not force) (or equivalent-current equivalent-queued))
        (or equivalent-current equivalent-queued)
      (when (and current (equal name (org-files-db-cache--job-view-name current)))
        (org-files-db-cache--cancel-current-job
         'superseded (plist-get state :generation)))
      (org-files-db-cache--remove-queued-view-jobs
       name 'superseded (plist-get state :generation))
      (let ((job
             (make-org-files-db-cache--job
              :id (cl-incf org-files-db-cache--job-counter)
              :view-name name
              :view (copy-tree view)
              :view-token view-token
              :cache-key cache-key
              :config-file config-file
              :config-key config-key
              :database-id (plist-get state :database-id)
              :generation (plist-get state :generation)
              :reason reason
              :event-info (copy-tree event-info)
              :requested-at (float-time)
              :status 'queued)))
        (puthash name (org-files-db-cache--job-id job)
                 org-files-db-cache--expected-jobs)
        (remhash name org-files-db-cache--failures)
        (setq org-files-db-cache--queue
              (nconc org-files-db-cache--queue (list job)))
        (org-files-db-cache--debug
         "queued view=%s generation=%s reason=%s queue=%d"
         name (org-files-db-cache--job-generation job) reason
         (length org-files-db-cache--queue))
        (unless org-files-db-cache--defer-worker-start
          (org-files-db-cache--start-next-worker))
        job))))

(defun org-files-db-cache--rebuild-config-views (config-file state reason event-info)
  "Queue CONFIG-FILE pre-cached views for STATE, REASON, and EVENT-INFO."
  (let ((org-files-db-cache--defer-worker-start t))
    (dolist (view (org-files-db-cache--views-for-config config-file))
      (org-files-db-cache--request-rebuild
       view config-file state reason event-info)))
  (org-files-db-cache--start-next-worker))

(defun org-files-db-cache--apply-state-change
    (config-file old-state new-state reason event-info)
  "Apply CONFIG-FILE transition from OLD-STATE to NEW-STATE.
REASON identifies why EVENT-INFO caused the transition."
  (let ((config-key (org-files-db-cache--config-key config-file)))
    (puthash config-key new-state org-files-db-cache--states)
    (org-files-db-cache--ensure-watcher config-file new-state)
    (unless (org-files-db-cache--same-state-p old-state new-state)
      (when (or (null old-state)
                (not (equal (plist-get old-state :database-id)
                            (plist-get new-state :database-id))))
        (org-files-db-cache--remove-config-entries config-key))
      (org-files-db-cache--supersede-config-jobs config-key new-state)
      (org-files-db-cache--rebuild-config-views
       config-file new-state reason event-info))))

(defun org-files-db-cache--debounced-status-check (config-key)
  "Run one authoritative status check after CONFIG-KEY file activity."
  (when-let* ((watch (and org-files-db-cache-mode
                          (gethash config-key org-files-db-cache--watchers))))
    (setf (org-files-db-cache--watch-debounce-timer watch) nil)
    (let* ((config-file (org-files-db-cache--watch-config-file watch))
           (old-state (gethash config-key org-files-db-cache--states))
           (status-check-at (float-time)))
      (condition-case err
          (progn
            ;; A `stopped' event invalidates the descriptor.  Reinstall from the
            ;; last authoritative state before querying status so a transient CLI
            ;; failure cannot leave cache mode permanently unwatched.
            (when (and old-state
                       (null (org-files-db-cache--watch-descriptor watch)))
              (org-files-db-cache--ensure-watcher config-file old-state))
            (let* ((new-state (org-files-db-cache--read-index-state config-file))
                   (info (org-files-db-cache--event-info
                          watch old-state new-state status-check-at)))
              (org-files-db-cache--record-generation-check
               watch old-state new-state info)
              (org-files-db-cache--apply-state-change
               config-file old-state new-state 'file-notify info)))
        (error
         (setf (org-files-db-cache--watch-pending-event-count watch) 0
               (org-files-db-cache--watch-first-event-at watch) nil)
         (org-files-db-cache--debug
          "status check failed config=%s error=%s"
          config-file (error-message-string err)))))))

(defun org-files-db-cache--bind-worker-options (options function)
  "Call FUNCTION with presentation OPTIONS dynamically bound."
  (let ((org-files-db-heading-columns
         (plist-get options :heading-columns))
        (org-files-db-file-columns
         (plist-get options :file-columns))
        (org-files-db-link-columns
         (plist-get options :link-columns))
        (org-files-db-search-columns
         (plist-get options :search-columns))
        (org-files-db-heading-sort
         (plist-get options :heading-sort))
        (org-files-db-file-sort
         (plist-get options :file-sort))
        (org-files-db-link-sort
         (plist-get options :link-sort))
        (org-files-db-search-sort
         (plist-get options :search-sort))
        (org-files-db-outline-path-separator
         (plist-get options :outline-separator))
        (org-files-db-outline-path-include-root
         (plist-get options :outline-include-root))
        (org-files-db-outline-path-include-match
         (plist-get options :outline-include-match))
        (org-files-db-truncate-position
         (plist-get options :truncate-position))
        (org-files-db-truncate-marker
         (plist-get options :truncate-marker)))
    (funcall function)))

(defun org-files-db-cache--worker-effective-sort (view context)
  "Return effective sort for VIEW in CONTEXT."
  (let ((properties (cdr view)))
    (org-files-db--effective-sort
     context
     (plist-get properties :sort)
     (not (null (plist-member properties :sort))))))

(defun org-files-db-cache--worker-fetch-query (view config-file)
  "Fetch query VIEW using CONFIG-FILE and return data plus timings."
  (let* ((properties (cdr view))
         (origin (format "View `%s'" (car view)))
         (query (plist-get properties :query)))
    (unless query
      (user-error "Query view `%s' has no :query" (car view)))
    (let* ((requested-columns (plist-get properties :columns))
           (inferred-target (org-files-db--query-target query))
           (initial-columns
            (or requested-columns
                (org-files-db--default-columns inferred-target nil)))
           (normalized-columns (org-files-db--normalize-columns initial-columns))
           (sort (org-files-db-cache--worker-effective-sort view inferred-target))
           (request-sort
            (org-files-db--normalize-sort
             sort normalized-columns inferred-target origin))
           (includes
            (delete-dups
             (append
              (org-files-db--column-includes normalized-columns)
              (org-files-db--sort-includes
               request-sort normalized-columns inferred-target origin))))
           (query-start (float-time))
           (raw
            (org-files-db--call-raw
             (cons "query"
                   (org-files-db--query-arguments
                    query config-file origin includes))))
           (query-end (float-time))
           (normalize-start (float-time))
           (response (org-files-db--parse-json raw))
           (results
            (org-files-db--results-with-config
             (org-files-db--normalize-results response) config-file))
           (target (or (org-files-db--response-target response) inferred-target))
           (final-columns
            (if (or requested-columns (eq target inferred-target))
                normalized-columns
              (org-files-db--normalize-columns
               (org-files-db--default-columns target results))))
           (final-sort
            (if (and (eq target inferred-target)
                     (eq final-columns normalized-columns))
                request-sort
              (org-files-db--normalize-sort sort final-columns target origin)))
           (normalize-end (float-time)))
      (list :results results
            :columns final-columns
            :sort final-sort
            :context target
            :query-search-start-at query-start
            :query-search-end-at query-end
            :query-search-duration (- query-end query-start)
            :json-normalization-duration (- normalize-end normalize-start)))))

(defun org-files-db-cache--worker-fetch-search (view config-file)
  "Fetch search VIEW using CONFIG-FILE and return data plus timings."
  (let* ((properties (cdr view))
         (origin (format "View `%s'" (car view)))
         (expression (plist-get properties :expression))
         (scope (or (plist-get properties :scope) 'all)))
    (unless (and (stringp expression) (not (string-empty-p expression)))
      (user-error "Search view `%s' has no valid :expression" (car view)))
    (let* ((query-start (float-time))
           (raw
            (org-files-db--call-raw
             (cons "search"
                   (org-files-db--search-arguments
                    expression scope config-file origin))))
           (query-end (float-time))
           (normalize-start (float-time))
           (response (org-files-db--parse-json raw))
           (results
            (org-files-db--results-with-config
             (org-files-db--normalize-results response) config-file))
           (columns
            (org-files-db--normalize-columns
             (or (plist-get properties :columns)
                 (org-files-db--default-columns 'search results))))
           (sort
            (org-files-db--normalize-sort
             (org-files-db-cache--worker-effective-sort view 'search)
             columns 'search origin))
           (normalize-end (float-time)))
      (list :results results
            :columns columns
            :sort sort
            :context 'search
            :query-search-start-at query-start
            :query-search-end-at query-end
            :query-search-duration (- query-end query-start)
            :json-normalization-duration (- normalize-end normalize-start)))))

(defun org-files-db-cache--write-dump (file value)
  "Write readable Lisp VALUE to FILE and return its byte size."
  (let ((coding-system-for-write 'utf-8-emacs-unix)
        (print-length nil)
        (print-level nil)
        (print-circle t))
    (with-temp-buffer
      (prin1 value (current-buffer))
      (write-region (point-min) (point-max) file nil 'silent)))
  (file-attribute-size (file-attributes file)))

(defun org-files-db-cache--read-dump (file)
  "Read one complete readable Lisp value from transport FILE safely."
  (unless (and (stringp file) (file-regular-p file) (file-readable-p file))
    (signal 'org-files-db-error
            (list "Cache worker dump is missing or unreadable")))
  (let ((coding-system-for-read 'utf-8-emacs-unix)
        value)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (setq value (read (current-buffer)))
      (skip-chars-forward " \t\r\n")
      (unless (eobp)
        (signal 'org-files-db-error
                (list "Cache worker dump contains trailing data"))))
    value))

(defun org-files-db-cache--worker-run (request)
  "Execute one complete cache worker REQUEST and return small metadata."
  (let* ((worker-started (float-time))
         (async-start-requested-at
          (or (plist-get request :async-start-requested-at) worker-started))
         (dump-file (plist-get request :dump-file))
         (job-id (plist-get request :job-id))
         (view (plist-get request :view))
         (view-name (car view))
         (database-id (plist-get request :database-id))
         (generation (plist-get request :generation))
         (config-file (plist-get request :config-file))
         (org-files-db-executable (plist-get request :executable))
         (options (plist-get request :presentation-options)))
    (condition-case err
        (org-files-db-cache--bind-worker-options
         options
         (lambda ()
           (let* ((status-start (float-time))
                  (state (org-files-db-cache--read-index-state config-file))
                  (status-duration (org-files-db-cache--elapsed status-start)))
             (unless (and (equal database-id (plist-get state :database-id))
                          (= generation (plist-get state :generation)))
               (signal 'org-files-db-error
                       (list "Database generation changed before worker query")))
             (let* ((fetched
                     (pcase (org-files-db-cache--view-command view)
                       ('query
                        (org-files-db-cache--worker-fetch-query view config-file))
                       ('search
                        (org-files-db-cache--worker-fetch-search view config-file))))
                    (results (plist-get fetched :results))
                    (columns (plist-get fetched :columns))
                    (sort (plist-get fetched :sort))
                    (context (plist-get fetched :context))
                    (presentation-start (float-time))
                    (presentation
                     (org-files-db--prepare-presentation
                      results columns sort context
                      (format "View `%s'" view-name)))
                    (presentation-duration
                     (org-files-db-cache--elapsed presentation-start))
                    (candidates
                     (org-files-db--presentation-candidates presentation))
                    (lookup (org-files-db--presentation-lookup presentation))
                    (presentation-timings
                     (org-files-db--presentation-timings presentation))
                    (dump
                     (list :format-version org-files-db-cache--format-version
                           :job-id job-id
                           :view view-name
                           :view-token (plist-get request :view-token)
                           :cache-key (plist-get request :cache-key)
                           :database-id database-id
                           :generation generation
                           :candidates candidates
                           :lookup lookup
                           :result-count (length results)
                           :candidate-count (length candidates)))
                    (dump-start (float-time))
                    (dump-size (org-files-db-cache--write-dump dump-file dump))
                    (dump-duration (org-files-db-cache--elapsed dump-start))
                    (finished-at (float-time)))
               (list
                :status 'success
                :job-id job-id
                :view view-name
                :database-id database-id
                :generation generation
                :dump-file dump-file
                :result-count (length results)
                :candidate-count (length candidates)
                :dump-size dump-size
                :timings
                (list
                 :child-entered-at worker-started
                 :async-start-requested-at async-start-requested-at
                 :worker-startup-delay
                 (- worker-started async-start-requested-at)
                 :preflight-status status-duration
                 :query-search-start-at
                 (plist-get fetched :query-search-start-at)
                 :query-search-end-at
                 (plist-get fetched :query-search-end-at)
                 :query-search (plist-get fetched :query-search-duration)
                 :json-normalization
                 (plist-get fetched :json-normalization-duration)
                 :presentation presentation-duration
                 :sorting (or (plist-get presentation-timings :sorting) 0.0)
                 :width-calculation
                 (or (plist-get presentation-timings :shared-width-calculation)
                     0.0)
                 :candidate-formatting
                 (or (plist-get presentation-timings :candidate-formatting)
                     0.0)
                 :presentation-timings presentation-timings
                 :dump-write dump-duration
                 :dump-ready-at (+ dump-start dump-duration)
                 :worker-finished-at finished-at
                 :worker-total (- finished-at worker-started)))))))
      (error
       (list :status 'failed
             :job-id job-id
             :view view-name
             :database-id database-id
             :generation generation
             :dump-file dump-file
             :error (error-message-string err)
             :timings
             (list :child-entered-at worker-started
                   :async-start-requested-at async-start-requested-at
                   :worker-startup-delay
                   (- worker-started async-start-requested-at)
                   :worker-finished-at (float-time)
                   :worker-total (org-files-db-cache--elapsed worker-started)))))))

(defun org-files-db-cache--worker-request (job)
  "Return serializable child request for JOB."
  (list :job-id (org-files-db-cache--job-id job)
        :view (org-files-db-cache--worker-view-data
               (org-files-db-cache--job-view job))
        :view-token (org-files-db-cache--job-view-token job)
        :cache-key (org-files-db-cache--job-cache-key job)
        :config-file (org-files-db-cache--job-config-file job)
        :database-id (org-files-db-cache--job-database-id job)
        :generation (org-files-db-cache--job-generation job)
        :async-start-requested-at (org-files-db-cache--job-started-at job)
        :dump-file (org-files-db-cache--job-dump-file job)
        :executable (org-files-db--resolve-executable)
        :presentation-options (org-files-db-cache--presentation-options)))

(defun org-files-db-cache--worker-result-control-valid-p (job result)
  "Return non-nil when small worker RESULT metadata belongs to JOB."
  (and (listp result)
       (equal (plist-get result :job-id)
              (org-files-db-cache--job-id job))
       (equal (plist-get result :view)
              (org-files-db-cache--job-view-name job))
       (equal (plist-get result :database-id)
              (org-files-db-cache--job-database-id job))
       (equal (plist-get result :generation)
              (org-files-db-cache--job-generation job))
       (equal (plist-get result :dump-file)
              (org-files-db-cache--job-dump-file job))))

(defun org-files-db-cache--worker-dump-valid-p (job dump result)
  "Return non-nil when DUMP and RESULT describe current JOB data."
  (let ((candidates (plist-get dump :candidates))
        (lookup (plist-get dump :lookup)))
    (and (= (or (plist-get dump :format-version) -1)
            org-files-db-cache--format-version)
         (equal (plist-get dump :job-id)
                (org-files-db-cache--job-id job))
         (equal (plist-get dump :view)
                (org-files-db-cache--job-view-name job))
         (equal (plist-get dump :view-token)
                (org-files-db-cache--job-view-token job))
         (equal (plist-get dump :cache-key)
                (org-files-db-cache--job-cache-key job))
         (equal (plist-get dump :database-id)
                (org-files-db-cache--job-database-id job))
         (= (or (plist-get dump :generation) -1)
            (org-files-db-cache--job-generation job))
         (listp candidates)
         (vectorp lookup)
         (= (length candidates) (length lookup))
         (= (length candidates) (or (plist-get dump :candidate-count) -1))
         (= (or (plist-get dump :candidate-count) -1)
            (or (plist-get result :candidate-count) -2))
         (= (or (plist-get dump :result-count) -1)
            (or (plist-get result :result-count) -2)))))

(defun org-files-db-cache--record-failure (job message &optional result)
  "Record cache JOB failure MESSAGE and optional worker RESULT."
  (setf (org-files-db-cache--job-status job) 'failed)
  (let ((record
         (list :type 'rebuild
               :status 'failed
               :job-id (org-files-db-cache--job-id job)
               :view (org-files-db-cache--job-view-name job)
               :reason (org-files-db-cache--job-reason job)
               :database-id (org-files-db-cache--job-database-id job)
               :generation (org-files-db-cache--job-generation job)
               :error message
               :worker-timings (and result (plist-get result :timings))
               :failed-at (float-time))))
    (puthash (org-files-db-cache--job-view-name job) record
             org-files-db-cache--failures)
    (push record org-files-db-cache--benchmarks)
    (org-files-db-cache--debug
     "worker failed view=%s generation=%s error=%s"
     (org-files-db-cache--job-view-name job)
     (org-files-db-cache--job-generation job) message)))

(defun org-files-db-cache--publish-job
    (job result state &optional callback-start gcs-before gc-time-before
         status-check-duration)
  "Read, validate, and atomically publish successful JOB RESULT at STATE.
CALLBACK-START, GCS-BEFORE, GC-TIME-BEFORE, and STATUS-CHECK-DURATION let the
caller include the authoritative publication status check in parent blocking
metrics."
  (let* ((blocked-start (or callback-start (float-time)))
         (gcs-before (or gcs-before
                         (if (boundp 'gcs-done) gcs-done 0)))
         (gc-time-before (or gc-time-before
                             (if (boundp 'gc-elapsed) gc-elapsed 0.0)))
         (dump-file (org-files-db-cache--job-dump-file job))
         (read-start (float-time))
         (dump (org-files-db-cache--read-dump dump-file))
         (dump-read (org-files-db-cache--elapsed read-start))
         (validation-start (float-time)))
    (unless (and (org-files-db-cache--job-expected-p job)
                 (org-files-db-cache--worker-dump-valid-p job dump result)
                 (equal (plist-get state :database-id)
                        (org-files-db-cache--job-database-id job))
                 (= (plist-get state :generation)
                    (org-files-db-cache--job-generation job)))
      (signal 'org-files-db-error
              (list "Cache worker result became obsolete before publication")))
    (let* ((validation-duration (org-files-db-cache--elapsed validation-start))
           (candidates (plist-get dump :candidates))
           (lookup (plist-get dump :lookup))
           (publication-start (float-time))
           (presentation
            (make-org-files-db--presentation
             :candidates candidates :lookup lookup))
           (entry
            (make-org-files-db-cache--entry
             :view-name (org-files-db-cache--job-view-name job)
             :view-token (org-files-db-cache--job-view-token job)
             :cache-key (org-files-db-cache--job-cache-key job)
             :config-file (org-files-db-cache--job-config-file job)
             :config-key (org-files-db-cache--job-config-key job)
             :database-id (org-files-db-cache--job-database-id job)
             :generation (org-files-db-cache--job-generation job)
             :candidates candidates
             :lookup lookup
             :presentation presentation
             :result-count (plist-get dump :result-count)
             :candidate-count (plist-get dump :candidate-count)
             :published-at (float-time))))
      (when candidates
        (puthash candidates lookup org-files-db--candidate-lookups))
      (puthash (org-files-db-cache--job-view-name job) entry
               org-files-db-cache--entries)
      (let* ((publication-duration
              (org-files-db-cache--elapsed publication-start))
             (delete-duration (org-files-db-cache--cleanup-job-dump job))
             (blocked-duration (org-files-db-cache--elapsed blocked-start))
             (published-at (org-files-db-cache--entry-published-at entry))
             (record
              (append
               (list :type 'rebuild
                     :status 'success
                     :job-id (org-files-db-cache--job-id job)
                     :view (org-files-db-cache--job-view-name job)
                     :reason (org-files-db-cache--job-reason job)
                     :database-id (org-files-db-cache--job-database-id job)
                     :generation (org-files-db-cache--job-generation job)
                     :requested-at (org-files-db-cache--job-requested-at job)
                     :worker-started-at (org-files-db-cache--job-started-at job)
                     :callback-at blocked-start
                     :published-at published-at
                     :result-count (org-files-db-cache--entry-result-count entry)
                     :candidate-count
                     (org-files-db-cache--entry-candidate-count entry)
                     :dump-size (plist-get result :dump-size)
                     :worker-timings (plist-get result :timings)
                     :parent-timings
                     (list :status-check (or status-check-duration 0.0)
                           :dump-read dump-read
                           :validation validation-duration
                           :publication publication-duration
                           :dump-delete delete-duration
                           :main-blocked blocked-duration
                           :garbage-collections
                           (- (if (boundp 'gcs-done) gcs-done 0) gcs-before)
                           :garbage-collection-time
                           (- (if (boundp 'gc-elapsed) gc-elapsed 0.0)
                              gc-time-before)))
               (when-let* ((event-info (org-files-db-cache--job-event-info job)))
                 (list :file-notify event-info)))))
        (push record org-files-db-cache--benchmarks)
        (remhash (org-files-db-cache--job-view-name job)
                 org-files-db-cache--failures)
        (org-files-db-cache--debug
         "published view=%s generation=%s candidates=%s blocked=%.3fs"
         (org-files-db-cache--job-view-name job)
         (org-files-db-cache--job-generation job)
         (org-files-db-cache--entry-candidate-count entry)
         blocked-duration)
        entry))))

(defun org-files-db-cache--worker-finished (job result)
  "Handle final async RESULT for JOB."
  (when (eq job org-files-db-cache--current-job)
    (let ((callback-start (float-time))
          (gcs-before (if (boundp 'gcs-done) gcs-done 0))
          (gc-time-before (if (boundp 'gc-elapsed) gc-elapsed 0.0))
          (org-files-db-cache--defer-worker-start t))
      (setq org-files-db-cache--current-job nil
            org-files-db-cache--current-worker nil)
      (setf (org-files-db-cache--job-process job) nil)
      (unwind-protect
          (condition-case err
              (cond
               ((not (org-files-db-cache--job-expected-p job))
                (setf (org-files-db-cache--job-status job) 'superseded))
               ((not (org-files-db-cache--worker-result-control-valid-p
                      job result))
                (org-files-db-cache--record-failure
                 job "Cache worker returned malformed control metadata" result))
               ((eq (plist-get result :status) 'failed)
                (org-files-db-cache--record-failure
                 job (or (plist-get result :error) "Cache worker failed") result))
               ((not (eq (plist-get result :status) 'success))
                (org-files-db-cache--record-failure
                 job "Cache worker returned an unknown status" result))
               (t
                (let* ((config-file (org-files-db-cache--job-config-file job))
                       (config-key (org-files-db-cache--job-config-key job))
                       (old-state
                        (gethash config-key org-files-db-cache--states))
                       (status-start (float-time))
                       (state
                        (org-files-db-cache--read-index-state config-file))
                       (status-duration
                        (org-files-db-cache--elapsed status-start)))
                  (if (not
                       (and
                        (equal (plist-get state :database-id)
                               (org-files-db-cache--job-database-id job))
                        (= (plist-get state :generation)
                           (org-files-db-cache--job-generation job))))
                      (progn
                        (puthash config-key state org-files-db-cache--states)
                        (org-files-db-cache--ensure-watcher config-file state)
                        (setf (org-files-db-cache--job-status job) 'superseded)
                        (org-files-db-cache--record-cancellation
                         job 'superseded (plist-get state :generation))
                        (org-files-db-cache--rebuild-config-views
                         config-file state 'superseded nil))
                    (puthash config-key state org-files-db-cache--states)
                    (org-files-db-cache--ensure-watcher config-file state)
                    (org-files-db-cache--publish-job
                     job result state callback-start gcs-before gc-time-before
                     status-duration)
                    (setf (org-files-db-cache--job-status job) 'success)
                    (when (and old-state
                               (not (org-files-db-cache--same-state-p
                                     old-state state)))
                      (org-files-db-cache--debug
                       "publication observed generation transition for %s"
                       config-file))))))
            (error
             (unless (memq (org-files-db-cache--job-status job)
                           '(cancelled superseded))
               (org-files-db-cache--record-failure
                job (error-message-string err) result))))
        (org-files-db-cache--cleanup-job-dump job)
        (when (equal (gethash (org-files-db-cache--job-view-name job)
                              org-files-db-cache--expected-jobs)
                     (org-files-db-cache--job-id job))
          (unless (eq (org-files-db-cache--job-status job) 'superseded)
            (remhash (org-files-db-cache--job-view-name job)
                     org-files-db-cache--expected-jobs)))))
    (org-files-db-cache--start-next-worker)))

(defun org-files-db-cache--async-process-ended (job process event sentinel-error)
  "Handle terminal async PROCESS for JOB after EVENT and SENTINEL-ERROR."
  (when (and (eq job org-files-db-cache--current-job)
             (memq (process-status process) '(exit signal)))
    (let ((cancelled-status (org-files-db-cache--job-status job)))
      (unless (memq cancelled-status '(cancelled superseded success))
        (setq org-files-db-cache--current-job nil
              org-files-db-cache--current-worker nil)
        (org-files-db-cache--record-failure
         job
         (if sentinel-error
             (format "async.el callback failed: %s" sentinel-error)
           (format "Child Emacs ended unexpectedly (%s, status %s)"
                   (string-trim (or event ""))
                   (process-exit-status process))))
        (org-files-db-cache--cleanup-job-dump job)
        (when (equal (gethash (org-files-db-cache--job-view-name job)
                              org-files-db-cache--expected-jobs)
                     (org-files-db-cache--job-id job))
          (remhash (org-files-db-cache--job-view-name job)
                   org-files-db-cache--expected-jobs))
        (org-files-db-cache--cleanup-process-buffers process)
        (org-files-db-cache--start-next-worker)))))

(defun org-files-db-cache--wrap-async-sentinel (job process)
  "Wrap async.el PROCESS sentinel so JOB failures cannot strand the queue."
  (let ((original (process-sentinel process)))
    (set-process-sentinel
     process
     (lambda (proc event)
       (let (sentinel-error)
         (condition-case err
             (when original
               (funcall original proc event))
           (error
            (setq sentinel-error (error-message-string err))))
         (org-files-db-cache--async-process-ended
          job proc event sentinel-error)
         (when (memq (process-status proc) '(exit signal))
           (org-files-db-cache--cleanup-process-buffers proc)))))))

(defun org-files-db-cache--start-next-worker ()
  "Start the next current queued cache worker when possible."
  (when (and org-files-db-cache-mode
             (not org-files-db-cache--defer-worker-start)
             (null org-files-db-cache--current-job))
    (let ((job (pop org-files-db-cache--queue)))
      (while (and job (not (org-files-db-cache--job-expected-p job)))
        (org-files-db-cache--cancel-job job 'superseded nil)
        (setq job (pop org-files-db-cache--queue)))
      (when job
        (let* ((state (gethash (org-files-db-cache--job-config-key job)
                               org-files-db-cache--states))
               (view (org-files-db-cache--current-view
                      (org-files-db-cache--job-view-name job))))
          (if (not (and state view
                        (org-files-db-cache--view-pre-cache-p view)
                        (equal (org-files-db-cache--job-view-token job)
                               (org-files-db-cache--view-token
                                view (org-files-db-cache--job-config-file job)))
                        (equal (plist-get state :database-id)
                               (org-files-db-cache--job-database-id job))
                        (= (plist-get state :generation)
                           (org-files-db-cache--job-generation job))))
              (progn
                (org-files-db-cache--cancel-job job 'superseded nil)
                (when (org-files-db-cache--job-expected-p job)
                  (remhash (org-files-db-cache--job-view-name job)
                           org-files-db-cache--expected-jobs))
                (org-files-db-cache--start-next-worker))
            (let ((dump-file (make-temp-file "org-files-db-cache-" nil ".el")))
              (setf (org-files-db-cache--job-dump-file job) dump-file
                    (org-files-db-cache--job-started-at job) (float-time)
                    (org-files-db-cache--job-status job) 'running)
              (puthash dump-file (org-files-db-cache--job-id job)
                       org-files-db-cache--owned-dumps)
              (setq org-files-db-cache--current-job job)
              (condition-case err
                  (let* ((request (org-files-db-cache--worker-request job))
                         (cached-async-library org-files-db-cache--async-library)
                         (original-locate-library
                          (symbol-function 'locate-library))
                         (async-prompt-for-password nil)
                         (async-process-noquery-on-exit t)
                         (process
                          (cl-letf
                              (((symbol-function 'locate-library)
                                (lambda (library &rest arguments)
                                  (if (equal library "async")
                                      cached-async-library
                                    (apply original-locate-library
                                           library arguments)))))
                            (async-start
                             `(lambda ()
                                (add-to-list
                                 'load-path
                                 ,org-files-db-cache--library-directory)
                                (require 'org-files-db-cache)
                                (org-files-db-cache--worker-run ',request))
                             (lambda (result)
                               (org-files-db-cache--worker-finished
                                job result))))))
                    (setf (org-files-db-cache--job-process job) process)
                    (setq org-files-db-cache--current-worker process)
                    (org-files-db-cache--wrap-async-sentinel job process)
                    (org-files-db-cache--debug
                     "started view=%s generation=%s job=%s"
                     (org-files-db-cache--job-view-name job)
                     (org-files-db-cache--job-generation job)
                     (org-files-db-cache--job-id job)))
                (error
                 (setq org-files-db-cache--current-job nil
                       org-files-db-cache--current-worker nil)
                 (org-files-db-cache--record-failure
                  job (error-message-string err))
                 (org-files-db-cache--cleanup-job-dump job)
                 (when (org-files-db-cache--job-expected-p job)
                   (remhash (org-files-db-cache--job-view-name job)
                            org-files-db-cache--expected-jobs))
                 (org-files-db-cache--start-next-worker))))))))))

(defun org-files-db-cache--prioritize-view (name)
  "Move queued rebuild for view NAME to the front without cancelling work."
  (let ((job (seq-find
              (lambda (item)
                (equal name (org-files-db-cache--job-view-name item)))
              org-files-db-cache--queue)))
    (when job
      (setq org-files-db-cache--queue
            (cons job (delq job org-files-db-cache--queue))))))

(defun org-files-db-cache--refresh-active-p (name)
  "Return non-nil when NAME has a current or queued expected rebuild."
  (or (and org-files-db-cache--current-job
           (equal name
                  (org-files-db-cache--job-view-name
                   org-files-db-cache--current-job))
           (org-files-db-cache--job-expected-p org-files-db-cache--current-job))
      (seq-some
       (lambda (job)
         (and (equal name (org-files-db-cache--job-view-name job))
              (org-files-db-cache--job-expected-p job)))
       org-files-db-cache--queue)))

(defun org-files-db-cache--ensure-state (config-file &optional refresh)
  "Return known state for CONFIG-FILE, reading status when REFRESH or absent."
  (let* ((config-key (org-files-db-cache--config-key config-file))
         (known (gethash config-key org-files-db-cache--states)))
    (if (and known (not refresh))
        known
      (let ((state (org-files-db-cache--read-index-state config-file)))
        (puthash config-key state org-files-db-cache--states)
        (org-files-db-cache--ensure-watcher config-file state)
        state))))

(defun org-files-db-cache--wait-for-view (view config-file &optional force)
  "Wait for newest async rebuild of VIEW using CONFIG-FILE.
When FORCE is non-nil, an already warm entry does not satisfy the initial
request; the replacement worker must publish first."
  (let* ((name (car view))
         (started-at (float-time))
         (deadline (+ started-at
                      (max 0.1 org-files-db-cache-wait-timeout)))
         (initial-expected (gethash name org-files-db-cache--expected-jobs))
         (last-expected initial-expected)
         (replacement-count 0)
         entry done)
    (condition-case err
        (progn
          (org-files-db-cache--prioritize-view name)
          (while (not done)
            (let* ((state
                    (gethash (org-files-db-cache--config-key config-file)
                             org-files-db-cache--states))
                   (current-view (or (org-files-db-cache--current-view name)
                                     view))
                   (candidate (gethash name org-files-db-cache--entries))
                   (expected (gethash name org-files-db-cache--expected-jobs)))
              (when (and expected last-expected
                         (not (equal expected last-expected)))
                (cl-incf replacement-count))
              (when expected
                (setq last-expected expected))
              (cond
               ((and candidate state
                     (org-files-db-cache--entry-valid-p
                      candidate current-view config-file state)
                     (or (not force)
                         (and initial-expected
                              (not (equal expected initial-expected))
                              (not (org-files-db-cache--refresh-active-p name)))
                         (and (null expected)
                              (not (org-files-db-cache--refresh-active-p name)))))
                (setq entry candidate done t))
               ((>= (float-time) deadline)
                (when (org-files-db-cache--refresh-active-p name)
                  (when (and org-files-db-cache--current-job
                             (equal name
                                    (org-files-db-cache--job-view-name
                                     org-files-db-cache--current-job)))
                    (org-files-db-cache--cancel-current-job 'cancelled nil))
                  (org-files-db-cache--remove-queued-view-jobs
                   name 'cancelled nil)
                  (remhash name org-files-db-cache--expected-jobs))
                (user-error "Timed out waiting for cache rebuild of view `%s'"
                            name))
               ((not (org-files-db-cache--refresh-active-p name))
                (if-let* ((failure (gethash name org-files-db-cache--failures)))
                    (user-error "Cache rebuild for view `%s' failed: %s"
                                name (plist-get failure :error))
                  (user-error "No cache rebuild is active for view `%s'" name)))
               (t
                (accept-process-output nil 0.05)))))
          (let ((finished-at (float-time)))
            (push
             (list :type 'interactive-wait
                   :status 'success
                   :view name
                   :database-id (org-files-db-cache--entry-database-id entry)
                   :generation (org-files-db-cache--entry-generation entry)
                   :initial-job-id initial-expected
                   :final-job-id last-expected
                   :replacement-count replacement-count
                   :started-at started-at
                   :finished-at finished-at
                   :duration (- finished-at started-at))
             org-files-db-cache--benchmarks))
          entry)
      (error
       (let ((finished-at (float-time)))
         (push
          (list :type 'interactive-wait
                :status 'failed
                :view name
                :initial-job-id initial-expected
                :final-job-id last-expected
                :replacement-count replacement-count
                :started-at started-at
                :finished-at finished-at
                :duration (- finished-at started-at)
                :error (error-message-string err))
          org-files-db-cache--benchmarks))
       (signal (car err) (cdr err))))))

(defun org-files-db-cache--present-entry (entry action prompt)
  "Present warm cache ENTRY using ACTION and PROMPT."
  (org-files-db--present-presentation
   (org-files-db-cache--entry-presentation entry) action prompt))

(defun org-files-db-cache--record-warm-lookup (entry started-at)
  "Record a completed in-memory warm lookup of ENTRY since STARTED-AT."
  (let ((finished-at (float-time)))
    (push
     (list :type 'warm-lookup
           :status 'success
           :view (org-files-db-cache--entry-view-name entry)
           :database-id (org-files-db-cache--entry-database-id entry)
           :generation (org-files-db-cache--entry-generation entry)
           :candidate-count (org-files-db-cache--entry-candidate-count entry)
           :started-at started-at
           :finished-at finished-at
           :duration (- finished-at started-at))
     org-files-db-cache--benchmarks)))

(defun org-files-db-cache-present-view
    (view config-file action prompt &optional force-refresh)
  "Present predefined VIEW through its in-memory async cache.
CONFIG-FILE is already resolved.  ACTION and PROMPT control completion.
FORCE-REFRESH requests a new asynchronous full rebuild and waits for it."
  (unless org-files-db-cache-mode
    (user-error "org-files-db-cache-mode is not enabled"))
  (unless (org-files-db-cache--view-pre-cache-p view)
    (user-error "View `%s' does not enable :pre-cache" (car view)))
  (let* ((lookup-started (float-time))
         (name (car view))
         (state (org-files-db-cache--ensure-state config-file force-refresh))
         (entry (gethash name org-files-db-cache--entries)))
    (cond
     ((and (not force-refresh)
           (org-files-db-cache--entry-valid-p entry view config-file state))
      (org-files-db-cache--debug
       "warm lookup view=%s generation=%s" name (plist-get state :generation))
      (org-files-db-cache--record-warm-lookup entry lookup-started)
      (org-files-db-cache--present-entry entry action prompt))
     (t
      (org-files-db-cache--request-rebuild
       view config-file state
       (if force-refresh 'force-refresh 'interactive)
       nil force-refresh)
      (org-files-db-cache--present-entry
       (org-files-db-cache--wait-for-view view config-file force-refresh)
       action prompt)))))

(defun org-files-db-cache-refresh-view (view config-file &optional synchronous)
  "Refresh pre-cached VIEW using CONFIG-FILE asynchronously.
When SYNCHRONOUS is non-nil, wait for the asynchronous replacement to publish."
  (unless org-files-db-cache-mode
    (user-error "org-files-db-cache-mode is not enabled"))
  (unless (org-files-db-cache--view-pre-cache-p view)
    (user-error "View `%s' does not enable :pre-cache" (car view)))
  (let ((state (org-files-db-cache--ensure-state config-file t)))
    (org-files-db-cache--request-rebuild
     view config-file state 'manual nil t)
    (if synchronous
        (progn
          (org-files-db-cache--wait-for-view view config-file t)
          (message "Refreshed cache for view `%s'" (car view)))
      (message "Refreshing cache for view `%s'" (car view)))))

(defun org-files-db-cache-refresh-all (&optional synchronous)
  "Refresh every configured pre-cached view.
When SYNCHRONOUS is non-nil, wait for each asynchronous replacement."
  (unless org-files-db-cache-mode
    (user-error "org-files-db-cache-mode is not enabled"))
  (let ((views (org-files-db-cache--configured-pre-cache-views))
        requested)
    (let ((org-files-db-cache--defer-worker-start t))
      (dolist (view views)
        (let* ((config-file (org-files-db-cache--view-config-file view))
               (state (org-files-db-cache--ensure-state config-file t)))
          (org-files-db-cache--request-rebuild
           view config-file state 'manual nil t)
          (push (cons view config-file) requested))))
    (org-files-db-cache--start-next-worker)
    (when synchronous
      (dolist (item (nreverse requested))
        (org-files-db-cache--wait-for-view (car item) (cdr item) t)))
    (message "Refreshing %d org-files-db view cache%s"
             (length views) (if (= (length views) 1) "" "s"))))

(defun org-files-db-cache-clear-view (name)
  "Clear published cache and pending work for predefined view NAME."
  (when (and org-files-db-cache--current-job
             (equal name
                    (org-files-db-cache--job-view-name
                     org-files-db-cache--current-job)))
    (org-files-db-cache--cancel-current-job 'cancelled nil))
  (org-files-db-cache--remove-queued-view-jobs name 'cancelled nil)
  (remhash name org-files-db-cache--expected-jobs)
  (remhash name org-files-db-cache--failures)
  (remhash name org-files-db-cache--entries)
  (org-files-db-cache--start-next-worker)
  (message "Cleared cache for view `%s'" name))

(defun org-files-db-cache-clear-all ()
  "Clear every published cache and queued or running rebuild."
  (org-files-db-cache--cancel-current-job 'cancelled nil)
  (dolist (job org-files-db-cache--queue)
    (org-files-db-cache--cancel-job job 'cancelled nil))
  (setq org-files-db-cache--queue nil)
  (clrhash org-files-db-cache--expected-jobs)
  (clrhash org-files-db-cache--failures)
  (clrhash org-files-db-cache--entries)
  (when org-files-db-cache-mode
    (org-files-db-cache--start-next-worker))
  (message "Cleared all org-files-db view caches"))

(defun org-files-db-cache--cleanup-all-dumps ()
  "Delete every temporary dump currently owned by the cache."
  (let (files)
    (maphash (lambda (file _job-id) (push file files))
             org-files-db-cache--owned-dumps)
    (dolist (file files)
      (org-files-db-cache--delete-dump file))))

(defun org-files-db-cache--enable-mode ()
  "Resolve status, install watches, and queue initial pre-cache rebuilds."
  (setq org-files-db-cache--async-library (locate-library "async"))
  (unless org-files-db-cache--async-library
    (user-error "Cannot locate async.el for org-files-db-cache-mode"))
  (let ((views (org-files-db-cache--configured-pre-cache-views))
        (states (make-hash-table :test #'equal)))
    (condition-case err
        (progn
          (dolist (view views)
            (let* ((config-file (org-files-db-cache--view-config-file view))
                   (config-key (org-files-db-cache--config-key config-file)))
              (unless (gethash config-key states)
                (let ((state (org-files-db-cache--read-index-state config-file)))
                  (puthash config-key state states)
                  (puthash config-key state org-files-db-cache--states)
                  (org-files-db-cache--ensure-watcher config-file state)))))
          (let ((org-files-db-cache--defer-worker-start t))
            (dolist (view views)
              (let* ((config-file (org-files-db-cache--view-config-file view))
                     (state (gethash (org-files-db-cache--config-key config-file)
                                     org-files-db-cache--states)))
                (org-files-db-cache--request-rebuild
                 view config-file state 'initial nil))))
          (org-files-db-cache--start-next-worker)
          (org-files-db-cache--debug
           "cache mode enabled views=%d" (length views)))
      (error
       (setq org-files-db-cache-mode nil)
       (org-files-db-cache--disable-mode)
       (signal (car err) (cdr err))))))

(defun org-files-db-cache--disable-mode ()
  "Stop all cache work, remove watches, and clear published state."
  (org-files-db-cache--cancel-current-job 'cancelled nil)
  (dolist (job org-files-db-cache--queue)
    (org-files-db-cache--cancel-job job 'cancelled nil))
  (setq org-files-db-cache--queue nil
        org-files-db-cache--current-job nil
        org-files-db-cache--current-worker nil
        org-files-db-cache--async-library nil)
  (let (keys)
    (maphash (lambda (key _watch) (push key keys))
             org-files-db-cache--watchers)
    (dolist (key keys)
      (org-files-db-cache--remove-watch key)))
  (org-files-db-cache--cleanup-all-dumps)
  (clrhash org-files-db-cache--entries)
  (clrhash org-files-db-cache--states)
  (clrhash org-files-db-cache--expected-jobs)
  (clrhash org-files-db-cache--failures)
  (org-files-db-cache--debug "cache mode disabled"))

(defun org-files-db-cache--format-seconds (value)
  "Return VALUE seconds in a concise human-readable form."
  (format "%.3f s" (or value 0.0)))

(defun org-files-db-cache--format-bytes (bytes)
  "Return BYTES in a concise human-readable form."
  (if (>= (or bytes 0) (* 1024 1024))
      (format "%.1f MB" (/ bytes 1048576.0))
    (format "%d bytes" (or bytes 0))))

(defun org-files-db-cache--insert-success-benchmark (record)
  "Insert human-readable successful rebuild benchmark RECORD."
  (let* ((worker (plist-get record :worker-timings))
         (parent (plist-get record :parent-timings))
         (event (plist-get record :file-notify))
         (requested (plist-get record :requested-at))
         (published (plist-get record :published-at)))
    (insert (format "View: %s\n" (plist-get record :view)))
    (insert (format "Reason: %s\n" (plist-get record :reason)))
    (insert (format "Generation: %s\n" (plist-get record :generation)))
    (insert (format "Results: %s\n" (plist-get record :result-count)))
    (insert (format "Candidates: %s\n\n" (plist-get record :candidate-count)))
    (insert (format "worker startup:       %s\n"
                    (org-files-db-cache--format-seconds
                     (plist-get worker :worker-startup-delay))))
    (insert (format "worker status check:  %s\n"
                    (org-files-db-cache--format-seconds
                     (plist-get worker :preflight-status))))
    (insert (format "query/search:         %s\n"
                    (org-files-db-cache--format-seconds
                     (plist-get worker :query-search))))
    (insert (format "JSON + normalize:     %s\n"
                    (org-files-db-cache--format-seconds
                     (plist-get worker :json-normalization))))
    (insert (format "presentation:         %s\n"
                    (org-files-db-cache--format-seconds
                     (plist-get worker :presentation))))
    (insert (format "sorting:              %s\n"
                    (org-files-db-cache--format-seconds
                     (plist-get worker :sorting))))
    (insert (format "width calculation:    %s\n"
                    (org-files-db-cache--format-seconds
                     (plist-get worker :width-calculation))))
    (insert (format "candidate formatting: %s\n"
                    (org-files-db-cache--format-seconds
                     (plist-get worker :candidate-formatting))))
    (insert (format "dump write:           %s\n"
                    (org-files-db-cache--format-seconds
                     (plist-get worker :dump-write))))
    (insert (format "worker total:         %s\n\n"
                    (org-files-db-cache--format-seconds
                     (plist-get worker :worker-total))))
    (insert (format "dump size:            %s\n"
                    (org-files-db-cache--format-bytes
                     (plist-get record :dump-size))))
    (insert (format "parent status check:  %s\n"
                    (org-files-db-cache--format-seconds
                     (plist-get parent :status-check))))
    (insert (format "parent dump read:     %s\n"
                    (org-files-db-cache--format-seconds
                     (plist-get parent :dump-read))))
    (insert (format "parent validation:    %s\n"
                    (org-files-db-cache--format-seconds
                     (plist-get parent :validation))))
    (insert (format "parent publish:       %s\n"
                    (org-files-db-cache--format-seconds
                     (plist-get parent :publication))))
    (insert (format "parent dump delete:   %s\n"
                    (org-files-db-cache--format-seconds
                     (plist-get parent :dump-delete))))
    (insert (format "main Emacs blocked:   %s\n"
                    (org-files-db-cache--format-seconds
                     (plist-get parent :main-blocked))))
    (insert (format "parent GC:            %d / %s\n"
                    (or (plist-get parent :garbage-collections) 0)
                    (org-files-db-cache--format-seconds
                     (plist-get parent :garbage-collection-time))))
    (when (and requested published)
      (insert (format "\ntotal rebuild:        %s\n"
                      (org-files-db-cache--format-seconds
                       (- published requested)))))
    (when event
      (insert "\nfile-notify refresh:\n")
      (insert (format "raw events:           %s\n"
                      (plist-get event :raw-event-count)))
      (insert (format "first raw event at:   %.6f\n"
                      (or (plist-get event :first-event-at) 0.0)))
      (insert (format "old generation:       %s\n"
                      (plist-get event :old-generation)))
      (insert (format "new generation:       %s\n"
                      (plist-get event :new-generation)))
      (insert (format "status checked at:     %.6f\n"
                      (or (plist-get event :status-check-at) 0.0)))
      (insert (format "rebuild requested at: %.6f\n" (or requested 0.0)))
      (insert (format "worker requested at:  %.6f\n"
                      (or (plist-get worker :async-start-requested-at) 0.0)))
      (insert (format "child entered at:     %.6f\n"
                      (or (plist-get worker :child-entered-at) 0.0)))
      (insert (format "cache published at:   %.6f\n" (or published 0.0)))
      (when (and (plist-get event :first-event-at)
                 (plist-get event :generation-detected-at))
        (insert (format "event -> generation:  %s\n"
                        (org-files-db-cache--format-seconds
                         (- (plist-get event :generation-detected-at)
                            (plist-get event :first-event-at))))))
      (when (and (plist-get event :generation-detected-at)
                 (plist-get worker :child-entered-at))
        (insert (format "generation -> child: %s\n"
                        (org-files-db-cache--format-seconds
                         (- (plist-get worker :child-entered-at)
                            (plist-get event :generation-detected-at))))))
      (when (and (plist-get worker :child-entered-at)
                 (plist-get worker :dump-ready-at))
        (insert (format "child -> dump ready:  %s\n"
                        (org-files-db-cache--format-seconds
                         (- (plist-get worker :dump-ready-at)
                            (plist-get worker :child-entered-at))))))
      (when (and (plist-get worker :dump-ready-at) published)
        (insert (format "dump -> publication:  %s\n"
                        (org-files-db-cache--format-seconds
                         (- published (plist-get worker :dump-ready-at))))))
      (when (and (plist-get event :first-event-at) published)
        (insert (format "event -> usable cache: %s\n"
                        (org-files-db-cache--format-seconds
                         (- published (plist-get event :first-event-at)))))))))

;;;###autoload
(defun org-files-db-cache-benchmark-report ()
  "Display the newest structured cache lifecycle benchmark record."
  (interactive)
  (let ((record (car org-files-db-cache--benchmarks))
        (buffer (get-buffer-create "*org-files-db cache benchmark*")))
    (unless record
      (user-error "No org-files-db cache benchmark data is available"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "org-files-db cache benchmark\n\n")
        (pcase (plist-get record :type)
          ('warm-lookup
           (insert (format "View: %s\n" (plist-get record :view)))
           (insert "Operation: warm lookup\n")
           (insert (format "Generation: %s\n"
                           (plist-get record :generation)))
           (insert (format "Candidates: %s\n"
                           (plist-get record :candidate-count)))
           (insert (format "Lookup: %s\n"
                           (org-files-db-cache--format-seconds
                            (plist-get record :duration)))))
          ('interactive-wait
           (insert (format "View: %s\n" (plist-get record :view)))
           (insert (format "Operation: interactive wait (%s)\n"
                           (plist-get record :status)))
           (when (plist-member record :generation)
             (insert (format "Generation: %s\n"
                             (plist-get record :generation))))
           (insert (format "Replacement workers followed: %d\n"
                           (or (plist-get record :replacement-count) 0)))
           (insert (format "Wait: %s\n"
                           (org-files-db-cache--format-seconds
                            (plist-get record :duration))))
           (when-let* ((message (plist-get record :error)))
             (insert (format "Error: %s\n" message))))
          ('generation-check
           (insert (format "Operation: file-notify generation check (%s)\n"
                           (plist-get record :status)))
           (insert (format "Raw events: %s\n"
                           (plist-get record :raw-event-count)))
           (insert (format "Old generation: %s\n"
                           (or (plist-get record :old-generation) "none")))
           (insert (format "New generation: %s\n"
                           (plist-get record :new-generation)))
           (when (and (plist-get record :first-event-at)
                      (plist-get record :generation-detected-at))
             (insert
              (format "Event -> generation: %s\n"
                      (org-files-db-cache--format-seconds
                       (- (plist-get record :generation-detected-at)
                          (plist-get record :first-event-at)))))))
          ('rebuild
           (pcase (plist-get record :status)
             ('success
              (org-files-db-cache--insert-success-benchmark record))
             ((or 'cancelled 'superseded)
              (insert (format "View: %s\n" (plist-get record :view)))
              (insert (format "Status: %s\n" (plist-get record :status)))
              (insert (format "Generation: %s\n"
                              (plist-get record :generation)))
              (insert (format "Replacement generation: %s\n"
                              (or (plist-get record :replacement-generation)
                                  "none")))
              (insert (format "Time before cancellation: %s\n"
                              (org-files-db-cache--format-seconds
                               (plist-get record :time-before-cancellation))))
              (insert (format "Dump produced: %s\n"
                              (if (plist-get record :dump-created-p)
                                  "yes"
                                "no")))
              (insert (format "Cleanup: %s\n"
                              (org-files-db-cache--format-seconds
                               (plist-get record :cleanup-duration)))))
             (_
              (insert (format "View: %s\nStatus: failed\nError: %s\n"
                              (plist-get record :view)
                              (plist-get record :error))))))
          (_
           (insert (format "Unsupported benchmark record: %S\n" record))))
        (insert (format "\nSuperseded jobs total: %d\n"
                        org-files-db-cache--superseded-count))
        (insert (format "Cancelled jobs total:  %d\n"
                        org-files-db-cache--cancelled-count))
        (special-mode)))
    (pop-to-buffer buffer)
    record))

;;;###autoload
(defun org-files-db-cache-file-notify-trace ()
  "Display every raw file-notify event recorded by the cache."
  (interactive)
  (let ((buffer (get-buffer-create "*org-files-db cache file events*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "org-files-db cache raw file-notify events\n\n")
        (dolist (event (reverse org-files-db-cache--file-events))
          (insert
           (format "%.6f  %S  %S  %S%s\n"
                   (plist-get event :timestamp)
                   (plist-get event :descriptor)
                   (plist-get event :action)
                   (plist-get event :path)
                   (if-let* ((second (plist-get event :second-path)))
                       (format "  -> %S" second)
                     ""))))
        (special-mode)))
    (pop-to-buffer buffer)))

;;;###autoload
(define-minor-mode org-files-db-cache-mode
  "Toggle asynchronous in-memory pre-caching for predefined views.
The global mode resolves authoritative orgfdb status, watches the canonical
SQLite directory through file-notify, and asynchronously pre-caches every view
marked with :pre-cache t.  File notifications are the only change wake-up
mechanism; no polling or periodic cache timer is installed."
  :global t
  :group 'org-files-db
  :lighter nil
  (if org-files-db-cache-mode
      (org-files-db-cache--enable-mode)
    (org-files-db-cache--disable-mode)))

(provide 'org-files-db-cache)

;;; org-files-db-cache.el ends here
