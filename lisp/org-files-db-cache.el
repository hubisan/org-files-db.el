;;; org-files-db-cache.el --- Generation-aware predefined-view cache -*- lexical-binding: t; -*-

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

;; Prepared in-memory caches for predefined query and search views.  Cache
;; freshness is governed by the database identity and committed generation
;; reported by orgfdb.  Structural query views can use affected-file deltas;
;; search and unsafe query views are rebuilt completely.

;;; Code:

(require 'async)
(require 'cl-lib)
(require 'filenotify nil t)
(require 'org-files-db-core)
(require 'seq)
(require 'subr-x)

(defvar org-files-db-views)
(defvar org-files-db-cache-mode nil)
(defvar org-files-db-cache--worker-process-p nil)

(declare-function file-notify-add-watch "filenotify" (file flags callback))
(declare-function file-notify-rm-watch "filenotify" (descriptor))

(defconst org-files-db-cache--library-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the loaded cache module.")

(defcustom org-files-db-views-pre-cache-max-entries 3
  "Maximum number of prepared predefined-view cache entries retained."
  :type 'natnum
  :group 'org-files-db)

(defcustom org-files-db-views-pre-cache-max-results 10000
  "Maximum number of rows retained automatically for one predefined view."
  :type 'natnum
  :group 'org-files-db)

(defcustom org-files-db-views-pre-cache-max-total-results 20000
  "Approximate maximum rows retained across all predefined-view caches."
  :type 'natnum
  :group 'org-files-db)

(defcustom org-files-db-views-pre-cache-poll-interval nil
  "Seconds between fallback index-state polling checks.
File notification is the primary wake-up mechanism.  Polling only checks
configured databases without an active file-notify watcher.  Set this to nil
to disable fallback polling entirely. Enable this, if for some reason file
notify is not working."
  :type '(choice
          (const :tag "Disabled" nil)
          (number :tag "Seconds"))
  :group 'org-files-db)

(defcustom org-files-db-views-pre-cache-file-notify-debounce 0.25
  "Seconds used to debounce SQLite-related file notifications."
  :type 'number
  :group 'org-files-db)

(defcustom org-files-db-views-pre-cache-wait-timeout 60.0
  "Maximum seconds to wait for an asynchronous view-cache refresh.
When a stale predefined view already has, or can start, a background refresh,
the view command waits for that replacement instead of cancelling it and
running the same expensive work synchronously.  Reaching this timeout is
treated as a worker failure and falls back to the synchronous fresh path."
  :type 'number
  :group 'org-files-db)

(defcustom org-files-db-cache-debug nil
  "When non-nil, report generation-aware view-cache diagnostics.
Messages include lifecycle, queue, worker, status, notification, publication,
and timing events.  The default nil keeps normal cache operation quiet."
  :type 'boolean
  :group 'org-files-db)

(defconst org-files-db-cache--format-version 4
  "Version of the prepared predefined-view cache representation.")

(defconst org-files-db-cache--failure-backoff-seconds 60.0
  "Minimum delay before retrying an unavailable background status check.")

(cl-defstruct org-files-db-cache--entry
  "One complete prepared predefined-view cache entry."
  key
  storage-key
  view-name
  view-token
  command
  config-file
  database-id
  generation
  created-at
  last-used
  view
  results
  columns
  presentation
  result-count
  estimated-memory
  complete-p
  stale-p
  source-dirty-p
  in-use-p
  refresh-token)

(cl-defstruct org-files-db-cache--job
  "One queued asynchronous cache replacement."
  view-name
  view-token
  cache-key
  command
  config-file
  database-id
  source-generation
  target-generation
  refresh-type
  upsert-files
  deleted-files
  refresh-token
  explicit-p
  started-at
  request-file
  result-file
  watchdog-timer)

(defvar org-files-db-cache--entries (make-hash-table :test #'equal)
  "Prepared cache entries keyed by process-local storage key.")

(defvar org-files-db-cache--view-keys (make-hash-table :test #'equal)
  "Map predefined view names to their currently published storage keys.")

(defvar org-files-db-cache--entry-sequence 0
  "Monotonic sequence used to disambiguate retained cache entries.")

(defvar org-files-db-cache--index-states (make-hash-table :test #'equal)
  "Last successfully observed index state per effective configuration.")

(defvar org-files-db-cache--skipped (make-hash-table :test #'equal)
  "Automatic cache skips keyed by view definition and index state.")

(defvar org-files-db-cache--failures (make-hash-table :test #'equal)
  "Background refresh failures keyed by view definition and index state.")

(defvar org-files-db-cache--status-failures (make-hash-table :test #'equal)
  "Recent status failures keyed by effective configuration.")

(defvar org-files-db-cache--refresh-tokens (make-hash-table :test #'equal)
  "Latest refresh token for every predefined view name.")

(defvar org-files-db-cache--queue nil
  "FIFO queue of pending `org-files-db-cache--job' objects.")

(defvar org-files-db-cache--current-worker nil
  "Current background cache worker process, or nil.")

(defvar org-files-db-cache--current-job nil
  "Job owned by `org-files-db-cache--current-worker'.")

(defvar org-files-db-cache--refresh-counter 0
  "Monotonic local refresh token counter.")

(defvar org-files-db-cache--poll-timer nil)
(defvar org-files-db-cache--watchers (make-hash-table :test #'equal))
(defvar org-files-db-cache--debounce-timers (make-hash-table :test #'equal))
(defvar org-files-db-cache--notification-times (make-hash-table :test #'equal))

(defun org-files-db-cache--debug (format-string &rest arguments)
  "Report FORMAT-STRING with ARGUMENTS when cache debugging is enabled."
  (when org-files-db-cache-debug
    (apply #'message
           (concat "org-files-db cache: " format-string)
           arguments)))

(defun org-files-db-cache--elapsed (started-at)
  "Return elapsed seconds since STARTED-AT, or nil."
  (and started-at (- (float-time) started-at)))

(defun org-files-db-cache--config-key (config-file)
  "Return a stable key for effective CONFIG-FILE."
  (or config-file :no-config))

(defun org-files-db-cache--required-value (object key context)
  "Return OBJECT KEY or signal a CONTEXT error when it is absent."
  (unless (org-files-db--has-key-p object key)
    (signal 'org-files-db-error
            (list (format "%s is missing required field %s" context key))))
  (org-files-db--get object key))

(defun org-files-db-cache--string-value (value field context &optional nil-ok)
  "Validate VALUE as a string FIELD in CONTEXT.
When NIL-OK is non-nil, nil is accepted."
  (unless (or (and nil-ok (null value))
              (and (stringp value) (not (string-empty-p value))))
    (signal 'org-files-db-error
            (list (format "%s has invalid %s: %S" context field value))))
  value)

(defun org-files-db-cache--integer-value (value field context)
  "Validate VALUE as a non-negative integer FIELD in CONTEXT."
  (unless (and (integerp value) (>= value 0))
    (signal 'org-files-db-error
            (list (format "%s has invalid %s: %S" context field value))))
  value)

(defun org-files-db-cache--path-list (value field context)
  "Return validated path list VALUE for FIELD in CONTEXT."
  (let ((paths (cond
                ((null value) nil)
                ((vectorp value) (append value nil))
                ((listp value) value)
                (t :invalid))))
    (when (or (eq paths :invalid)
              (not (seq-every-p
                    (lambda (path)
                      (and (stringp path) (not (string-empty-p path))))
                    paths)))
      (signal 'org-files-db-error
              (list (format "%s has invalid %s: %S" context field value))))
    (delete-dups (copy-sequence paths))))

(defun org-files-db-cache--normalize-index-state (response)
  "Return normalized index state from orgfdb status RESPONSE."
  (let* ((context "orgfdb status response")
         (schema
          (org-files-db-cache--integer-value
           (org-files-db-cache--required-value response 'schema_version context)
           "schema_version" context))
         (database-id
          (org-files-db-cache--string-value
           (org-files-db-cache--required-value response 'database_id context)
           "database_id" context))
         (generation
          (org-files-db-cache--integer-value
           (org-files-db-cache--required-value response 'generation context)
           "generation" context))
         (last-changed-at
          (org-files-db--get response 'last_changed_at))
         (database-path
          (or (org-files-db--get response 'database_path)
              (org-files-db--get response 'db_path)
              (org-files-db--get response 'database_file))))
    (when last-changed-at
      (org-files-db-cache--string-value
       last-changed-at "last_changed_at" context))
    (when database-path
      (org-files-db-cache--string-value
       database-path "database_path" context)
      (unless (file-name-absolute-p database-path)
        (signal 'org-files-db-error
                (list (format "%s has non-canonical database_path: %S"
                              context database-path))))
      (setq database-path (expand-file-name database-path)))
    (list :schema-version schema
          :database-id database-id
          :generation generation
          :last-changed-at last-changed-at
          :database-path database-path)))

(defun org-files-db-cache--status-arguments (config-file)
  "Return status arguments for effective CONFIG-FILE."
  (append (org-files-db--config-arguments config-file "View cache status")
          '("--format" "json")))

(defun org-files-db-cache--record-status-failure (config-file error-text)
  "Record status ERROR-TEXT for effective CONFIG-FILE."
  (puthash (org-files-db-cache--config-key config-file)
           (list :time (float-time) :error error-text)
           org-files-db-cache--status-failures))

(defun org-files-db-cache--status-backoff-p (config-file)
  "Return non-nil when CONFIG-FILE status retries are temporarily backed off."
  (when-let* ((failure
               (gethash (org-files-db-cache--config-key config-file)
                        org-files-db-cache--status-failures)))
    (< (- (float-time) (plist-get failure :time))
       org-files-db-cache--failure-backoff-seconds)))

(defun org-files-db-cache--read-index-state (config-file)
  "Read and normalize authoritative index state for effective CONFIG-FILE."
  (let* ((response
          (org-files-db--call
           "status"
           (org-files-db-cache--status-arguments config-file)))
         (state (org-files-db-cache--normalize-index-state response)))
    (puthash (org-files-db-cache--config-key config-file)
             state org-files-db-cache--index-states)
    (remhash (org-files-db-cache--config-key config-file)
             org-files-db-cache--status-failures)
    (when (and org-files-db-cache-mode
               (not org-files-db-cache--worker-process-p))
      (org-files-db-cache--ensure-watcher config-file state))
    (org-files-db-cache--debug
     "status db=%s gen=%s path=%s"
     (plist-get state :database-id)
     (plist-get state :generation)
     (or (plist-get state :database-path) "unavailable"))
    state))

(defun org-files-db-cache--normalize-cache-action (value context)
  "Return supported cache action symbol for VALUE in CONTEXT."
  (let ((action (cond
                 ((symbolp value) value)
                 ((stringp value) (intern value))
                 (t nil))))
    (unless (memq action '(unchanged patch rebuild))
      (signal 'org-files-db-error
              (list (format "%s has unsupported cache_action: %S"
                            context value))))
    action))

(defun org-files-db-cache--normalize-changes
    (response expected-database-id expected-generation)
  "Normalize changes RESPONSE for EXPECTED-DATABASE-ID and EXPECTED-GENERATION."
  (let* ((context "orgfdb changes response")
         (schema
          (org-files-db-cache--integer-value
           (org-files-db-cache--required-value response 'schema_version context)
           "schema_version" context))
         (database-id
          (org-files-db-cache--string-value
           (org-files-db-cache--required-value response 'database_id context)
           "database_id" context))
         (from
          (org-files-db-cache--integer-value
           (org-files-db-cache--required-value response 'from_generation context)
           "from_generation" context))
         (to
          (org-files-db-cache--integer-value
           (org-files-db-cache--required-value response 'to_generation context)
           "to_generation" context))
         (oldest
          (org-files-db-cache--integer-value
           (org-files-db-cache--required-value
            response 'oldest_available_generation context)
           "oldest_available_generation" context))
         (action
          (org-files-db-cache--normalize-cache-action
           (org-files-db-cache--required-value response 'cache_action context)
           context))
         (complete
          (org-files-db-cache--required-value response 'complete context))
         (reason (org-files-db--get response 'reason))
         (upsert
          (org-files-db-cache--path-list
           (org-files-db-cache--required-value response 'upsert_files context)
           "upsert_files" context))
         (deleted
          (org-files-db-cache--path-list
           (org-files-db-cache--required-value response 'deleted_files context)
           "deleted_files" context)))
    (unless (equal database-id expected-database-id)
      (signal 'org-files-db-error
              (list (format "Changes database ID mismatch: expected %s, got %s"
                            expected-database-id database-id))))
    (unless (= from expected-generation)
      (signal 'org-files-db-error
              (list (format "Changes start generation mismatch: expected %d, got %d"
                            expected-generation from))))
    (unless (<= from to)
      (signal 'org-files-db-error
              (list (format "Changes response moves backwards: %d to %d" from to))))
    (unless (or (eq complete t) (null complete))
      (signal 'org-files-db-error
              (list (format "%s has invalid complete value: %S"
                            context complete))))
    (when reason
      (org-files-db-cache--string-value reason "reason" context))
    (when (and (eq action 'patch)
               (or (not complete) (> oldest from)))
      (setq action 'rebuild
            reason (or reason "history-unavailable")))
    (list :schema-version schema
          :database-id database-id
          :from-generation from
          :to-generation to
          :oldest-available-generation oldest
          :cache-action action
          :complete complete
          :reason reason
          :upsert-files upsert
          :deleted-files deleted)))

(defun org-files-db-cache--read-changes
    (config-file cached-database-id cached-generation)
  "Read CONFIG-FILE changes for CACHED-DATABASE-ID since CACHED-GENERATION."
  (let ((response
         (org-files-db--call
          "changes"
          (append
           (org-files-db--config-arguments config-file "View cache changes")
           (list "--database-id" cached-database-id
                 "--since-generation" (number-to-string cached-generation)
                 "--format" "json")))))
    (org-files-db-cache--normalize-changes
     response cached-database-id cached-generation)))

(defun org-files-db-cache--view-command (view)
  "Return the command symbol configured by VIEW."
  (plist-get (cdr view) :command))

(defun org-files-db-cache--view-pre-cache-p (view)
  "Return non-nil when VIEW opts into pre-caching."
  (eq (plist-get (cdr view) :pre-cache) t))

(defun org-files-db-cache--view-columns (view)
  "Return effective plain column definitions for VIEW."
  (let* ((properties (cdr view))
         (command (org-files-db-cache--view-command view))
         (configured (plist-get properties :columns)))
    (or configured
        (pcase command
          ('query
           (org-files-db--default-columns
            (org-files-db--query-target (plist-get properties :query)) nil))
          ('search org-files-db-search-columns)
          (_ nil)))))

(defun org-files-db-cache--function-key (function)
  "Return a stable cache-key representation for FUNCTION."
  (cond
   ((null function) nil)
   ((symbolp function) function)
   (t (secure-hash 'sha256 (prin1-to-string function)))))

(defun org-files-db-cache--view-token (view config-file)
  "Return a deterministic definition token for VIEW and CONFIG-FILE."
  (let ((properties (copy-tree (cdr view))))
    (when (plist-member properties :action)
      (setq properties
            (plist-put properties :action
                       (org-files-db-cache--function-key
                        (plist-get properties :action)))))
    (secure-hash
     'sha256
     (prin1-to-string
      (list :cache-format org-files-db-cache--format-version
            :name (car view)
            :properties properties
            :config-file config-file
            :columns (org-files-db-cache--view-columns view)
            :outline-separator org-files-db-outline-path-separator
            :outline-root org-files-db-outline-path-include-root
            :outline-match org-files-db-outline-path-include-match
            :truncate-position org-files-db-truncate-position
            :truncate-marker org-files-db-truncate-marker
            :themes custom-enabled-themes
            :frame-background-mode
            (and (boundp 'frame-background-mode) frame-background-mode))))))

(defun org-files-db-cache--cache-key (view config-file state)
  "Return complete cache key for VIEW, CONFIG-FILE, and index STATE."
  (let* ((columns (org-files-db-cache--view-columns view))
         (normalized (org-files-db--normalize-columns columns))
         (includes (org-files-db--column-includes normalized)))
    (secure-hash
     'sha256
     (prin1-to-string
      (list :cache-format org-files-db-cache--format-version
            :view-token (org-files-db-cache--view-token view config-file)
            :database-id (plist-get state :database-id)
            :command (org-files-db-cache--view-command view)
            :query (plist-get (cdr view) :query)
            :expression (plist-get (cdr view) :expression)
            :scope (or (plist-get (cdr view) :scope) 'all)
            :columns columns
            :row-source (plist-get (cdr view) :row-source)
            :sort (plist-get (cdr view) :sort)
            :includes includes)))))

(defun org-files-db-cache--current-view (name)
  "Return a copy of currently configured view NAME, or nil."
  (when-let* ((view (and (boundp 'org-files-db-views)
                         (assoc name org-files-db-views #'string=))))
    (copy-tree view)))

(defun org-files-db-cache--entry-for-view (name)
  "Return the currently indexed cache entry for view NAME."
  (when-let* ((key (gethash name org-files-db-cache--view-keys)))
    (gethash key org-files-db-cache--entries)))

(defun org-files-db-cache--entry-table-key (entry)
  "Return the hash-table storage key used for cache ENTRY."
  (or (org-files-db-cache--entry-storage-key entry)
      (org-files-db-cache--entry-key entry)))

(defun org-files-db-cache--view-in-use-p (name)
  "Return non-nil when any retained entry for view NAME is being displayed."
  (let (in-use-p)
    (maphash
     (lambda (_storage-key entry)
       (when (and (equal (org-files-db-cache--entry-view-name entry) name)
                  (org-files-db-cache--entry-in-use-p entry))
         (setq in-use-p t)))
     org-files-db-cache--entries)
    in-use-p))

(defun org-files-db-cache--allocate-storage-key (key)
  "Return an unused storage key for logical cache KEY.
Prefer KEY itself unless it is occupied by an entry that must remain alive."
  (let ((occupant (gethash key org-files-db-cache--entries)))
    (cond
     ((null occupant) key)
     ((org-files-db-cache--entry-in-use-p occupant)
      (list key :retained (cl-incf org-files-db-cache--entry-sequence)))
     (t
      (org-files-db-cache--remove-entry occupant)
      key))))

(defun org-files-db-cache--remove-entry (entry)
  "Remove cache ENTRY and its view-name index."
  (when entry
    (let ((storage-key (org-files-db-cache--entry-table-key entry)))
      (remhash storage-key org-files-db-cache--entries)
      (when (equal
             (gethash (org-files-db-cache--entry-view-name entry)
                      org-files-db-cache--view-keys)
             storage-key)
        (remhash (org-files-db-cache--entry-view-name entry)
                 org-files-db-cache--view-keys)))))

(defun org-files-db-cache--remove-view-entry (name)
  "Remove the current cache entry for predefined view NAME."
  (org-files-db-cache--remove-entry
   (org-files-db-cache--entry-for-view name)))

(defun org-files-db-cache--touch-entry (entry)
  "Record recent use of cache ENTRY and return it."
  (setf (org-files-db-cache--entry-last-used entry) (float-time))
  entry)

(defun org-files-db-cache--entry-at-state-p (entry state key)
  "Return non-nil when ENTRY is complete for committed STATE and KEY."
  (and entry
       (equal (org-files-db-cache--entry-key entry) key)
       (equal (org-files-db-cache--entry-database-id entry)
              (plist-get state :database-id))
       (= (org-files-db-cache--entry-generation entry)
          (plist-get state :generation))
       (org-files-db-cache--entry-complete-p entry)))

(defun org-files-db-cache--entry-current-p (entry state key)
  "Return non-nil when ENTRY is current for STATE and complete KEY."
  (and (org-files-db-cache--entry-at-state-p entry state key)
       (not (org-files-db-cache--entry-stale-p entry))
       (not (org-files-db-cache--entry-source-dirty-p entry))))

(defun org-files-db-cache--source-dirty-at-state-p (entry state)
  "Return non-nil when ENTRY awaits indexing at committed STATE."
  (and entry state
       (org-files-db-cache--entry-source-dirty-p entry)
       (equal (org-files-db-cache--entry-database-id entry)
              (plist-get state :database-id))
       (= (org-files-db-cache--entry-generation entry)
          (plist-get state :generation))))

(defun org-files-db-cache--result-owner-path (result)
  "Return canonical owning path for structural RESULT, or nil."
  (or (org-files-db--get result 'location 'file_path)
      (and (memq (org-files-db--kind result) '(file root))
           (or (org-files-db--get result 'path)
               (org-files-db--get result 'file_path)))))

(defun org-files-db-cache--all-results-owned-p (results)
  "Return non-nil when every structural result in RESULTS has an owner path."
  (seq-every-p
   (lambda (result)
     (let ((path (org-files-db-cache--result-owner-path result)))
       (and (stringp path) (not (string-empty-p path)))))
   results))

(defun org-files-db-cache--patch-safe-p (view entry)
  "Return non-nil when VIEW and complete ENTRY permit file-delta patching."
  (let ((properties (cdr view)))
    (and entry
         (eq (org-files-db-cache--view-command view) 'query)
         (org-files-db-cache--entry-complete-p entry)
         (vectorp
          (and (org-files-db-cache--entry-presentation entry)
               (org-files-db--presentation-rows
                (org-files-db-cache--entry-presentation entry))))
         (not (plist-get properties :limit))
         (not (plist-get properties :offset))
         (not (plist-get properties :truncate))
         (not (plist-get properties :top))
         (not (plist-get properties :row-source))
         (not (plist-get properties :sort))
         (memq (org-files-db--query-target (plist-get properties :query))
               '(headings links files))
         (org-files-db-cache--all-results-owned-p
          (org-files-db-cache--entry-results entry)))))

(defun org-files-db-cache--fetch-view (view config-file)
  "Fetch complete result data for VIEW using effective CONFIG-FILE."
  (let* ((properties (cdr view))
         (origin (format "View `%s'" (car view))))
    (pcase (org-files-db-cache--view-command view)
      ('query
       (let ((query (plist-get properties :query)))
         (unless query
           (user-error "Query view `%s' has no :query" (car view)))
         (org-files-db--fetch-query
          query (plist-get properties :columns) config-file origin)))
      ('search
       (let ((expression (plist-get properties :expression))
             (scope (or (plist-get properties :scope) 'all)))
         (unless (and (stringp expression)
                      (not (string-empty-p expression)))
           (user-error "Search view `%s' has no valid :expression" (car view)))
         (org-files-db--fetch-search
          expression (plist-get properties :columns)
          scope config-file origin)))
      (_ (user-error "View `%s' has an invalid command" (car view))))))

(defun org-files-db-cache--fetch-restricted-query
    (view config-file files columns)
  "Fetch query VIEW restricted to FILES using CONFIG-FILE and COLUMNS."
  (let* ((query (plist-get (cdr view) :query))
         (includes
          (org-files-db--column-includes
           (org-files-db--normalize-columns columns)))
         (response
          (org-files-db--execute-query-restricted
           query files config-file (format "View `%s' patch" (car view))
           includes)))
    (org-files-db--results-with-config
     (org-files-db--normalize-results response)
     config-file)))

(defun org-files-db-cache--compact-presentation (presentation)
  "Release redundant internals from prepared PRESENTATION and return it.
Keep logical rows because patch refreshes reuse their cached cells.  Once cell
values have been prepared, rows no longer need presentation-source objects."
  (when-let* ((rows (org-files-db--presentation-rows presentation))
              ((vectorp rows)))
    (dotimes (index (length rows))
      (setf (org-files-db--presentation-row-source (aref rows index)) nil)))
  (setf (org-files-db--presentation-sources presentation) nil
        (org-files-db--presentation-face-cache presentation) nil)
  presentation)

(defun org-files-db-cache--prepare (results columns)
  "Prepare RESULTS with COLUMNS while retaining patchable logical rows."
  (let* ((large-p
          (>= (length results) org-files-db--large-presentation-row-count))
         (bounded-gc-p
          (and large-p
               (< gc-cons-threshold
                  org-files-db--large-presentation-gc-threshold)
               (< gc-cons-percentage 1.0)))
         (gc-cons-threshold
          (if bounded-gc-p
              org-files-db--large-presentation-gc-threshold
            gc-cons-threshold)))
    (org-files-db-cache--compact-presentation
     (org-files-db--prepare-presentation-1 results columns))))

(defun org-files-db-cache--column-definitions (columns)
  "Return plain definitions represented by normalized COLUMNS."
  (mapcar #'org-files-db--presentation-column-definition
          (append columns nil)))

(defun org-files-db-cache--plain-data-p (value)
  "Return non-nil when VALUE is acyclic readable async worker data."
  (let ((states (make-hash-table :test #'eq))
        (exit-marker (make-symbol "org-files-db-cache-exit"))
        (stack (list value))
        (valid t))
    (while (and valid stack)
      (let ((item (pop stack)))
        (if (and (consp item) (eq (car item) exit-marker))
            (puthash (cdr item) 'done states)
          (cond
           ((or (null item) (stringp item) (numberp item) (symbolp item)))
           ((or (consp item) (vectorp item))
            (pcase (gethash item states)
              ('visiting (setq valid nil))
              ('done nil)
              (_
               (puthash item 'visiting states)
               (push (cons exit-marker item) stack)
               (if (consp item)
                   (progn
                     (push (cdr item) stack)
                     (push (car item) stack))
                 (let ((index (1- (length item))))
                   (while (>= index 0)
                     (push (aref item index) stack)
                     (setq index (1- index))))))))
           (t (setq valid nil))))))
    valid))

(defun org-files-db-cache--write-data-file (file value)
  "Write readable Lisp VALUE to transport FILE and return its byte size.
Worker payloads are assembled exclusively from normalized JSON result objects
and plain column definitions.  Avoid another full graph walk here: malformed
artifacts are rejected by metadata and publication validation in the parent."
  (let ((coding-system-for-write 'utf-8-emacs-unix)
        print-level
        print-length
        (print-circle nil))
    (with-temp-buffer
      (prin1 value (current-buffer))
      (write-region (point-min) (point-max) file nil 'silent)))
  (file-attribute-size (file-attributes file)))

(defun org-files-db-cache--read-data-file (file)
  "Read one complete plain Lisp value from transport FILE."
  (unless (and (stringp file) (file-regular-p file) (file-readable-p file))
    (signal 'org-files-db-error
            (list "Cache worker result artifact is missing or unreadable")))
  (let ((coding-system-for-read 'utf-8-emacs-unix)
        value)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (setq value (read (current-buffer)))
      (skip-chars-forward " \t\r\n")
      (unless (eobp)
        (signal 'org-files-db-error
                (list "Cache worker result artifact has trailing data"))))
    value))

(defun org-files-db-cache--delete-transport-file (file)
  "Delete cache transport FILE when it exists."
  (when (and (stringp file) (file-exists-p file))
    (condition-case nil
        (delete-file file)
      (file-error nil))))

(defun org-files-db-cache--cleanup-job-transport (job)
  "Cancel JOB's watchdog and remove its transport artifacts."
  (when job
    (when (timerp (org-files-db-cache--job-watchdog-timer job))
      (cancel-timer (org-files-db-cache--job-watchdog-timer job)))
    (setf (org-files-db-cache--job-watchdog-timer job) nil)
    (org-files-db-cache--delete-transport-file
     (org-files-db-cache--job-request-file job))
    (org-files-db-cache--delete-transport-file
     (org-files-db-cache--job-result-file job))
    (setf (org-files-db-cache--job-request-file job) nil
          (org-files-db-cache--job-result-file job) nil)))

(defun org-files-db-cache--estimated-memory (results presentation)
  "Return an approximate retained byte cost for RESULTS and PRESENTATION."
  (let* ((rows (org-files-db--presentation-rows presentation))
         (row-count (if (vectorp rows) (length rows) (length results)))
         (column-count
          (length (org-files-db--presentation-columns presentation))))
    (+ (* (length results) 256)
       (* row-count 128)
       (* row-count column-count 96)
       (cl-loop
        for candidate in (org-files-db--presentation-candidates presentation)
        sum (* 2 (length candidate))))))

(defun org-files-db-cache--skip-key (view-token state)
  "Return automatic skip key for VIEW-TOKEN and index STATE."
  (list view-token
        (plist-get state :database-id)
        (plist-get state :generation)))

(defun org-files-db-cache--entry-eligible-p (result-count)
  "Return non-nil when RESULT-COUNT fits configured automatic limits."
  (and (> org-files-db-views-pre-cache-max-entries 0)
       (<= result-count org-files-db-views-pre-cache-max-results)
       (<= result-count org-files-db-views-pre-cache-max-total-results)))

(defun org-files-db-cache--total-results ()
  "Return total row count retained across all cache entries."
  (let ((total 0))
    (maphash
     (lambda (_key entry)
       (setq total (+ total (org-files-db-cache--entry-result-count entry))))
     org-files-db-cache--entries)
    total))

(defun org-files-db-cache--lru-candidates ()
  "Return evictable entries ordered from least to most recently used."
  (sort
   (let (entries)
     (maphash
      (lambda (_key entry)
        (unless (org-files-db-cache--entry-in-use-p entry)
          (push entry entries)))
      org-files-db-cache--entries)
     entries)
   (lambda (left right)
     (< (or (org-files-db-cache--entry-last-used left) 0.0)
        (or (org-files-db-cache--entry-last-used right) 0.0)))))

(defun org-files-db-cache--enforce-limits ()
  "Evict least-recently-used entries until configured limits are met."
  (let ((candidates (org-files-db-cache--lru-candidates)))
    (while (and candidates
                (or (> (hash-table-count org-files-db-cache--entries)
                       org-files-db-views-pre-cache-max-entries)
                    (> (org-files-db-cache--total-results)
                       org-files-db-views-pre-cache-max-total-results)))
      (org-files-db-cache--remove-entry (pop candidates)))))

(defun org-files-db-cache--record-result-limit-skip
    (view config-file state result-count)
  "Record that VIEW with CONFIG-FILE at STATE exceeds RESULT-COUNT limits."
  (let ((view-token (org-files-db-cache--view-token view config-file)))
    (puthash
     (org-files-db-cache--skip-key view-token state)
     (list :reason 'result-limit :result-count result-count)
     org-files-db-cache--skipped)))

(defun org-files-db-cache--retire-obsolete-view-entry (name)
  "Release the obsolete retained entry for predefined view NAME.
An entry currently displayed remains alive until its completion session closes,
but it is removed from the view-name index immediately."
  (when-let* ((previous (org-files-db-cache--entry-for-view name)))
    (if (org-files-db-cache--entry-in-use-p previous)
        (progn
          (setf (org-files-db-cache--entry-stale-p previous) t)
          (when (equal (gethash name org-files-db-cache--view-keys)
                       (org-files-db-cache--entry-table-key previous))
            (remhash name org-files-db-cache--view-keys)))
      (org-files-db-cache--remove-entry previous))))

(defun org-files-db-cache--publish
    (view config-file state results columns presentation refresh-token)
  "Publish a complete prepared replacement for VIEW using CONFIG-FILE at STATE.
Return the new entry for RESULTS, COLUMNS, and PRESENTATION, or nil when
automatic cache limits reject it.  REFRESH-TOKEN identifies the refresh."
  (let* ((name (car view))
         (view-token (org-files-db-cache--view-token view config-file))
         (rows (org-files-db--presentation-rows presentation))
         (count (if (vectorp rows) (length rows) (length results)))
         (skip-key (org-files-db-cache--skip-key view-token state))
         (previous (org-files-db-cache--entry-for-view name)))
    (if (not (org-files-db-cache--entry-eligible-p count))
        (progn
          (org-files-db-cache--record-result-limit-skip
           view config-file state count)
          ;; The previous entry cannot become current for this definition and
          ;; generation.  Keep it only while an existing completion is using
          ;; it; otherwise release the obsolete retained generation now.
          (org-files-db-cache--retire-obsolete-view-entry name)
          nil)
      (let* ((key (org-files-db-cache--cache-key view config-file state))
             (now (float-time))
             (defer-limits-p (org-files-db-cache--view-in-use-p name)))
        (unless defer-limits-p
          (org-files-db-cache--remove-entry previous))
        (let* ((storage-key (org-files-db-cache--allocate-storage-key key))
               (entry
                (make-org-files-db-cache--entry
                 :key key
                 :storage-key storage-key
                 :view-name name
                 :view-token view-token
                 :command (org-files-db-cache--view-command view)
                 :config-file config-file
                 :database-id (plist-get state :database-id)
                 :generation (plist-get state :generation)
                 :created-at now
                 :last-used now
                 :view (copy-tree view)
                 :results results
                 :columns columns
                 :presentation presentation
                 :result-count count
                 :estimated-memory
                 (org-files-db-cache--estimated-memory results presentation)
                 :complete-p t
                 :stale-p nil
                 :source-dirty-p nil
                 :in-use-p nil
                 :refresh-token refresh-token)))
          ;; A completion already displaying an older entry keeps that object
          ;; alive until its minibuffer closes.  STORAGE-KEY prevents a new
          ;; generation with the same logical cache key from overwriting it.
          (puthash storage-key entry org-files-db-cache--entries)
          (puthash name storage-key org-files-db-cache--view-keys)
          (remhash skip-key org-files-db-cache--skipped)
          (remhash skip-key org-files-db-cache--failures)
          ;; Let an in-use obsolete entry close before enforcing limits, so the
          ;; replacement is not immediately evicted to protect the old display.
          (unless defer-limits-p
            (org-files-db-cache--enforce-limits))
          (org-files-db-cache--debug
           "cache published view=%s db=%s gen=%s rows=%d notify-to-publish=%s"
           name (plist-get state :database-id) (plist-get state :generation)
           count
           (if-let* ((notified-at
                      (gethash (org-files-db-cache--config-key config-file)
                               org-files-db-cache--notification-times)))
               (format "%.3fs" (org-files-db-cache--elapsed notified-at))
             "n/a"))
          (remhash (org-files-db-cache--config-key config-file)
                   org-files-db-cache--notification-times)
          entry)))))

(defun org-files-db-cache--same-state-p (left right)
  "Return non-nil when LEFT and RIGHT identify the same committed state."
  (and left right
       (equal (plist-get left :database-id)
              (plist-get right :database-id))
       (= (plist-get left :generation)
          (plist-get right :generation))))

(defun org-files-db-cache--build-sync (view config-file state &optional publish-p)
  "Build VIEW synchronously for STATE using CONFIG-FILE.
When PUBLISH-P is non-nil, publish only after a final generation check.  Return
plist keys :presentation, :results, :columns, and optional :entry."
  (let ((attempt 0)
        fetched presentation final-state entry stable-p done-p)
    (while (and (< attempt 2) (not done-p))
      (setq attempt (1+ attempt))
      (let ((source-state state))
        (setq fetched (org-files-db-cache--fetch-view view config-file)
              presentation
              (org-files-db-cache--prepare
               (plist-get fetched :results)
               (plist-get fetched :columns))
              final-state
              (if publish-p
                  (condition-case nil
                      (org-files-db-cache--read-index-state config-file)
                    (error nil))
                source-state))
        (cond
         ((not publish-p)
          (setq stable-p t done-p t))
         ((null final-state)
          (setq done-p t))
         ((and source-state
               (org-files-db-cache--same-state-p source-state final-state))
          (setq stable-p t done-p t))
         (t
          (setq state final-state)))))
    (when (and publish-p (not stable-p) final-state)
      ;; The generation advanced during both checked attempts.  Do not present
      ;; data already known to precede FINAL-STATE and do not loop forever on a
      ;; busy index.  Run the ordinary synchronous path once more without
      ;; publishing it as a generation-labelled cache entry.
      (setq fetched (org-files-db-cache--fetch-view view config-file)
            presentation
            (org-files-db-cache--prepare
             (plist-get fetched :results)
             (plist-get fetched :columns))))
    (when (and publish-p stable-p final-state)
      (setq entry
            (org-files-db-cache--publish
             view config-file final-state
             (plist-get fetched :results)
             (plist-get fetched :columns)
             presentation
             (gethash (car view) org-files-db-cache--refresh-tokens))))
    (list :presentation presentation
          :results (plist-get fetched :results)
          :columns (plist-get fetched :columns)
          :entry entry
          :state final-state)))

(defun org-files-db-cache--present-entry (entry action prompt)
  "Present cache ENTRY with ACTION and PROMPT, protecting it from eviction."
  (setf (org-files-db-cache--entry-in-use-p entry) t)
  (org-files-db-cache--touch-entry entry)
  (unwind-protect
      (org-files-db--present-presentation
       (org-files-db-cache--entry-presentation entry)
       action prompt)
    (setf (org-files-db-cache--entry-in-use-p entry) nil)
    (unless (equal
             (gethash (org-files-db-cache--entry-view-name entry)
                      org-files-db-cache--view-keys)
             (org-files-db-cache--entry-table-key entry))
      (org-files-db-cache--remove-entry entry))
    (org-files-db-cache--enforce-limits)))

(defun org-files-db-cache--refresh-active-p (name)
  "Return non-nil when the latest refresh for predefined view NAME is active."
  (let ((latest (gethash name org-files-db-cache--refresh-tokens -1)))
    (or (and org-files-db-cache--current-job
             (equal name
                    (org-files-db-cache--job-view-name
                     org-files-db-cache--current-job))
             (= latest
                (org-files-db-cache--job-refresh-token
                 org-files-db-cache--current-job)))
        (seq-some
         (lambda (job)
           (and (equal name (org-files-db-cache--job-view-name job))
                (= latest (org-files-db-cache--job-refresh-token job))))
         org-files-db-cache--queue))))

(defun org-files-db-cache--prioritize-refresh (name)
  "Move NAME's latest queued refresh ahead of other queued prewarm jobs."
  (let ((latest (gethash name org-files-db-cache--refresh-tokens -1))
        prioritized retained)
    (dolist (job org-files-db-cache--queue)
      (if (and (equal name (org-files-db-cache--job-view-name job))
               (= latest (org-files-db-cache--job-refresh-token job)))
          (push job prioritized)
        (push job retained)))
    (when prioritized
      (setq org-files-db-cache--queue
            (nconc (nreverse prioritized) (nreverse retained)))
      (org-files-db-cache--debug
       "interactive refresh prioritized view=%s queue=%d"
       name (length org-files-db-cache--queue)))))

(defun org-files-db-cache--known-index-state (config-file fallback)
  "Return latest known state for CONFIG-FILE, or FALLBACK when unavailable."
  (or (gethash (org-files-db-cache--config-key config-file)
               org-files-db-cache--index-states)
      fallback))

(defun org-files-db-cache--wait-for-refresh (view config-file initial-state)
  "Wait for VIEW's async replacement starting from INITIAL-STATE.
CONFIG-FILE is the effective configuration.  Generation changes observed while
waiting are followed through subsequent queued workers.  Return a current cache
entry, or nil after a worker failure, missing replacement, or timeout."
  (let* ((name (car view))
         (timeout (max 0.1 org-files-db-views-pre-cache-wait-timeout))
         (deadline (+ (float-time) timeout))
         (state initial-state)
         entry done)
    (org-files-db-cache--debug
     "interactive lookup waiting view=%s db=%s gen=%s timeout=%.1fs"
     name (plist-get state :database-id) (plist-get state :generation) timeout)
    (org-files-db-cache--request-refresh view config-file nil state)
    (org-files-db-cache--prioritize-refresh name)
    (while (not done)
      (setq state (org-files-db-cache--known-index-state config-file state))
      (let ((key (and state
                      (org-files-db-cache--cache-key
                       view config-file state))))
        (setq entry (org-files-db-cache--entry-for-view name))
        (cond
         ((and state
               (org-files-db-cache--entry-current-p entry state key))
          (setq done t))
         ((>= (float-time) deadline)
          (org-files-db-cache--debug
           "interactive wait timed out view=%s gen=%s"
           name (and state (plist-get state :generation)))
          (setq entry nil
                done t))
         ((not (org-files-db-cache--refresh-active-p name))
          ;; A prior worker may have observed a newer generation and finished
          ;; before this loop iteration.  Ask once more for the latest known
          ;; state; failure backoff prevents a tight retry loop.
          (org-files-db-cache--request-refresh view config-file nil state)
          (org-files-db-cache--prioritize-refresh name)
          (unless (org-files-db-cache--refresh-active-p name)
            (setq entry nil
                  done t)))
         (t
          ;; `accept-process-output' keeps Emacs responsive while the command
          ;; waits for the already-running async replacement.  When our job is
          ;; queued behind another view, waiting on nil services that worker too.
          ;; Waiting on nil services all process filters and sentinels as well as
          ;; timers, including file-notify debounce and worker watchdog timers.
          (accept-process-output nil 0.05)))))
    entry))

(defun org-files-db-cache-present-view
    (view config-file action prompt &optional force-refresh)
  "Present predefined VIEW through its generation-aware cache.
CONFIG-FILE is already resolved.  ACTION and PROMPT control selection.
FORCE-REFRESH bypasses background work and performs a complete synchronous
refresh.  A normal stale view waits for its async replacement instead of
cancelling that worker and duplicating the expensive refresh synchronously."
  (unless org-files-db-cache-mode
    (user-error "org-files-db-cache-mode is not enabled"))
  (let* ((name (car view))
         (state
          (condition-case err
              (org-files-db-cache--read-index-state config-file)
            (error
             (org-files-db-cache--record-status-failure
              config-file (error-message-string err))
             (message "View `%s' cache status failed: %s"
                      name (error-message-string err))
             nil))))
    (cond
     ((null state)
      (let ((built (org-files-db-cache--build-sync
                    view config-file nil nil)))
        (org-files-db--present-presentation
         (plist-get built :presentation) action prompt)))
     (force-refresh
      (org-files-db-cache--obsolete-view-refresh name)
      (let* ((built (org-files-db-cache--build-sync
                     view config-file state t))
             (entry (plist-get built :entry)))
        (if entry
            (org-files-db-cache--present-entry entry action prompt)
          (org-files-db--present-presentation
           (plist-get built :presentation) action prompt))))
     (t
      (let* ((key (org-files-db-cache--cache-key view config-file state))
             (entry (org-files-db-cache--entry-for-view name)))
        (cond
         ((org-files-db-cache--entry-current-p entry state key)
          (org-files-db-cache--debug
           "warm lookup used view=%s db=%s gen=%s"
           name (plist-get state :database-id) (plist-get state :generation))
          (org-files-db-cache--present-entry entry action prompt))
         ;; Package-controlled source changes can be newer than the committed
         ;; Rust generation.  Until orgfdb advances that generation, the old
         ;; prepared entry is still the exact view of the authoritative DB
         ;; state, so avoid a redundant synchronous query of the same state.
         ((and (org-files-db-cache--source-dirty-at-state-p entry state)
               (org-files-db-cache--entry-at-state-p entry state key))
          (org-files-db-cache--debug
           "warm committed lookup used view=%s db=%s gen=%s source-dirty=t"
           name (plist-get state :database-id) (plist-get state :generation))
          (org-files-db-cache--present-entry entry action prompt))
         ;; Preserve the original fast first-open behaviour when no cache and
         ;; no initial pre-cache worker exists yet.
         ((and (null entry)
               (not (org-files-db-cache--refresh-active-p name)))
          (let* ((built (org-files-db-cache--build-sync
                         view config-file state t))
                 (new-entry (plist-get built :entry)))
            (if new-entry
                (org-files-db-cache--present-entry new-entry action prompt)
              (org-files-db--present-presentation
               (plist-get built :presentation) action prompt))))
         (t
          (let ((refreshed
                 (org-files-db-cache--wait-for-refresh
                  view config-file state)))
            (if refreshed
                (org-files-db-cache--present-entry refreshed action prompt)
              ;; A failed or timed-out worker is the only normal-view case that
              ;; falls back to a fresh synchronous rebuild.  Obsolete any hung
              ;; worker first so it cannot publish over the fallback result.
              (org-files-db-cache--obsolete-view-refresh name)
              (let* ((fresh-state
                      (condition-case nil
                          (org-files-db-cache--read-index-state config-file)
                        (error state)))
                     (built
                      (org-files-db-cache--build-sync
                       view config-file fresh-state t))
                     (new-entry (plist-get built :entry)))
                (if new-entry
                    (org-files-db-cache--present-entry
                     new-entry action prompt)
                  (org-files-db--present-presentation
                   (plist-get built :presentation) action prompt))))))))))))

(defun org-files-db-cache--merge-patch-results
    (base-results upsert-files deleted-files restricted-results)
  "Return patched BASE-RESULTS using UPSERT-FILES and DELETED-FILES.
Rows owned by affected paths are removed before RESTRICTED-RESULTS are merged.
File groups use canonical path order and retain the CLI order of rows within
each owning file.  This pure result-level operation is safe in async workers."
  (let ((affected (make-hash-table :test #'equal))
        (groups (make-hash-table :test #'equal)))
    (dolist (path (append upsert-files deleted-files))
      (puthash path t affected))
    (dolist (result base-results)
      (let ((path (org-files-db-cache--result-owner-path result)))
        (unless path
          (signal 'org-files-db-error
                  (list "Cached query result lacks a stable owning path")))
        (unless (gethash path affected)
          (puthash path (cons result (gethash path groups)) groups))))
    (dolist (result restricted-results)
      (let ((path (org-files-db-cache--result-owner-path result)))
        (unless path
          (signal 'org-files-db-error
                  (list "Restricted query result lacks a stable owning path")))
        (puthash path (cons result (gethash path groups)) groups)))
    (let (paths merged)
      (maphash
       (lambda (path results)
         (puthash path (nreverse results) groups)
         (push path paths))
       groups)
      (dolist (path (sort paths #'string<))
        (setq merged (nconc merged (copy-sequence (gethash path groups)))))
      merged)))

(defun org-files-db-cache--apply-patch-results
    (entry upsert-files deleted-files restricted-results)
  "Return complete replacement results by patching ENTRY.
UPSERT-FILES, DELETED-FILES, and RESTRICTED-RESULTS describe the authoritative
file delta."
  (org-files-db-cache--merge-patch-results
   (org-files-db-cache--entry-results entry)
   upsert-files deleted-files restricted-results))

(defun org-files-db-cache--next-refresh-token (view-name)
  "Return and store the next refresh token for VIEW-NAME."
  (setq org-files-db-cache--refresh-counter
        (1+ org-files-db-cache--refresh-counter))
  (puthash view-name org-files-db-cache--refresh-counter
           org-files-db-cache--refresh-tokens)
  org-files-db-cache--refresh-counter)

(defun org-files-db-cache--job-equal-p (left right)
  "Return non-nil when jobs LEFT and RIGHT represent the same replacement."
  (and (equal (org-files-db-cache--job-view-name left)
              (org-files-db-cache--job-view-name right))
       (equal (org-files-db-cache--job-cache-key left)
              (org-files-db-cache--job-cache-key right))
       (= (org-files-db-cache--job-target-generation left)
          (org-files-db-cache--job-target-generation right))
       (eq (org-files-db-cache--job-refresh-type left)
           (org-files-db-cache--job-refresh-type right))))

(defun org-files-db-cache--failure-key (view-token state)
  "Return failure-backoff key for VIEW-TOKEN and index STATE."
  (org-files-db-cache--skip-key view-token state))

(defun org-files-db-cache--record-failure (job error-text &optional stderr)
  "Record background JOB failure with ERROR-TEXT and optional STDERR."
  (let ((state (list :database-id
                     (org-files-db-cache--job-database-id job)
                     :generation
                     (org-files-db-cache--job-target-generation job))))
    (puthash
     (org-files-db-cache--failure-key
      (org-files-db-cache--job-view-token job) state)
     (list :view-name (org-files-db-cache--job-view-name job)
           :cache-key (org-files-db-cache--job-cache-key job)
           :database-id (org-files-db-cache--job-database-id job)
           :source-generation
           (org-files-db-cache--job-source-generation job)
           :target-generation
           (org-files-db-cache--job-target-generation job)
           :refresh-type (org-files-db-cache--job-refresh-type job)
           :started-at (org-files-db-cache--job-started-at job)
           :error error-text
           :stderr stderr)
     org-files-db-cache--failures)))

(defun org-files-db-cache--record-refresh-failure
    (view config-file state error-text)
  "Record VIEW refresh failure for CONFIG-FILE at STATE using ERROR-TEXT."
  (let* ((view-token (org-files-db-cache--view-token view config-file))
         (entry (org-files-db-cache--entry-for-view (car view))))
    (puthash
     (org-files-db-cache--failure-key view-token state)
     (list :view-name (car view)
           :cache-key
           (condition-case nil
               (org-files-db-cache--cache-key view config-file state)
             (error nil))
           :database-id (plist-get state :database-id)
           :source-generation
           (and entry (org-files-db-cache--entry-generation entry))
           :target-generation (plist-get state :generation)
           :refresh-type 'classification
           :started-at (float-time)
           :error error-text
           :stderr nil)
     org-files-db-cache--failures)))

(defun org-files-db-cache--equivalent-job (job)
  "Return an active equivalent running or queued replacement for JOB."
  (let ((latest
         (gethash (org-files-db-cache--job-view-name job)
                  org-files-db-cache--refresh-tokens -1)))
    (or (and org-files-db-cache--current-job
             (= (org-files-db-cache--job-refresh-token
                 org-files-db-cache--current-job)
                latest)
             (org-files-db-cache--job-equal-p
              job org-files-db-cache--current-job)
             org-files-db-cache--current-job)
        (seq-find
         (lambda (queued)
           (and (= (org-files-db-cache--job-refresh-token queued) latest)
                (org-files-db-cache--job-equal-p job queued)))
         org-files-db-cache--queue))))

(defun org-files-db-cache--enqueue-job (job)
  "Enqueue JOB unless an equivalent replacement is running or pending.
When a replacement already exists, preserve its refresh token so duplicate
wake-up events cannot obsolete the only worker for that state."
  (if-let* ((existing (org-files-db-cache--equivalent-job job)))
      (progn
        (puthash (org-files-db-cache--job-view-name existing)
                 (org-files-db-cache--job-refresh-token existing)
                 org-files-db-cache--refresh-tokens)
        existing)
    (setq org-files-db-cache--queue
          (nconc org-files-db-cache--queue (list job)))
    (org-files-db-cache--debug
     "%s refresh queued view=%s source=%s target=%s token=%s queue=%d"
     (org-files-db-cache--job-refresh-type job)
     (org-files-db-cache--job-view-name job)
     (or (org-files-db-cache--job-source-generation job) "none")
     (org-files-db-cache--job-target-generation job)
     (org-files-db-cache--job-refresh-token job)
     (length org-files-db-cache--queue))
    (org-files-db-cache--start-next-worker)
    job))

(defun org-files-db-cache--worker-view-data (view)
  "Return serializable worker-relevant data from VIEW."
  (let ((properties (cdr view)))
    (list (car view)
          :command (plist-get properties :command)
          :query (plist-get properties :query)
          :expression (plist-get properties :expression)
          :scope (or (plist-get properties :scope) 'all)
          :columns (copy-tree (plist-get properties :columns)))))

(defun org-files-db-cache--prepare-job-transport (job entry)
  "Create the result transport artifact for JOB and source ENTRY.
ENTRY is intentionally not serialized.  Patch jobs no longer copy the complete
retained cache into a request artifact.  The worker returns only restricted
replacement results, which the main Emacs merges with the still-published
source entry after validating its generation."
  (ignore entry)
  (org-files-db-cache--cleanup-job-transport job)
  (setf (org-files-db-cache--job-result-file job)
        (make-temp-file "org-files-db-cache-result-"))
  job)

(defun org-files-db-cache--worker-request (job view)
  "Return serializable async worker request for JOB and VIEW."
  (let* ((patch-p (eq (org-files-db-cache--job-refresh-type job) 'patch))
         (entry
          (org-files-db-cache--entry-for-view
           (org-files-db-cache--job-view-name job))))
    (when (and patch-p
               (or (null entry)
                   (not (equal
                         (org-files-db-cache--entry-database-id entry)
                         (org-files-db-cache--job-database-id job)))
                   (/= (org-files-db-cache--entry-generation entry)
                       (org-files-db-cache--job-source-generation job))))
      (signal 'org-files-db-error
              (list "Patch cache source changed before worker start")))
    (let ((request
           (list :view (org-files-db-cache--worker-view-data view)
                 :command (org-files-db-cache--job-command job)
                 :config-file (org-files-db-cache--job-config-file job)
                 :database-id (org-files-db-cache--job-database-id job)
                 :source-generation
                 (org-files-db-cache--job-source-generation job)
                 :target-generation
                 (org-files-db-cache--job-target-generation job)
                 :cache-key (org-files-db-cache--job-cache-key job)
                 :view-token (org-files-db-cache--job-view-token job)
                 :refresh-token (org-files-db-cache--job-refresh-token job)
                 :refresh-type (org-files-db-cache--job-refresh-type job)
                 :upsert-files (org-files-db-cache--job-upsert-files job)
                 :deleted-files (org-files-db-cache--job-deleted-files job)
                 :columns
                 (and entry
                      (org-files-db-cache--column-definitions
                       (org-files-db-cache--entry-columns entry)))
                 :presentation-options
                 (list :heading-columns (copy-tree org-files-db-heading-columns)
                       :file-columns (copy-tree org-files-db-file-columns)
                       :link-columns (copy-tree org-files-db-link-columns)
                       :search-columns (copy-tree org-files-db-search-columns)
                       :outline-separator org-files-db-outline-path-separator
                       :outline-include-root org-files-db-outline-path-include-root
                       :outline-include-match org-files-db-outline-path-include-match
                       :truncate-position org-files-db-truncate-position
                       :truncate-marker org-files-db-truncate-marker))))
      (unless (org-files-db-cache--plain-data-p request)
        (signal 'org-files-db-error
                (list "Cache worker request is not serializable")))
      request)))

(defun org-files-db-cache--worker-run (request)
  "Execute serializable cache worker REQUEST and return compact plain data.
Complete jobs return fetched results.  Patch jobs return only restricted
upsert results; the main Emacs merges them with the retained source entry."
  (condition-case err
      (let* ((options (plist-get request :presentation-options))
             (org-files-db-heading-columns
              (if (plist-member options :heading-columns)
                  (plist-get options :heading-columns)
                org-files-db-heading-columns))
             (org-files-db-file-columns
              (if (plist-member options :file-columns)
                  (plist-get options :file-columns)
                org-files-db-file-columns))
             (org-files-db-link-columns
              (if (plist-member options :link-columns)
                  (plist-get options :link-columns)
                org-files-db-link-columns))
             (org-files-db-search-columns
              (if (plist-member options :search-columns)
                  (plist-get options :search-columns)
                org-files-db-search-columns))
             (org-files-db-outline-path-separator
              (if (plist-member options :outline-separator)
                  (plist-get options :outline-separator)
                org-files-db-outline-path-separator))
             (org-files-db-outline-path-include-root
              (if (plist-member options :outline-include-root)
                  (plist-get options :outline-include-root)
                org-files-db-outline-path-include-root))
             (org-files-db-outline-path-include-match
              (if (plist-member options :outline-include-match)
                  (plist-get options :outline-include-match)
                org-files-db-outline-path-include-match))
             (org-files-db-truncate-position
              (if (plist-member options :truncate-position)
                  (plist-get options :truncate-position)
                org-files-db-truncate-position))
             (org-files-db-truncate-marker
              (if (plist-member options :truncate-marker)
                  (plist-get options :truncate-marker)
                org-files-db-truncate-marker))
             (view (plist-get request :view))
             (config-file (plist-get request :config-file))
             (expected-id (plist-get request :database-id))
             (expected-generation (plist-get request :target-generation))
             (state (org-files-db-cache--read-index-state config-file)))
        (unless (and (equal expected-id (plist-get state :database-id))
                     (= expected-generation (plist-get state :generation)))
          (signal 'org-files-db-error
                  (list "Index changed before cache worker started")))
        (let* ((refresh-type (plist-get request :refresh-type))
               (patch-p (eq refresh-type 'patch))
               (fetched
                (unless patch-p
                  (org-files-db-cache--fetch-view view config-file)))
               (columns
                (if fetched
                    (plist-get fetched :columns)
                  (org-files-db--normalize-columns
                   (or (plist-get request :columns)
                       (org-files-db-cache--view-columns view)))))
               (restricted-results
                (when (and patch-p (plist-get request :upsert-files))
                  (org-files-db-cache--fetch-restricted-query
                   view config-file
                   (plist-get request :upsert-files)
                   columns)))
               (results
                (if patch-p restricted-results (plist-get fetched :results)))
               (response
                (list :ok t
                      :format-version org-files-db-cache--format-version
                      :database-id expected-id
                      :source-generation
                      (plist-get request :source-generation)
                      :target-generation expected-generation
                      :cache-key (plist-get request :cache-key)
                      :view-token (plist-get request :view-token)
                      :refresh-token (plist-get request :refresh-token)
                      :refresh-type refresh-type
                      :payload-kind (if patch-p 'restricted 'complete)
                      :results results
                      :columns
                      (org-files-db-cache--column-definitions columns))))
          response))
    (error
     (list :ok nil :error (error-message-string err)))))

(defun org-files-db-cache--worker-run-to-file (request result-file)
  "Run cache worker REQUEST and write its payload to RESULT-FILE.
Return only small control data suitable for async.el serialization."
  (let* ((started-at (float-time))
         (response (org-files-db-cache--worker-run request)))
    (if (not (plist-get response :ok))
        response
      (let* ((write-started-at (float-time))
             (payload-bytes
              (org-files-db-cache--write-data-file result-file response))
             (control
              (list :ok t
                    :format-version
                    (plist-get response :format-version)
                    :database-id (plist-get response :database-id)
                    :source-generation (plist-get response :source-generation)
                    :target-generation (plist-get response :target-generation)
                    :cache-key (plist-get response :cache-key)
                    :view-token (plist-get response :view-token)
                    :refresh-token (plist-get response :refresh-token)
                    :refresh-type (plist-get response :refresh-type)
                    :payload-bytes payload-bytes
                    :payload-write-seconds
                    (org-files-db-cache--elapsed write-started-at)
                    :worker-seconds
                    (org-files-db-cache--elapsed started-at))))
        (unless (org-files-db-cache--plain-data-p control)
          (signal 'org-files-db-error
                  (list "Cache worker control result is not serializable")))
        control))))

(defun org-files-db-cache--async-start (start finish)
  "Start asynchronous START form and call FINISH with its result."
  ;; Cache workers are strictly local and never need TRAMP credentials.
  ;; Disabling async.el password detection also prevents arbitrary worker
  ;; output from being mistaken for a password prompt and opening recursive
  ;; minibuffers while a large cache payload is transferred.
  (let ((async-prompt-for-password nil))
    (async-start start finish)))

(defun org-files-db-cache--worker-process-failed (job process reason)
  "Finish active JOB as failed for PROCESS using REASON."
  (when (eq job org-files-db-cache--current-job)
    (setq org-files-db-cache--current-worker nil
          org-files-db-cache--current-job nil)
    (org-files-db-cache--record-failure job reason)
    (org-files-db-cache--debug
     "worker failed view=%s target=%s token=%s error=%s"
     (org-files-db-cache--job-view-name job)
     (org-files-db-cache--job-target-generation job)
     (org-files-db-cache--job-refresh-token job) reason)
    (org-files-db-cache--cleanup-job-transport job)
    (org-files-db-cache--cancel-worker process)
    (org-files-db-cache--start-next-worker)))

(defun org-files-db-cache--install-worker-sentinel (process job)
  "Wrap async PROCESS sentinel so terminal states always finish JOB."
  (let ((async-sentinel (process-sentinel process)))
    (set-process-sentinel
     process
     (lambda (worker event)
       (condition-case err
           (funcall async-sentinel worker event)
         (error
          (org-files-db-cache--worker-process-failed
           job worker
           (format "Async worker callback failed: %s"
                   (error-message-string err)))))
       (when (and (memq (process-status worker) '(exit signal failed closed))
                  (eq job org-files-db-cache--current-job))
         (org-files-db-cache--worker-process-failed
          job worker
          (format "Async worker terminated without a result (%s)"
                  (string-trim event))))))))

(defun org-files-db-cache--start-worker-watchdog (process job)
  "Start the bounded execution watchdog for PROCESS and JOB."
  (let ((timeout (max 0.1 org-files-db-views-pre-cache-wait-timeout)))
    (setf
     (org-files-db-cache--job-watchdog-timer job)
     (run-at-time
      timeout nil
      (lambda ()
        (when (eq job org-files-db-cache--current-job)
          (org-files-db-cache--debug
           "worker watchdog expired view=%s target=%s timeout=%.1fs"
           (org-files-db-cache--job-view-name job)
           (org-files-db-cache--job-target-generation job) timeout)
          (when (process-live-p process)
            (delete-process process))
          (when (eq job org-files-db-cache--current-job)
            (org-files-db-cache--worker-process-failed
             job process (format "Cache worker exceeded %.1f seconds" timeout)))))))))

(defun org-files-db-cache--start-next-worker ()
  "Start the next queued cache job when no worker is active."
  (when (and org-files-db-cache-mode
             (null org-files-db-cache--current-worker)
             org-files-db-cache--queue)
    (let* ((job (pop org-files-db-cache--queue))
           (view (org-files-db-cache--current-view
                  (org-files-db-cache--job-view-name job)))
           (config-file (org-files-db-cache--job-config-file job)))
      (if (or (null view)
              (not (org-files-db-cache--view-pre-cache-p view))
              (not (equal
                    (org-files-db-cache--view-token view config-file)
                    (org-files-db-cache--job-view-token job)))
              (not (= (gethash (org-files-db-cache--job-view-name job)
                               org-files-db-cache--refresh-tokens -1)
                      (org-files-db-cache--job-refresh-token job))))
          (org-files-db-cache--start-next-worker)
        (condition-case err
            (let* ((entry
                    (org-files-db-cache--entry-for-view
                     (org-files-db-cache--job-view-name job)))
                   (_transport
                    (org-files-db-cache--prepare-job-transport job entry))
                   (request (org-files-db-cache--worker-request job view))
                   (result-file (org-files-db-cache--job-result-file job))
                   (worker-load-path
                    (cons org-files-db-cache--library-directory
                          (copy-sequence load-path)))
                   (worker-default-directory default-directory)
                   (worker-executable (org-files-db--resolve-executable))
                   (async-process-noquery-on-exit t))
              (setf (org-files-db-cache--job-started-at job) (float-time))
              (setq org-files-db-cache--current-job job)
              (org-files-db-cache--debug
               "worker started view=%s source=%s target=%s type=%s token=%s"
               (org-files-db-cache--job-view-name job)
               (or (org-files-db-cache--job-source-generation job) "none")
               (org-files-db-cache--job-target-generation job)
               (org-files-db-cache--job-refresh-type job)
               (org-files-db-cache--job-refresh-token job))
              (condition-case start-error
                  (let ((process
                         (org-files-db-cache--async-start
                          `(lambda ()
                             (condition-case err
                                 (progn
                                   (setq load-path ',worker-load-path
                                         load-prefer-newer t
                                         default-directory
                                         ,worker-default-directory
                                         org-files-db-executable
                                         ,worker-executable
                                         org-files-db-cache--worker-process-p t)
                                   (require 'org-files-db-cache)
                                   (org-files-db-cache--worker-run-to-file
                                    ',request ,result-file))
                               (error
                                (list :ok nil
                                      :error (error-message-string err)))))
                          (lambda (result)
                            (org-files-db-cache--worker-finished job result)))))
                    (setq org-files-db-cache--current-worker process)
                    (when (processp process)
                      (org-files-db-cache--install-worker-sentinel process job)
                      (org-files-db-cache--start-worker-watchdog process job)))
                (error
                 (setq org-files-db-cache--current-worker nil
                       org-files-db-cache--current-job nil)
                 (org-files-db-cache--record-failure
                  job (error-message-string start-error))
                 (org-files-db-cache--cleanup-job-transport job)
                 (org-files-db-cache--start-next-worker))))
          (error
           (setq org-files-db-cache--current-worker nil
                 org-files-db-cache--current-job nil)
           (org-files-db-cache--record-failure
            job (error-message-string err))
           (org-files-db-cache--cleanup-job-transport job)
           (org-files-db-cache--start-next-worker)))))))

(defun org-files-db-cache--job-current-p (job view state)
  "Return non-nil when JOB still matches current VIEW and index STATE."
  (and view state
       (= (gethash (org-files-db-cache--job-view-name job)
                   org-files-db-cache--refresh-tokens -1)
          (org-files-db-cache--job-refresh-token job))
       (equal (org-files-db-cache--view-token
               view (org-files-db-cache--job-config-file job))
              (org-files-db-cache--job-view-token job))
       (equal (plist-get state :database-id)
              (org-files-db-cache--job-database-id job))
       (= (plist-get state :generation)
          (org-files-db-cache--job-target-generation job))
       (equal (org-files-db-cache--cache-key
               view (org-files-db-cache--job-config-file job) state)
              (org-files-db-cache--job-cache-key job))))

(defun org-files-db-cache--worker-metadata-valid-p (job result)
  "Return non-nil when RESULT metadata identifies JOB."
  (and (listp result)
       (plist-get result :ok)
       (equal (plist-get result :format-version)
              org-files-db-cache--format-version)
       (equal (plist-get result :database-id)
              (org-files-db-cache--job-database-id job))
       (equal (plist-get result :source-generation)
              (org-files-db-cache--job-source-generation job))
       (equal (plist-get result :target-generation)
              (org-files-db-cache--job-target-generation job))
       (equal (plist-get result :cache-key)
              (org-files-db-cache--job-cache-key job))
       (equal (plist-get result :view-token)
              (org-files-db-cache--job-view-token job))
       (equal (plist-get result :refresh-token)
              (org-files-db-cache--job-refresh-token job))
       (eq (plist-get result :refresh-type)
           (org-files-db-cache--job-refresh-type job))))

(defun org-files-db-cache--worker-result-valid-p (job result)
  "Return non-nil when async control RESULT is valid for JOB."
  (and (org-files-db-cache--worker-metadata-valid-p job result)
       (integerp (plist-get result :payload-bytes))
       (>= (plist-get result :payload-bytes) 0)
       (org-files-db-cache--plain-data-p result)
       (let* ((file (org-files-db-cache--job-result-file job))
              (attributes (and file (file-attributes file))))
         (and attributes
              (= (plist-get result :payload-bytes)
                 (file-attribute-size attributes))))))

(defun org-files-db-cache--worker-payload-valid-p (job result)
  "Return non-nil when complete worker payload RESULT is valid for JOB."
  (and (org-files-db-cache--worker-metadata-valid-p job result)
       (memq (plist-get result :payload-kind) '(complete restricted))
       (eq (plist-get result :payload-kind)
           (if (eq (org-files-db-cache--job-refresh-type job) 'patch)
               'restricted
             'complete))
       (let ((results (plist-get result :results))
             (columns (plist-get result :columns)))
         (and (or (listp results) (vectorp results))
              (or (listp columns) (vectorp columns))))))

(defun org-files-db-cache--publish-worker-presentation
    (job view results columns presentation)
  "Publish JOB PRESENTATION for VIEW, RESULTS, and COLUMNS after a state check."
  (let ((state
         (condition-case err
             (org-files-db-cache--read-index-state
              (org-files-db-cache--job-config-file job))
           (error
            (org-files-db-cache--record-status-failure
             (org-files-db-cache--job-config-file job)
             (error-message-string err))
            nil))))
    (if (org-files-db-cache--job-current-p job view state)
        (org-files-db-cache--publish
         view (org-files-db-cache--job-config-file job)
         state results columns presentation
         (org-files-db-cache--job-refresh-token job))
      (when view
        (org-files-db-cache--request-refresh
         view (org-files-db-cache--job-config-file job) nil state))
      nil)))

(defun org-files-db-cache--worker-result-list (payload)
  "Return worker PAYLOAD results as a list."
  (let ((results (plist-get payload :results)))
    (cond
     ((null results) nil)
     ((vectorp results) (append results nil))
     ((listp results) results)
     (t
      (signal 'org-files-db-error
              (list "Cache worker payload has invalid results"))))))

(defun org-files-db-cache--patch-source-entry (job)
  "Return the unchanged retained source entry required by patch JOB."
  (let ((entry
         (org-files-db-cache--entry-for-view
          (org-files-db-cache--job-view-name job))))
    (unless (and entry
                 (equal (org-files-db-cache--entry-database-id entry)
                        (org-files-db-cache--job-database-id job))
                 (= (org-files-db-cache--entry-generation entry)
                    (org-files-db-cache--job-source-generation job)))
      (signal 'org-files-db-error
              (list "Patch cache source changed while the worker was running")))
    entry))

(defun org-files-db-cache--complete-worker-results (job payload)
  "Return complete results represented by worker JOB PAYLOAD."
  (let ((worker-results (org-files-db-cache--worker-result-list payload)))
    (if (eq (plist-get payload :payload-kind) 'restricted)
        (let ((entry (org-files-db-cache--patch-source-entry job)))
          (org-files-db-cache--merge-patch-results
           (org-files-db-cache--entry-results entry)
           (org-files-db-cache--job-upsert-files job)
           (org-files-db-cache--job-deleted-files job)
           worker-results))
      worker-results)))

(defun org-files-db-cache--worker-finished (job result)
  "Validate and publish async JOB RESULT, then continue the queue."
  ;; An obsolete async process may still invoke its callback after a newer
  ;; worker has started.  It must not clear or replace the newer worker state.
  (when (eq job org-files-db-cache--current-job)
    (setq org-files-db-cache--current-worker nil
          org-files-db-cache--current-job nil)
    (unwind-protect
        (condition-case err
            (let* ((view
                    (org-files-db-cache--current-view
                     (org-files-db-cache--job-view-name job)))
                   (state
                    (condition-case status-error
                        (org-files-db-cache--read-index-state
                         (org-files-db-cache--job-config-file job))
                      (error
                       (org-files-db-cache--record-status-failure
                        (org-files-db-cache--job-config-file job)
                        (error-message-string status-error))
                       nil))))
              (cond
               ((not (org-files-db-cache--job-current-p job view state))
                (org-files-db-cache--debug
                 "worker result rejected view=%s target=%s token=%s reason=obsolete"
                 (org-files-db-cache--job-view-name job)
                 (org-files-db-cache--job-target-generation job)
                 (org-files-db-cache--job-refresh-token job))
                (when view
                  (org-files-db-cache--request-refresh
                   view (org-files-db-cache--job-config-file job) nil state)))
               ((not (org-files-db-cache--worker-result-valid-p job result))
                (org-files-db-cache--record-failure
                 job (or (and (listp result) (plist-get result :error))
                         "Cache worker returned a malformed result"))
                (org-files-db-cache--debug
                 "worker result rejected view=%s target=%s token=%s reason=control"
                 (org-files-db-cache--job-view-name job)
                 (org-files-db-cache--job-target-generation job)
                 (org-files-db-cache--job-refresh-token job))
                (when (and state
                           (eq (org-files-db-cache--job-refresh-type job) 'patch))
                  (org-files-db-cache--request-full-refresh
                   view (org-files-db-cache--job-config-file job)
                   state nil)))
               (t
                (condition-case build-error
                    (let* ((read-started-at (float-time))
                           (payload
                            (org-files-db-cache--read-data-file
                             (org-files-db-cache--job-result-file job)))
                           (read-seconds
                            (org-files-db-cache--elapsed read-started-at)))
                      (unless (org-files-db-cache--worker-payload-valid-p
                               job payload)
                        (signal 'org-files-db-error
                                (list "Cache worker payload metadata is invalid")))
                      (let* ((results
                              (org-files-db-cache--complete-worker-results
                               job payload))
                             (columns
                              (org-files-db--normalize-columns
                               (plist-get payload :columns)))
                             (publication-started-at (float-time))
                             (presentation
                              (org-files-db-cache--prepare results columns))
                             (publication-seconds
                              (org-files-db-cache--elapsed
                               publication-started-at))
                             (result-count (length results)))
                        (org-files-db-cache--debug
                         (concat "worker finished view=%s target=%s token=%s "
                                 "worker=%.3fs write=%.3fs bytes=%d "
                                 "read=%.3fs publish=%.3fs total=%.3fs")
                         (org-files-db-cache--job-view-name job)
                         (org-files-db-cache--job-target-generation job)
                         (org-files-db-cache--job-refresh-token job)
                         (or (plist-get result :worker-seconds) 0.0)
                         (or (plist-get result :payload-write-seconds) 0.0)
                         (plist-get result :payload-bytes)
                         read-seconds publication-seconds
                         (or (org-files-db-cache--elapsed
                              (org-files-db-cache--job-started-at job))
                             0.0))
                        (if (not (org-files-db-cache--entry-eligible-p
                                  result-count))
                            (progn
                              (org-files-db-cache--record-result-limit-skip
                               view (org-files-db-cache--job-config-file job)
                               state result-count)
                              (org-files-db-cache--retire-obsolete-view-entry
                               (org-files-db-cache--job-view-name job)))
                          (org-files-db-cache--publish-worker-presentation
                           job view results columns presentation))))
                  (error
                   (org-files-db-cache--record-failure
                    job (error-message-string build-error))
                   (when (and state
                              (eq (org-files-db-cache--job-refresh-type job)
                                  'patch))
                     (org-files-db-cache--request-full-refresh
                      view (org-files-db-cache--job-config-file job)
                      state nil)))))))
          (error
           (org-files-db-cache--record-failure
            job (error-message-string err))))
      (org-files-db-cache--cleanup-job-transport job)
      (org-files-db-cache--start-next-worker))))

(defun org-files-db-cache--request-full-refresh
    (view config-file state explicit-p)
  "Enqueue a complete VIEW refresh for CONFIG-FILE at STATE.
EXPLICIT-P records whether the user explicitly requested the refresh."
  (let* ((name (car view))
         (job
          (make-org-files-db-cache--job
           :view-name name
           :view-token (org-files-db-cache--view-token view config-file)
           :cache-key (org-files-db-cache--cache-key view config-file state)
           :command (org-files-db-cache--view-command view)
           :config-file config-file
           :database-id (plist-get state :database-id)
           :source-generation
           (when-let* ((entry (org-files-db-cache--entry-for-view name)))
             (org-files-db-cache--entry-generation entry))
           :target-generation (plist-get state :generation)
           :refresh-type 'full
           :explicit-p explicit-p)))
    (or (org-files-db-cache--equivalent-job job)
        (progn
          (setf (org-files-db-cache--job-refresh-token job)
                (org-files-db-cache--next-refresh-token name))
          (org-files-db-cache--enqueue-job job)))))

(defun org-files-db-cache--request-patch
    (view config-file state entry changes explicit-p)
  "Enqueue a CONFIG-FILE patch for VIEW and ENTRY at STATE using CHANGES.
EXPLICIT-P records whether the user explicitly requested the refresh."
  (let* ((name (car view))
         (job
          (make-org-files-db-cache--job
           :view-name name
           :view-token (org-files-db-cache--view-token view config-file)
           :cache-key (org-files-db-cache--cache-key view config-file state)
           :command 'query
           :config-file config-file
           :database-id (plist-get state :database-id)
           :source-generation (org-files-db-cache--entry-generation entry)
           :target-generation (plist-get state :generation)
           :refresh-type 'patch
           :upsert-files (plist-get changes :upsert-files)
           :deleted-files (plist-get changes :deleted-files)
           :explicit-p explicit-p)))
    (or (org-files-db-cache--equivalent-job job)
        (progn
          (setf (org-files-db-cache--job-refresh-token job)
                (org-files-db-cache--next-refresh-token name))
          (org-files-db-cache--enqueue-job job)))))

(defun org-files-db-cache--request-refresh
    (view config-file explicit-p &optional known-state)
  "Request an asynchronous refresh for pre-cached VIEW.
CONFIG-FILE is effective and EXPLICIT-P bypasses failure backoff.
KNOWN-STATE avoids a duplicate status command after an authoritative check."
  (when (and org-files-db-cache-mode
             (org-files-db-cache--view-pre-cache-p view)
             (or known-state explicit-p
                 (not (org-files-db-cache--status-backoff-p config-file))))
    (let (state)
      (condition-case err
          (progn
            (setq state (or known-state
                            (org-files-db-cache--read-index-state config-file)))
            (let* ((name (car view))
                   (entry (org-files-db-cache--entry-for-view name))
                   (key (org-files-db-cache--cache-key view config-file state))
                   (view-token (org-files-db-cache--view-token view config-file))
                   (failure-key
                    (org-files-db-cache--failure-key view-token state))
                   (skip-key (org-files-db-cache--skip-key view-token state)))
              (cond
               ((and (not explicit-p)
                     (or (gethash failure-key org-files-db-cache--failures)
                         (gethash skip-key org-files-db-cache--skipped)))
                nil)
               ((org-files-db-cache--entry-current-p entry state key)
                entry)
               ((org-files-db-cache--source-dirty-at-state-p entry state)
                nil)
               ((or (null entry)
                    (not (equal (org-files-db-cache--entry-key entry) key))
                    (not (equal
                          (org-files-db-cache--entry-database-id entry)
                          (plist-get state :database-id)))
                    (not (org-files-db-cache--patch-safe-p view entry)))
                (org-files-db-cache--request-full-refresh
                 view config-file state explicit-p))
               (t
                (let ((changes
                       (org-files-db-cache--read-changes
                        config-file
                        (org-files-db-cache--entry-database-id entry)
                        (org-files-db-cache--entry-generation entry))))
                  (unless (= (plist-get changes :to-generation)
                             (plist-get state :generation))
                    (signal 'org-files-db-error
                            (list "Changes response does not reach current generation")))
                  (pcase (plist-get changes :cache-action)
                    ('unchanged
                     (unless (org-files-db-cache--entry-source-dirty-p entry)
                       (setf (org-files-db-cache--entry-generation entry)
                             (plist-get state :generation)
                             (org-files-db-cache--entry-stale-p entry) nil)
                       (org-files-db-cache--touch-entry entry)))
                    ('patch
                     (org-files-db-cache--request-patch
                      view config-file state entry changes explicit-p))
                    (_
                     (org-files-db-cache--request-full-refresh
                      view config-file state explicit-p))))))))
        (error
         (if state
             (org-files-db-cache--record-refresh-failure
              view config-file state (error-message-string err))
           (org-files-db-cache--record-status-failure
            config-file (error-message-string err)))
         (when explicit-p
           (message "View `%s' cache refresh failed: %s"
                    (car view) (error-message-string err)))
         nil)))))

(defun org-files-db-cache--cancel-worker (process)
  "Cancel async cache PROCESS and release its main temporary buffer."
  (when (processp process)
    (let ((buffer (process-buffer process)))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun org-files-db-cache--obsolete-view-refresh (name)
  "Obsolete queued and running refreshes for predefined view NAME."
  (org-files-db-cache--next-refresh-token name)
  (let (retained)
    (dolist (job org-files-db-cache--queue)
      (if (equal (org-files-db-cache--job-view-name job) name)
          (org-files-db-cache--cleanup-job-transport job)
        (push job retained)))
    (setq org-files-db-cache--queue (nreverse retained)))
  (when (and org-files-db-cache--current-job
             (equal (org-files-db-cache--job-view-name
                     org-files-db-cache--current-job)
                    name))
    (let ((job org-files-db-cache--current-job)
          (worker org-files-db-cache--current-worker))
      (setq org-files-db-cache--current-worker nil
            org-files-db-cache--current-job nil)
      (org-files-db-cache--cleanup-job-transport job)
      (org-files-db-cache--cancel-worker worker))
    (org-files-db-cache--start-next-worker)))

(defun org-files-db-cache-refresh-view (view config-file &optional synchronous)
  "Refresh pre-cached VIEW using CONFIG-FILE.
When SYNCHRONOUS is non-nil, perform a complete replacement immediately."
  (unless org-files-db-cache-mode
    (user-error "org-files-db-cache-mode is not enabled"))
  (unless (org-files-db-cache--view-pre-cache-p view)
    (user-error "View `%s' does not enable :pre-cache" (car view)))
  (let* ((state (org-files-db-cache--read-index-state config-file))
         (entry (org-files-db-cache--entry-for-view (car view))))
    (when (org-files-db-cache--source-dirty-at-state-p entry state)
      (user-error
       "View `%s' has source changes awaiting an orgfdb generation update"
       (car view)))
    (org-files-db-cache--obsolete-view-refresh (car view))
    (if synchronous
        (let ((built
               (org-files-db-cache--build-sync view config-file state t)))
          (if (plist-get built :entry)
              (message "Refreshed cache for view `%s'" (car view))
            (message "View `%s' was refreshed but not retained"
                     (car view))))
      (org-files-db-cache--request-full-refresh
       view config-file state t)
      (message "Refreshing cache for view `%s'" (car view)))))

(defun org-files-db-cache-clear-view (name)
  "Clear prepared cache and obsolete refreshes for predefined view NAME."
  (org-files-db-cache--obsolete-view-refresh name)
  (org-files-db-cache--remove-view-entry name)
  ;; Backoff keys are definition hashes rather than names; clearing them is
  ;; conservative and lets explicit cache management retry any current view.
  (clrhash org-files-db-cache--skipped)
  (clrhash org-files-db-cache--failures)
  (message "Cleared cache for view `%s'" name))

(defun org-files-db-cache-clear-all ()
  "Clear all prepared entries, jobs, failures, and automatic skip records."
  (let ((job org-files-db-cache--current-job)
        (worker org-files-db-cache--current-worker))
    (setq org-files-db-cache--current-worker nil
          org-files-db-cache--current-job nil)
    (org-files-db-cache--cleanup-job-transport job)
    (org-files-db-cache--cancel-worker worker))
  (dolist (job org-files-db-cache--queue)
    (org-files-db-cache--cleanup-job-transport job))
  (setq org-files-db-cache--current-worker nil
        org-files-db-cache--current-job nil
        org-files-db-cache--queue nil
        org-files-db-cache--entry-sequence 0)
  (clrhash org-files-db-cache--entries)
  (clrhash org-files-db-cache--view-keys)
  (clrhash org-files-db-cache--skipped)
  (clrhash org-files-db-cache--failures)
  (clrhash org-files-db-cache--status-failures)
  (clrhash org-files-db-cache--notification-times)
  (clrhash org-files-db-cache--refresh-tokens)
  (message "Cleared all org-files-db view caches"))

(defun org-files-db-cache--prune-unconfigured ()
  "Remove entries whose view disappeared, changed, or disabled pre-caching."
  (let (remove)
    (maphash
     (lambda (_key entry)
       (let ((view
              (org-files-db-cache--current-view
               (org-files-db-cache--entry-view-name entry))))
         (unless
             (and view
                  (org-files-db-cache--view-pre-cache-p view)
                  (condition-case nil
                      (equal
                       (org-files-db-cache--view-token
                        view (org-files-db-cache--view-config-file view))
                       (org-files-db-cache--entry-view-token entry))
                    (error nil)))
           (push entry remove))))
     org-files-db-cache--entries)
    (dolist (entry remove)
      (if (org-files-db-cache--entry-in-use-p entry)
          (progn
            (setf (org-files-db-cache--entry-stale-p entry) t)
            (when (equal
                   (gethash (org-files-db-cache--entry-view-name entry)
                            org-files-db-cache--view-keys)
                   (org-files-db-cache--entry-table-key entry))
              (remhash (org-files-db-cache--entry-view-name entry)
                       org-files-db-cache--view-keys)))
        (org-files-db-cache--remove-entry entry)))
    (org-files-db-cache--prune-watchers)))

(defun org-files-db-cache--configured-pre-cache-views ()
  "Return copies of all configured views opting into pre-caching."
  (let (views)
    (dolist (view (and (boundp 'org-files-db-views) org-files-db-views))
      (when (org-files-db-cache--view-pre-cache-p view)
        (push (copy-tree view) views)))
    (nreverse views)))

(defun org-files-db-cache--view-config-file (view)
  "Return effective configuration file for VIEW without loading the view module."
  (let ((properties (cdr view)))
    (org-files-db--resolve-config-file
     (plist-get properties :config-file)
     (not (null (plist-member properties :config-file)))
     (format "View `%s'" (car view)))))

(defun org-files-db-cache-refresh-all (&optional synchronous)
  "Refresh every configured pre-cached view.
When SYNCHRONOUS is non-nil, perform complete refreshes serially."
  (unless org-files-db-cache-mode
    (user-error "org-files-db-cache-mode is not enabled"))
  (dolist (view (org-files-db-cache--configured-pre-cache-views))
    (condition-case err
        (org-files-db-cache-refresh-view
         view (org-files-db-cache--view-config-file view) synchronous)
      (error
       (message "View `%s' cache refresh failed: %s"
                (car view) (error-message-string err))))))

(defun org-files-db-cache--mark-source-mutated (config-file _paths)
  "Mark entries for effective CONFIG-FILE stale after source mutation."
  (maphash
   (lambda (_key entry)
     (when (equal config-file
                  (org-files-db-cache--entry-config-file entry))
       (setf (org-files-db-cache--entry-source-dirty-p entry) t
             (org-files-db-cache--entry-stale-p entry) t)))
   org-files-db-cache--entries))

(defun org-files-db-cache--handle-state-change (config-file old-state new-state)
  "Mark caches stale when CONFIG-FILE moves from OLD-STATE to NEW-STATE.
A generation change invalidates every cache sharing that database identity.
When the database identity itself changes, only caches using CONFIG-FILE are
invalidated; another configuration may still point to the old database."
  (unless (org-files-db-cache--same-state-p old-state new-state)
    (let* ((old-id (plist-get old-state :database-id))
           (new-id (plist-get new-state :database-id))
           (same-database-p (and old-id new-id (equal old-id new-id))))
      (maphash
       (lambda (_key entry)
         (when (or (equal config-file
                          (org-files-db-cache--entry-config-file entry))
                   (and same-database-p
                        (equal (org-files-db-cache--entry-database-id entry)
                               old-id)))
           (setf (org-files-db-cache--entry-stale-p entry) t)))
       org-files-db-cache--entries))))

(defun org-files-db-cache--ensure-config-views (config-file state)
  "Ensure affected pre-cached views have replacements for index STATE.
Always include views using CONFIG-FILE.  Also include existing entries sharing
STATE's database identity, even when another configuration observed the change."
  (dolist (view (org-files-db-cache--configured-pre-cache-views))
    (condition-case nil
        (let* ((view-config (org-files-db-cache--view-config-file view))
               (entry (org-files-db-cache--entry-for-view (car view))))
          (when (or (equal view-config config-file)
                    (and entry
                         (equal (org-files-db-cache--entry-database-id entry)
                                (plist-get state :database-id))))
            (org-files-db-cache--request-refresh
             view view-config nil state)))
      (error nil))))

(defun org-files-db-cache--check-config (config-file)
  "Check CONFIG-FILE state and schedule required cache refreshes."
  (unless (org-files-db-cache--status-backoff-p config-file)
    (let* ((key (org-files-db-cache--config-key config-file))
           (old-state (gethash key org-files-db-cache--index-states))
           (new-state
            (condition-case err
                (org-files-db-cache--read-index-state config-file)
              (error
               (org-files-db-cache--record-status-failure
                config-file (error-message-string err))
               nil))))
      (when new-state
        (org-files-db-cache--handle-state-change
         config-file old-state new-state)
        (org-files-db-cache--ensure-config-views config-file new-state)))))

(defun org-files-db-cache--config-watched-p (config-file)
  "Return non-nil when CONFIG-FILE has an active database watcher."
  (let ((config-key (org-files-db-cache--config-key config-file))
        found)
    (maphash
     (lambda (key _descriptor)
       (when (equal (car key) config-key)
         (setq found t)))
     org-files-db-cache--watchers)
    found))

(defun org-files-db-cache--poll ()
  "Poll unwatched configured pre-cache databases for authoritative state."
  (when org-files-db-cache-mode
    (org-files-db-cache--prune-unconfigured)
    (let ((seen (make-hash-table :test #'equal)))
      (dolist (view (org-files-db-cache--configured-pre-cache-views))
        (condition-case nil
            (let* ((config-file (org-files-db-cache--view-config-file view))
                   (config-key (org-files-db-cache--config-key config-file)))
              (unless (or (gethash config-key seen)
                          (org-files-db-cache--config-watched-p config-file))
                (puthash config-key t seen)
                (org-files-db-cache--check-config config-file)))
          (error nil))))))

(defun org-files-db-cache--initial-pre-cache ()
  "Immediately queue all configured pre-cached views in definition order."
  (when org-files-db-cache-mode
    (org-files-db-cache--prune-unconfigured)
    (let ((states (make-hash-table :test #'equal)))
      (dolist (view (org-files-db-cache--configured-pre-cache-views))
        (condition-case nil
            (let* ((config-file (org-files-db-cache--view-config-file view))
                   (config-key (org-files-db-cache--config-key config-file))
                   (known (gethash config-key states :missing))
                   (state
                    (if (eq known :missing)
                        (let ((fresh
                               (org-files-db-cache--read-index-state
                                config-file)))
                          (puthash config-key fresh states)
                          fresh)
                      known)))
              (org-files-db-cache--request-refresh
               view config-file nil state))
          (error nil))))))

(defun org-files-db-cache--sqlite-event-p (event database-path directory)
  "Return non-nil when file-notify EVENT concerns DATABASE-PATH.
DIRECTORY is the watched database location used for relative notification paths."
  (let* ((files (delq nil (list (nth 2 event) (nth 3 event))))
         (database-path (expand-file-name database-path)))
    (seq-some
     (lambda (file)
       (when (stringp file)
         (let ((expanded (expand-file-name file directory)))
           (or (equal expanded database-path)
               (equal expanded (concat database-path "-wal"))
               (equal expanded (concat database-path "-shm"))))))
     files)))

(defun org-files-db-cache--debounced-check (config-file)
  "Schedule one debounced authoritative check for CONFIG-FILE."
  (when org-files-db-cache-mode
    (let ((key (org-files-db-cache--config-key config-file)))
      (unless (timerp (gethash key org-files-db-cache--debounce-timers))
        (puthash key (float-time) org-files-db-cache--notification-times)
        (org-files-db-cache--debug
         "file notify received config=%s" config-file)
        (puthash
         key
         (run-at-time
          org-files-db-views-pre-cache-file-notify-debounce nil
          (lambda ()
            (remhash key org-files-db-cache--debounce-timers)
            (when org-files-db-cache-mode
              (org-files-db-cache--check-config config-file))))
         org-files-db-cache--debounce-timers)))))

(defun org-files-db-cache--remove-watcher (key)
  "Remove file-notify watcher identified by KEY."
  (when-let* ((descriptor (gethash key org-files-db-cache--watchers)))
    (when (fboundp 'file-notify-rm-watch)
      (condition-case nil
          (file-notify-rm-watch descriptor)
        (error nil)))
    (remhash key org-files-db-cache--watchers)))

(defun org-files-db-cache--prune-watchers ()
  "Remove watchers and debounce timers for inactive configurations or paths."
  (let ((active (make-hash-table :test #'equal))
        remove)
    (dolist (view (org-files-db-cache--configured-pre-cache-views))
      (condition-case nil
          (let ((config-key
                 (org-files-db-cache--config-key
                  (org-files-db-cache--view-config-file view))))
            (puthash config-key t active))
        (error nil)))
    (maphash
     (lambda (key _descriptor)
       (let* ((config-key (car key))
              (database-path (cadr key))
              (state (gethash config-key org-files-db-cache--index-states)))
         (unless (and (gethash config-key active)
                      (equal database-path
                             (plist-get state :database-path)))
           (push key remove))))
     org-files-db-cache--watchers)
    (dolist (key remove)
      (org-files-db-cache--remove-watcher key))
    (let (inactive-timers)
      (maphash
       (lambda (config-key timer)
         (unless (gethash config-key active)
           (push (cons config-key timer) inactive-timers)))
       org-files-db-cache--debounce-timers)
      (dolist (entry inactive-timers)
        (when (timerp (cdr entry))
          (cancel-timer (cdr entry)))
        (remhash (car entry) org-files-db-cache--debounce-timers)))))

(defun org-files-db-cache--ensure-watcher (config-file state)
  "Install a database-directory watcher for CONFIG-FILE and index STATE."
  (when org-files-db-cache-mode
    (when-let* ((database-path (plist-get state :database-path))
                ((file-name-absolute-p database-path))
                ((not (file-remote-p database-path)))
                ((fboundp 'file-notify-add-watch))
                ((file-directory-p (file-name-directory database-path))))
      (let* ((directory (file-name-directory database-path))
             (config-key (org-files-db-cache--config-key config-file))
             (key (list config-key database-path))
             stale)
        (maphash
         (lambda (existing-key _descriptor)
           (when (and (equal (car existing-key) config-key)
                      (not (equal existing-key key)))
             (push existing-key stale)))
         org-files-db-cache--watchers)
        (dolist (existing-key stale)
          (org-files-db-cache--remove-watcher existing-key))
        (unless (gethash key org-files-db-cache--watchers)
          (condition-case nil
              (let ((descriptor
                     (file-notify-add-watch
                      directory
                      '(change attribute-change)
                      (lambda (event)
                        (if (eq (cadr event) 'stopped)
                            (progn
                              (remhash key org-files-db-cache--watchers)
                              (org-files-db-cache--debounced-check config-file))
                          (when (org-files-db-cache--sqlite-event-p
                                 event database-path directory)
                            (org-files-db-cache--debounced-check
                             config-file)))))))
                (puthash key descriptor org-files-db-cache--watchers)
                (org-files-db-cache--debug
                 "watch installed config=%s path=%s" config-file database-path))
            (error nil)))))))

(defun org-files-db-cache--cancel-runtime ()
  "Cancel active workers, timers, notifications, and queued cache work."
  (let ((job org-files-db-cache--current-job)
        (worker org-files-db-cache--current-worker))
    (setq org-files-db-cache--current-worker nil
          org-files-db-cache--current-job nil)
    (org-files-db-cache--cleanup-job-transport job)
    (org-files-db-cache--cancel-worker worker))
  (dolist (job org-files-db-cache--queue)
    (org-files-db-cache--cleanup-job-transport job))
  (setq org-files-db-cache--queue nil)
  (when (timerp org-files-db-cache--poll-timer)
    (cancel-timer org-files-db-cache--poll-timer))
  (setq org-files-db-cache--poll-timer nil)
  (maphash
   (lambda (_key timer)
     (when (timerp timer)
       (cancel-timer timer)))
   org-files-db-cache--debounce-timers)
  (clrhash org-files-db-cache--debounce-timers)
  (clrhash org-files-db-cache--notification-times)
  (let (watcher-keys)
    (maphash
     (lambda (key _descriptor)
       (push key watcher-keys))
     org-files-db-cache--watchers)
    (dolist (key watcher-keys)
      (org-files-db-cache--remove-watcher key))))

(defun org-files-db-cache--clear-runtime-cache ()
  "Release all prepared cache state without user-facing messages."
  (setq org-files-db-cache--entry-sequence 0)
  (clrhash org-files-db-cache--entries)
  (clrhash org-files-db-cache--view-keys)
  (clrhash org-files-db-cache--index-states)
  (clrhash org-files-db-cache--skipped)
  (clrhash org-files-db-cache--failures)
  (clrhash org-files-db-cache--status-failures)
  (clrhash org-files-db-cache--notification-times)
  (clrhash org-files-db-cache--refresh-tokens))

(defun org-files-db-cache--enable-mode ()
  "Start cache monitoring and immediately pre-cache opted-in views."
  (org-files-db-cache--debug "cache-mode enabled")
  (add-hook 'org-files-db--source-mutated-hook
            #'org-files-db-cache--mark-source-mutated)
  (when (and (numberp org-files-db-views-pre-cache-poll-interval)
             (> org-files-db-views-pre-cache-poll-interval 0))
    (setq org-files-db-cache--poll-timer
          (run-at-time
           org-files-db-views-pre-cache-poll-interval
           org-files-db-views-pre-cache-poll-interval
           #'org-files-db-cache--poll)))
  (org-files-db-cache--initial-pre-cache))

(defun org-files-db-cache--disable-mode ()
  "Stop cache monitoring and release all prepared cache state."
  (org-files-db-cache--debug "cache-mode disabled")
  (remove-hook 'org-files-db--source-mutated-hook
               #'org-files-db-cache--mark-source-mutated)
  (org-files-db-cache--cancel-runtime)
  (org-files-db-cache--clear-runtime-cache))

;;;###autoload
(define-minor-mode org-files-db-cache-mode
  "Toggle generation-aware pre-caching for predefined org-files-db views.
This is a global minor mode.  Enabling it immediately queues every predefined
view marked with :pre-cache t, installs file notifications for databases whose
canonical SQLite path is available, and optionally installs fallback polling.
Disabling it stops all cache workers and notifications and releases the
in-memory cache."
  :global t
  :group 'org-files-db
  :lighter nil
  (if org-files-db-cache-mode
      (org-files-db-cache--enable-mode)
    (org-files-db-cache--disable-mode)))

(provide 'org-files-db-cache)

;;; org-files-db-cache.el ends here
