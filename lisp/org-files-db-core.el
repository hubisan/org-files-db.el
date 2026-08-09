;;; org-files-db-core.el --- Core support for org-files-db -*- lexical-binding: t; -*-

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

;; Shared configuration, process execution, result handling, formatting, and
;; completion support for org-files-db.el.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-id)
(require 'org-element)
(require 'seq)
(require 'subr-x)

(defvar read-eval)

(declare-function org-files-db-actions-open-result
                  "org-files-db-actions"
                  (result))

(defgroup org-files-db nil
  "Emacs interface for org-files-db."
  :group 'org
  :prefix "org-files-db-")

(defcustom org-files-db-executable "orgfdb"
  "Path or command name of the orgfdb executable."
  :type 'string
  :group 'org-files-db)

(defcustom org-files-db-config-file nil
  "Default configuration file passed to orgfdb.
Commands and views may override this value for one invocation.  When nil,
do not pass the --config option unless a command-specific path is supplied."
  :type '(choice
          (const :tag "No explicit configuration" nil)
          file)
  :group 'org-files-db)

(defcustom org-files-db-heading-columns
  '((todo-keyword :width (max 10))
    (priority :width (fixed 3))
    (outline-path :width (max 80))
    (file-name :width (max 30)))
  "Default columns for heading query results."
  :type '(repeat sexp)
  :group 'org-files-db)

(defcustom org-files-db-file-columns
  '((file-title :width (max 50))
    (file-path :width (max 100)))
  "Default columns for file query results."
  :type '(repeat sexp)
  :group 'org-files-db)

(defcustom org-files-db-link-columns
  '((link-type :width (max 12))
    (link-target :width (max 60))
    (source-outline-path :width (max 70))
    (file-name :width (max 30)))
  "Default columns for link query results."
  :type '(repeat sexp)
  :group 'org-files-db)

(defcustom org-files-db-outline-path-separator " » "
  "Default separator between outline-path components."
  :type 'string
  :group 'org-files-db)

(defcustom org-files-db-outline-path-include-root nil
  "Non-nil means outline-path columns include their file root by default."
  :type 'boolean
  :group 'org-files-db)

(defcustom org-files-db-outline-path-include-match t
  "Non-nil means outline-path columns include their final heading by default."
  :type 'boolean
  :group 'org-files-db)

(defcustom org-files-db-truncate-position 'right
  "Default position at which displayed column values are truncated."
  :type '(choice (const left) (const middle) (const right))
  :group 'org-files-db)

(defcustom org-files-db-truncate-marker "…"
  "Default marker inserted when a displayed column value is truncated."
  :type 'string
  :group 'org-files-db)

(defcustom org-files-db-search-columns
  '((title :width (max 70))
    (file-name :width (max 30))
    (line-number :width auto))
  "Default columns for full-text search results."
  :type '(repeat sexp)
  :group 'org-files-db)

(defcustom org-files-db-heading-sort nil
  "Default sorting for heading query results."
  :type '(repeat sexp)
  :group 'org-files-db)

(defcustom org-files-db-file-sort nil
  "Default sorting for file query results."
  :type '(repeat sexp)
  :group 'org-files-db)

(defcustom org-files-db-link-sort nil
  "Default sorting for link query results."
  :type '(repeat sexp)
  :group 'org-files-db)

(defcustom org-files-db-search-sort nil
  "Default sorting for full-text search results."
  :type '(repeat sexp)
  :group 'org-files-db)

(defcustom org-files-db-query-action #'org-files-db-actions-open-result
  "Function called with the selected query or search result."
  :type 'function
  :group 'org-files-db)

(defcustom org-files-db-export-layout 'flat
  "Default layout used by the Embark Org exporter."
  :type '(choice (const flat) (const outline))
  :group 'org-files-db)

(defcustom org-files-db-export-linked-heading-style 'preserve
  "How linked heading titles are represented in Org exports."
  :type '(choice (const preserve) (const resolve))
  :group 'org-files-db)

(defcustom org-files-db-file-link-style 'file
  "Default link style for file query results."
  :type '(choice (const file) (const id) (const custom-id))
  :group 'org-files-db)

(defcustom org-files-db-heading-link-style 'id
  "Default link style for heading query results."
  :type '(choice (const id) (const custom-id) (const heading))
  :group 'org-files-db)

(defcustom org-files-db-search-min-input 3
  "Minimum number of characters required before live search starts."
  :type 'natnum
  :group 'org-files-db)

(defcustom org-files-db-views nil
  "Named predefined query and search views.
Each entry has the form (NAME :command COMMAND ...), where COMMAND is
`query' or `search'.  An optional :config-file overrides
`org-files-db-config-file'; an explicit nil disables --config for that view.
An optional :sort overrides the configured default sorting; an explicit nil
preserves the order returned by orgfdb.
An optional :pre-cache t marks the view as eligible for asynchronous
in-memory caching while `org-files-db-cache-mode' is enabled."
  :type '(repeat sexp)
  :group 'org-files-db)

(defcustom org-files-db-cache-file-notify-debounce 0.20
  "Seconds to coalesce SQLite file-notify activity before one status check.
This timer exists only after relevant file-notify activity.  It is not a
polling interval and no periodic cache timer is installed."
  :type 'number
  :group 'org-files-db)

(defcustom org-files-db-cache-wait-timeout 60.0
  "Maximum seconds an interactive cached view waits for its async rebuild."
  :type 'number
  :group 'org-files-db)

(defcustom org-files-db-cache-debug nil
  "Non-nil means emit concise predefined-view cache diagnostics."
  :type 'boolean
  :group 'org-files-db)

(define-error 'org-files-db-error "org-files-db error")
(define-error 'org-files-db-cli-error "orgfdb command failed" 'org-files-db-error)
(define-error 'org-files-db-cli-usage-error
              "Invalid orgfdb command usage" 'org-files-db-cli-error)

(defconst org-files-db--completion-category 'org-files-db-result)

(defconst org-files-db--result-config-key 'org-files-db--config-file
  "Internal result key containing the effective orgfdb configuration file.")

(defvar org-files-db-query-history nil
  "Minibuffer history for orgfdb query expressions.")

(defvar org-files-db--source-mutated-hook nil
  "Hook run after package-controlled Org source mutations.
Each function receives the effective configuration file and a list of affected
paths.  A nil configuration identifies an invocation without --config.")

(defconst org-files-db--missing-key (make-symbol "org-files-db-missing-key")
  "Sentinel used to distinguish missing JSON object keys from nil values.")

(defun org-files-db--alist-value (object key)
  "Return from OBJECT the value associated with KEY.
OBJECT may be an alist or hash table, and its keys may be symbols or strings."
  (cond
   ((hash-table-p object)
    (if (symbolp key)
        (let ((value (gethash key object org-files-db--missing-key)))
          (if (eq value org-files-db--missing-key)
              (gethash (symbol-name key) object)
            value))
      (let* ((symbol-key (intern key))
             (value (gethash symbol-key object org-files-db--missing-key)))
        (if (eq value org-files-db--missing-key)
            (gethash key object)
          value))))
   ((listp object)
    (if (symbolp key)
        (let ((entry (assq key object)))
          (if entry
              (cdr entry)
            (cdr (assoc (symbol-name key) object #'string=))))
      (let* ((symbol-key (intern key))
             (entry (assq symbol-key object)))
        (if entry
            (cdr entry)
          (cdr (assoc key object #'string=))))))
   (t nil)))

(defun org-files-db--has-key-p (object key)
  "Return non-nil when OBJECT contains KEY."
  (cond
   ((hash-table-p object)
    (if (symbolp key)
        (or (not (eq (gethash key object org-files-db--missing-key)
                     org-files-db--missing-key))
            (not (eq (gethash (symbol-name key) object
                              org-files-db--missing-key)
                     org-files-db--missing-key)))
      (let ((symbol-key (intern key)))
        (or (not (eq (gethash symbol-key object
                              org-files-db--missing-key)
                     org-files-db--missing-key))
            (not (eq (gethash key object org-files-db--missing-key)
                     org-files-db--missing-key))))))
   ((listp object)
    (if (symbolp key)
        (or (assq key object)
            (assoc (symbol-name key) object #'string=))
      (or (assq (intern key) object)
          (assoc key object #'string=))))
   (t nil)))

(defun org-files-db--get (object &rest keys)
  "Return the nested value in OBJECT selected by KEYS."
  (dolist (key keys object)
    (setq object (org-files-db--alist-value object key))))

(cl-define-compiler-macro org-files-db--get (&whole _form object &rest keys)
  "Compile static KEYS in FORM without allocating a rest argument list."
  (if (null keys)
      object
    (let ((expression object))
      (dolist (key keys expression)
        (setq expression
              `(org-files-db--alist-value ,expression ,key))))))

(defun org-files-db--kind (result)
  "Return RESULT kind as a symbol."
  (let ((kind (org-files-db--get result 'kind)))
    (cond
     ((symbolp kind) kind)
     ((stringp kind) (intern kind))
     (t nil))))

(defun org-files-db--resolve-executable ()
  "Return the absolute path to `org-files-db-executable'."
  (unless (and (stringp org-files-db-executable)
               (not (string-empty-p org-files-db-executable)))
    (user-error "Org-files-db-executable must be a non-empty string"))
  (let ((path
         (if (file-name-directory org-files-db-executable)
             (expand-file-name org-files-db-executable)
           (executable-find org-files-db-executable))))
    (unless path
      (user-error "Cannot find orgfdb executable `%s'"
                  org-files-db-executable))
    (unless (file-executable-p path)
      (user-error "Orgfdb executable is not executable: %s" path))
    path))

(defun org-files-db--config-description (origin)
  "Return a configuration description for optional ORIGIN."
  (if origin
      (format "%s configuration file" origin)
    "Orgfdb configuration file"))

(defun org-files-db--resolve-config-file (config-file supplied-p &optional origin)
  "Return the effective expanded configuration file for CONFIG-FILE.
When SUPPLIED-P is non-nil, CONFIG-FILE overrides
`org-files-db-config-file', including when it is nil.  Otherwise inherit the
global value.  ORIGIN identifies the command or view in validation errors."
  (let* ((value (if supplied-p config-file org-files-db-config-file))
         (description (org-files-db--config-description origin)))
    (when value
      (unless (and (stringp value) (not (string-empty-p value)))
        (user-error "%s must be a non-empty file name or nil" description))
      (let ((file (expand-file-name value)))
        (unless (file-exists-p file)
          (user-error "%s does not exist: %s" description file))
        (unless (file-regular-p file)
          (user-error "%s is not a regular file: %s" description file))
        (unless (file-readable-p file)
          (user-error "%s is not readable: %s" description file))
        file))))

(cl-defun org-files-db--config-arguments
    (&optional (config-file nil config-file-supplied-p) origin)
  "Return orgfdb arguments for the effective CONFIG-FILE.
An omitted CONFIG-FILE inherits `org-files-db-config-file'; an explicitly
supplied nil disables --config.  ORIGIN identifies validation errors."
  (when-let* ((file (org-files-db--resolve-config-file
                     config-file config-file-supplied-p origin)))
    (list "--config" file)))

(defun org-files-db--buffer-string (buffer)
  "Return BUFFER contents without text properties."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-files-db--cleanup-process-buffers (process)
  "Kill temporary buffers associated with PROCESS."
  (dolist (buffer (process-get process 'org-files-db-buffers))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (process-put process 'org-files-db-buffers nil))

(defun org-files-db--run-process (arguments &optional input)
  "Run orgfdb synchronously with ARGUMENTS and optional standard INPUT.
Return a plist containing :status, :stdout, and :stderr."
  (let* ((program (org-files-db--resolve-executable))
         (stdout (generate-new-buffer " *org-files-db-stdout*"))
         (stderr (generate-new-buffer " *org-files-db-stderr*"))
         process)
    (unwind-protect
        (progn
          (setq process
                (make-process
                 :name "org-files-db"
                 :command (cons program arguments)
                 :buffer stdout
                 :stderr stderr
                 :connection-type 'pipe
                 :coding '(utf-8-unix . utf-8-unix)
                 :noquery t
                 :sentinel #'ignore))
          (when input
            (process-send-string process input)
            (process-send-eof process))
          (while (process-live-p process)
            (accept-process-output process 0.05))
          (list :status (process-exit-status process)
                :stdout (org-files-db--buffer-string stdout)
                :stderr (org-files-db--buffer-string stderr)))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (buffer-live-p stdout)
        (kill-buffer stdout))
      (when (buffer-live-p stderr)
        (kill-buffer stderr)))))

(defun org-files-db--error-message (status stderr)
  "Build an actionable error message from STATUS and STDERR."
  (let ((message (string-trim (or stderr ""))))
    (if (string-empty-p message)
        (format "orgfdb exited with status %d" status)
      (format "orgfdb exited with status %d: %s" status message))))

(defun org-files-db--signal-cli-error (status stderr)
  "Signal an orgfdb error for STATUS and STDERR."
  (let ((data (list (org-files-db--error-message status stderr)
                    status
                    (string-trim (or stderr "")))))
    (signal (if (= status 2)
                'org-files-db-cli-usage-error
              'org-files-db-cli-error)
            data)))

(defun org-files-db--call-raw (arguments &optional input)
  "Run orgfdb with ARGUMENTS and optional standard INPUT, returning stdout."
  (pcase-let* ((result (org-files-db--run-process arguments input))
               (status (plist-get result :status))
               (stdout (plist-get result :stdout))
               (stderr (plist-get result :stderr)))
    (if (zerop status)
        stdout
      (org-files-db--signal-cli-error status stderr))))

(defun org-files-db--parse-json-as (text object-type &optional array-type)
  "Parse JSON TEXT using OBJECT-TYPE and ARRAY-TYPE.
ARRAY-TYPE defaults to `list'."
  (condition-case err
      (json-parse-string text
                         :object-type object-type
                         :array-type (or array-type 'list)
                         :null-object nil
                         :false-object nil)
    (error
     (signal 'org-files-db-error
             (list (format "Invalid JSON from orgfdb: %s"
                           (error-message-string err)))))))

(defun org-files-db--parse-json (text)
  "Parse JSON TEXT into alists with vector arrays."
  (let ((parsed (org-files-db--parse-json-as text 'alist 'array)))
    (if (vectorp parsed) (append parsed nil) parsed)))

(defun org-files-db--call (command arguments &optional input)
  "Run orgfdb COMMAND with ARGUMENTS and optional INPUT, parsing JSON output."
  (org-files-db--parse-json
   (org-files-db--call-raw (cons command arguments) input)))

(defun org-files-db--process-sentinel (process event)
  "Handle completion of asynchronous orgfdb PROCESS with EVENT."
  (when (and (memq (process-status process) '(exit signal))
             (not (process-get process 'org-files-db-handled)))
    (process-put process 'org-files-db-handled t)
    (let ((cancelled (process-get process 'org-files-db-cancelled))
          (callback (process-get process 'org-files-db-callback))
          (status (process-exit-status process))
          (stdout (process-get process 'org-files-db-stdout))
          (stderr (process-get process 'org-files-db-stderr)))
      (unwind-protect
          (unless cancelled
            (let ((out (org-files-db--buffer-string stdout))
                  (err (org-files-db--buffer-string stderr)))
              (if (zerop status)
                  (condition-case parse-error
                      (funcall callback (org-files-db--parse-json out) nil)
                    (error
                     (funcall callback nil
                              (list :status status
                                    :stderr (error-message-string parse-error)
                                    :event event))))
                (funcall callback nil
                         (list :status status
                               :stderr (string-trim (or err ""))
                               :event event)))))
        (org-files-db--cleanup-process-buffers process)))))

(defun org-files-db--start-process (command arguments callback)
  "Start orgfdb COMMAND asynchronously with ARGUMENTS.
CALLBACK is called as (CALLBACK VALUE ERROR).  VALUE is parsed JSON on
success.  ERROR is a plist containing :status and :stderr on failure."
  (let* ((program (org-files-db--resolve-executable))
         (stdout (generate-new-buffer " *org-files-db-async-stdout*"))
         (stderr (generate-new-buffer " *org-files-db-async-stderr*"))
         process)
    (condition-case err
        (setq process
              (make-process
               :name "org-files-db-async"
               :command (append (list program command) arguments)
               :buffer stdout
               :stderr stderr
               :connection-type 'pipe
               :coding '(utf-8-unix . utf-8-unix)
               :noquery t
               :sentinel #'ignore))
      (error
       (when (buffer-live-p stdout)
         (kill-buffer stdout))
       (when (buffer-live-p stderr)
         (kill-buffer stderr))
       (signal (car err) (cdr err))))
    (process-put process 'org-files-db-buffers (list stdout stderr))
    (process-put process 'org-files-db-stdout stdout)
    (process-put process 'org-files-db-stderr stderr)
    (process-put process 'org-files-db-callback callback)
    (set-process-sentinel process #'org-files-db--process-sentinel)
    (when (memq (process-status process) '(exit signal))
      (org-files-db--process-sentinel process "finished"))
    process))

(defun org-files-db--cancel-process (process)
  "Cancel PROCESS and discard its callback."
  (when (processp process)
    (process-put process 'org-files-db-cancelled t)
    (when (process-live-p process)
      (delete-process process))
    (org-files-db--cleanup-process-buffers process)))

(defun org-files-db--read-sexp (prompt)
  "Read one Lisp expression using PROMPT."
  (let* ((input (read-from-minibuffer prompt nil nil nil
                                      'org-files-db-query-history))
         (parsed (condition-case err
                     (let ((read-eval nil))
                       (read-from-string input))
                   (error
                    (user-error "Invalid query expression: %s"
                                (error-message-string err))))))
    (unless (string-empty-p (string-trim (substring input (cdr parsed))))
      (user-error "Query contains trailing input"))
    (car parsed)))

(defun org-files-db--query-string (query)
  "Return QUERY in the textual form accepted by orgfdb."
  (cond
   ((stringp query)
    (if (string-empty-p (string-trim query))
        (user-error "Query must not be empty")
      query))
   ((consp query) (prin1-to-string query))
   (t (user-error "Query must be a list or string"))))

(defun org-files-db--column-name (definition)
  "Return the column name represented by DEFINITION."
  (if (consp definition) (car definition) definition))

(defun org-files-db--column-includes (columns)
  "Return the orgfdb includes required by COLUMNS.
The returned values are symbols in first-use order."
  (let ((normalized (org-files-db--normalize-columns columns))
        includes)
    (cl-loop for column across normalized
             for include = (org-files-db--presentation-column-required-include
                            column)
             when (and include (not (memq include includes)))
             do (setq includes (append includes (list include))))
    includes))

(defun org-files-db--include-arguments (includes)
  "Return orgfdb arguments for INCLUDES."
  (when includes
    (let ((names
           (delete-dups
            (mapcar
             (lambda (include)
               (cond
                ((symbolp include) (symbol-name include))
                ((and (stringp include) (not (string-empty-p include))) include)
                (t (user-error "Invalid orgfdb include: %S" include))))
             includes))))
      (list "--include" (string-join names ",")))))

(cl-defun org-files-db--query-arguments
    (query &optional (config-file nil config-file-supplied-p) origin includes)
  "Return command arguments for QUERY.
An omitted CONFIG-FILE inherits `org-files-db-config-file'; an explicitly
supplied nil disables --config.  ORIGIN identifies validation errors.
INCLUDES lists additional result context requested from orgfdb."
  (let ((effective-config-file
         (org-files-db--resolve-config-file
          config-file config-file-supplied-p origin)))
    (append '("--format" "json" "--output" "flat")
            (org-files-db--include-arguments includes)
            (org-files-db--config-arguments effective-config-file origin)
            (list (org-files-db--query-string query)))))

(cl-defun org-files-db--execute-query
    (query &optional (config-file nil config-file-supplied-p) origin includes)
  "Execute QUERY and return the response envelope.
An omitted CONFIG-FILE inherits `org-files-db-config-file'; an explicitly
supplied nil disables --config.  ORIGIN identifies validation errors.
INCLUDES lists additional result context requested from orgfdb."
  (let ((effective-config-file
         (org-files-db--resolve-config-file
          config-file config-file-supplied-p origin)))
    (org-files-db--call
     "query"
     (org-files-db--query-arguments
      query effective-config-file origin includes))))

(cl-defun org-files-db--execute-query-restricted
    (query files &optional (config-file nil config-file-supplied-p)
           origin includes)
  "Execute QUERY restricted to canonical owning FILES.
CONFIG-FILE, ORIGIN, and INCLUDES have the same meaning as in
`org-files-db--execute-query'.  FILES is encoded as one JSON array on standard
input and does not alter the query expression."
  (unless (and (listp files)
               (seq-every-p
                (lambda (file)
                  (and (stringp file) (not (string-empty-p file))))
                files))
    (user-error "Restricted query files must be a list of non-empty strings"))
  (let* ((effective-config-file
          (org-files-db--resolve-config-file
           config-file config-file-supplied-p origin))
         (base
          (org-files-db--query-arguments
           query effective-config-file origin includes))
         (query-text (car (last base)))
         (arguments
          (append (butlast base)
                  '("--restrict-files-json" "-")
                  (list query-text)))
         (input
          (concat
           (json-serialize (vconcat (delete-dups (copy-sequence files))))
           "\n")))
    (org-files-db--call "query" arguments input)))

(defun org-files-db--validate-search-scope (scope)
  "Return validated search SCOPE."
  (let ((scope (or scope 'all)))
    (unless (memq scope '(all title body))
      (user-error "Unsupported orgfdb search scope: %S" scope))
    scope))

(cl-defun org-files-db--search-arguments
    (expression &optional scope (config-file nil config-file-supplied-p) origin)
  "Return orgfdb arguments for EXPRESSION and SCOPE.
An omitted CONFIG-FILE inherits `org-files-db-config-file'; an explicitly
supplied nil disables --config.  ORIGIN identifies validation errors."
  (unless (and (stringp expression)
               (not (string-empty-p (string-trim expression))))
    (user-error "Search expression must be a non-empty string"))
  (let ((scope (org-files-db--validate-search-scope scope))
        (effective-config-file
         (org-files-db--resolve-config-file
          config-file config-file-supplied-p origin)))
    (append '("--format" "json")
            (pcase scope
              ('title '("--title"))
              ('body '("--body"))
              (_ nil))
            (org-files-db--config-arguments effective-config-file origin)
            (list expression))))

(cl-defun org-files-db--execute-search
    (expression &optional scope (config-file nil config-file-supplied-p) origin)
  "Execute one FTS5 search for EXPRESSION in SCOPE.
An omitted CONFIG-FILE inherits `org-files-db-config-file'; an explicitly
supplied nil disables --config.  ORIGIN identifies validation errors."
  (let ((effective-config-file
         (org-files-db--resolve-config-file
          config-file config-file-supplied-p origin)))
    (org-files-db--call
     "search"
     (org-files-db--search-arguments
      expression scope effective-config-file origin))))

(defun org-files-db--normalize-results (response)
  "Return a result list from orgfdb RESPONSE."
  (cond
   ((null response) nil)
   ((org-files-db--has-key-p response 'results)
    (let ((results (org-files-db--get response 'results)))
      (if (vectorp results) (append results nil) results)))
   ((and (vectorp response)
         (or (zerop (length response))
             (let ((first (aref response 0)))
               (or (listp first) (hash-table-p first)))))
    (append response nil))
   ((and (listp response)
         (or (listp (car response))
             (hash-table-p (car response))))
    response)
   (t
    (signal 'org-files-db-error
            (list "Unexpected orgfdb JSON response shape")))))

(defun org-files-db--result-with-config (result config-file)
  "Return a copy of RESULT carrying effective CONFIG-FILE metadata."
  (cond
   ((hash-table-p result)
    (let ((copy (copy-hash-table result)))
      (puthash org-files-db--result-config-key config-file copy)
      copy))
   ((listp result)
    (let ((entry (assq org-files-db--result-config-key result)))
      (cons (cons org-files-db--result-config-key config-file)
            (if entry
                (delq entry (copy-sequence result))
              result))))
   (t
    (signal 'org-files-db-error
            (list "Cannot attach configuration context to malformed result")))))

(defun org-files-db--results-with-config (results config-file)
  "Return copies of RESULTS carrying effective CONFIG-FILE metadata."
  (mapcar (lambda (result)
            (org-files-db--result-with-config result config-file))
          results))

(defun org-files-db--result-config-file (result &optional origin)
  "Return RESULT's effective configuration file.
For results created before configuration metadata was added, inherit the
current global value.  ORIGIN identifies the follow-up operation in errors."
  (let ((supplied-p
         (org-files-db--has-key-p result org-files-db--result-config-key)))
    (org-files-db--resolve-config-file
     (and supplied-p
          (org-files-db--get result org-files-db--result-config-key))
     supplied-p
     origin)))

(defun org-files-db--response-target (response)
  "Return RESPONSE target as a symbol, when present."
  (let ((target (org-files-db--get response 'target)))
    (cond
     ((symbolp target) target)
     ((stringp target) (intern target))
     (t nil))))

(defun org-files-db--result-location (result)
  "Return RESULT location object."
  (or (org-files-db--get result 'location)
      result))

(defun org-files-db--result-file (result)
  "Return the source file path for RESULT."
  (or (org-files-db--get result 'location 'file_path)
      (org-files-db--get result 'file_path)
      (and (memq (org-files-db--kind result) '(file root))
           (org-files-db--get result 'path))))

(defun org-files-db--result-line (result)
  "Return the one-based source line for RESULT, or nil."
  (or (org-files-db--get result 'location 'line)
      (org-files-db--get result 'line)
      (org-files-db--get result 'line_number)))

(defun org-files-db--result-byte-start (result)
  "Return the zero-based UTF-8 byte start for RESULT, or nil."
  (or (org-files-db--get result 'location 'byte_start)
      (org-files-db--get result 'byte_start)))

(defun org-files-db--result-title (result)
  "Return a readable title for RESULT."
  (or (org-files-db--get result 'title)
      (org-files-db--get result 'raw_description)
      (org-files-db--get result 'raw_target)
      (org-files-db--get result 'name)
      (when-let* ((file (org-files-db--result-file result)))
        (file-name-nondirectory file))
      "Result"))

(defun org-files-db--utf8-byte-position (file byte coding-system)
  "Return an Emacs character position for zero-based BYTE in FILE.
CODING-SYSTEM is the coding system of the visiting buffer."
  (when (and (integerp byte) (>= byte 0))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file nil 0 byte)
      (1+ (length (decode-coding-string
                   (buffer-substring-no-properties (point-min) (point-max))
                   (or coding-system 'utf-8-unix)
                   t))))))

(defun org-files-db--goto-result-location (result)
  "Move point to RESULT's stored location in the current buffer."
  (let ((byte (org-files-db--result-byte-start result))
        (line (org-files-db--result-line result))
        (file (org-files-db--result-file result)))
    (widen)
    (cond
     ((and byte file (file-readable-p file))
      (condition-case nil
          (goto-char
           (min (point-max)
                (org-files-db--utf8-byte-position
                 file byte buffer-file-coding-system)))
        (error
         (if line
             (progn
               (goto-char (point-min))
               (forward-line (1- line)))
           (goto-char (point-min))))))
     (line
      (goto-char (point-min))
      (forward-line (1- line)))
     (t (goto-char (point-min))))))

(defun org-files-db--backlinks-query-at-point (&optional no-create)
  "Return a backlink query for the current Org location.
When NO-CREATE is non-nil, never offer to create a missing heading ID."
  (unless (and buffer-file-name (derived-mode-p 'org-mode))
    (user-error "Backlinks require a file-visiting Org buffer"))
  (let ((file (expand-file-name buffer-file-name)))
    (if (org-before-first-heading-p)
        `(links (target (files (file-path ,file :exact t))))
      (org-back-to-heading t)
      (let ((id (org-entry-get nil "ID"))
            (custom-id (org-entry-get nil "CUSTOM_ID")))
        (cond
         (id
          `(links
            (target
             (headings (property "ID" ,id :inherit nil)))))
         (custom-id
          `(links
            (target
             (headings
              (and (file-path ,file :exact t)
                   (property "CUSTOM_ID" ,custom-id :inherit nil))))))
         ((and (not no-create)
               (yes-or-no-p "Heading has no stable identifier; create an ID? "))
          (setq id (org-id-get-create))
          (save-buffer)
          `(links
            (target
             (headings (property "ID" ,id :inherit nil)))))
         (t (user-error "A stable heading identifier is required")))))))

(defun org-files-db--visit-result (result)
  "Open RESULT, jump to its source location, and reveal Org context."
  (unless result
    (user-error "No org-files-db result"))
  (let ((file (org-files-db--result-file result)))
    (unless (and file (file-readable-p file))
      (user-error "Result file is missing or unreadable: %s" (or file "<none>")))
    (find-file file)
    (org-files-db--goto-result-location result)
    (when (derived-mode-p 'org-mode)
      (org-fold-show-context))))

(defun org-files-db--heading-at-result (result function)
  "Call FUNCTION at RESULT's heading and return its value."
  (let ((file (org-files-db--result-file result)))
    (when (and file (file-readable-p file))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (org-files-db--goto-result-location result)
          (when (derived-mode-p 'org-mode)
            (condition-case nil
                (progn
                  (org-back-to-heading t)
                  (funcall function))
              (error nil))))))))

(defun org-files-db--heading-link-info (result)
  "Return information about the first Org link in RESULT's title."
  (org-files-db--heading-at-result
   result
   (lambda ()
     (let* ((title (org-get-heading t t t t))
            (tree (org-element-parse-secondary-string title '(link)))
            (link (org-element-map tree 'link #'identity nil t)))
       (when (and link
                  (member (org-element-property :type link) '("file" "id")))
         (let* ((rendered (org-element-interpret-data link))
                (begin (string-search rendered title))
                (end (and begin (+ begin (length rendered))))
                (description
                 (when-let* ((contents (org-element-contents link)))
                   (org-element-interpret-data contents))))
           (list :title title
                 :source-result result
                 :source-file buffer-file-name
                 :type (org-element-property :type link)
                 :path (org-element-property :path link)
                 :search-option (org-element-property :search-option link)
                 :description description
                 :rendered rendered
                 :prefix (and begin (substring title 0 begin))
                 :suffix (and end (substring title end)))))))))

(defun org-files-db--split-file-link-path (path search-option)
  "Return a pair of file PATH and SEARCH-OPTION.
The fallback split is used for Org versions which leave the search option in
PATH."
  (if search-option
      (cons path search-option)
    (if (string-match "\\`\\(.*\\)::\\(.*\\)\\'" path)
        (cons (match-string 1 path) (match-string 2 path))
      (cons path nil))))

(defun org-files-db--absolute-linked-file (info)
  "Return the absolute file target described by INFO."
  (pcase-let* ((`(,path . ,_) (org-files-db--split-file-link-path
                               (plist-get info :path)
                               (plist-get info :search-option)))
               (path (org-link-unescape path)))
    (expand-file-name path
                      (file-name-directory (plist-get info :source-file)))))

(defun org-files-db--goto-linked-target (info)
  "Open and visit the link target described by INFO.
Return a result-like alist for the target."
  (pcase (plist-get info :type)
    ("id"
     (let* ((id (plist-get info :path))
            (config-file
             (org-files-db--result-config-file
              (plist-get info :source-result)
              "Follow-heading action"))
            (response
             (org-files-db--execute-query
              `(headings (property "ID" ,id :inherit nil))
              config-file
              "Follow-heading action"))
            (results
             (org-files-db--results-with-config
              (org-files-db--normalize-results response)
              config-file)))
       (pcase (length results)
         (0 (user-error "Cannot resolve indexed Org ID %s" id))
         (1
          (let ((result (car results)))
            (org-files-db--visit-result result)
            result))
         (_ (user-error "Org ID %s resolves to multiple indexed headings" id)))))
    ("file"
     (pcase-let* ((`(,_ . ,search)
                   (org-files-db--split-file-link-path
                    (plist-get info :path)
                    (plist-get info :search-option)))
                  (file (org-files-db--absolute-linked-file info)))
       (unless (file-readable-p file)
         (user-error "Linked file is missing: %s" file))
       (find-file file)
       (goto-char (point-min))
       (when search
         (org-link-search search))
       (let ((title (if (org-at-heading-p)
                        (org-get-heading t t t t)
                      (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))
                          (file-name-base file)))))
         (when (listp title)
           (setq title (car title)))
         `((kind . ,(if (org-at-heading-p) "heading" "file"))
           (title . ,title)
           (location . ((file_path . ,file)
                        (line . ,(line-number-at-pos))
                        (byte_start . nil)))))))
    (_ (user-error "Unsupported heading link type"))))

(defun org-files-db--base64url-encode (text)
  "Encode TEXT as unpadded URL-safe base64."
  (string-remove-suffix
   "="
   (string-remove-suffix
    "="
    (replace-regexp-in-string
     "/" "_"
     (replace-regexp-in-string
      (regexp-quote "+") "-" (base64-encode-string text t) t t)
     t t))))

(defun org-files-db--base64url-decode (text)
  "Decode URL-safe base64 TEXT."
  (let* ((normal (replace-regexp-in-string
                  "_" "/"
                  (replace-regexp-in-string "-" "+" text t t)
                  t t))
         (padding (mod (- 4 (mod (length normal) 4)) 4)))
    (base64-decode-string (concat normal (make-string padding ?=)))))

(defun org-files-db--result-link-target (result)
  "Return an org-files-db custom link target for RESULT."
  (let ((payload
         `((file_path . ,(org-files-db--result-file result))
           (line . ,(org-files-db--result-line result))
           (byte_start . ,(org-files-db--result-byte-start result)))))
    (concat "org-files-db:"
            (org-files-db--base64url-encode
             (json-encode payload)))))

(defun org-files-db--result-org-link (result &optional description)
  "Return an Org link for RESULT using DESCRIPTION."
  (org-link-make-string
   (org-files-db--result-link-target result)
   (or description (org-files-db--result-title result))))

(defun org-files-db--follow-org-link (path _)
  "Follow an org-files-db link encoded in PATH."
  (let* ((json (org-files-db--base64url-decode path))
         (result (org-files-db--parse-json json)))
    (org-files-db--visit-result `((location . ,result)))))

(org-link-set-parameters "org-files-db" :follow #'org-files-db--follow-org-link)

(defun org-files-db--node-title (node)
  "Return a title string for path NODE."
  (cond
   ((stringp node) node)
   ((or (listp node) (hash-table-p node))
    (or (org-files-db--get node 'title)
        (org-files-db--get node 'name)
        (org-files-db--get node 'raw_description)
        (org-files-db--get node 'raw_target)
        ""))
   (t (format "%s" node))))

(defun org-files-db--result-path-nodes (result)
  "Return outline path nodes for RESULT."
  (or (org-files-db--get result 'node_path)
      (org-files-db--get result 'source 'source_path)
      (org-files-db--get result 'heading_path)
      (org-files-db--get result 'outline_path)))

(defun org-files-db--column-properties (definition)
  "Return the property list represented by column DEFINITION."
  (if (consp definition) (cdr definition) nil))

(defun org-files-db--column-option (definition property default)
  "Return DEFINITION PROPERTY, or DEFAULT when it is absent."
  (let ((properties (org-files-db--column-properties definition)))
    (if (plist-member properties property)
        (plist-get properties property)
      default)))

(defun org-files-db--outline-options (definition)
  "Return validated outline options for column DEFINITION."
  (let ((separator
         (org-files-db--column-option
          definition :separator org-files-db-outline-path-separator))
        (include-root
         (org-files-db--column-option
          definition :include-root org-files-db-outline-path-include-root))
        (include-match
         (org-files-db--column-option
          definition :include-match org-files-db-outline-path-include-match)))
    (unless (stringp separator)
      (user-error "Outline-path separator must be a string"))
    (list :separator separator
          :include-root include-root
          :include-match include-match)))

(defun org-files-db--path-root-title (nodes)
  "Return the file/root title represented by NODES."
  (car (org-files-db--path-data nodes)))

(defun org-files-db--path-heading-titles (nodes)
  "Return heading titles represented by NODES."
  (cdr (org-files-db--path-data nodes)))

(defun org-files-db--path-data (nodes)
  "Return the root title and heading titles represented by NODES."
  (let (root headings)
    (seq-doseq (node nodes)
      (let* ((object-p (or (listp node) (hash-table-p node)))
             (kind (and object-p (org-files-db--kind node))))
        (when (and (null root) (memq kind '(file root)))
          (setq root (org-files-db--node-title node)))
        (when (or (stringp node)
                  (eq kind 'heading)
                  (and object-p (null kind)))
          (let ((title (org-files-db--node-title node)))
            (unless (string-empty-p title)
              (push title headings))))))
    (cons root (nreverse headings))))

(defun org-files-db--append-heading-title (headings title)
  "Append TITLE to HEADINGS unless it is already the final component."
  (if (or (not (and (stringp title) (not (string-empty-p title))))
          (equal (car (last headings)) title))
      headings
    (append headings (list title))))

(defun org-files-db--outline-data (root headings)
  "Return normalized outline data for ROOT and HEADINGS."
  (cons (and (stringp root) (not (string-empty-p root)) root)
        (delq nil headings)))

(defun org-files-db--generic-outline-data (result)
  "Return generic outline data for RESULT."
  (let* ((nodes (or (org-files-db--result-path-nodes result) nil))
         (path-data (org-files-db--path-data nodes))
         (root (car path-data))
         (headings (cdr path-data)))
    (unless headings
      (let ((title (org-files-db--result-title result)))
        (setq headings (list title))
        (when (equal root title)
          (setq root nil))))
    (if (and (eq root (car path-data))
             (eq headings (cdr path-data)))
        path-data
      (org-files-db--outline-data root headings))))

(defun org-files-db--source-outline-data (result)
  "Return source outline data for link RESULT."
  (let* ((source (org-files-db--get result 'source))
         (heading (org-files-db--get source 'heading))
         (nodes
          (or (org-files-db--get heading 'outline_path)
              (org-files-db--get source 'outline_path)
              (org-files-db--get source 'source_path)
              (org-files-db--get result 'node_path)
              nil))
         (path-data (org-files-db--path-data nodes))
         (headings (cdr path-data))
         (heading-title
          (or (org-files-db--get heading 'title)
              (when (eq (org-files-db--kind source) 'heading)
                (org-files-db--get source 'title))))
         (root
          (or (org-files-db--get source 'file 'title)
              (car path-data))))
    (org-files-db--outline-data
     root
     (org-files-db--append-heading-title headings heading-title))))

(defun org-files-db--resolved-target-p (result)
  "Return non-nil when RESULT may contain a resolved structured target."
  (let ((status (org-files-db--get result 'resolution_status)))
    (or (null status)
        (equal (if (symbolp status) (symbol-name status) status)
               "resolved"))))

(defun org-files-db--target-outline-data (result)
  "Return resolved target outline data for link RESULT, or nil."
  (when (org-files-db--resolved-target-p result)
    (let* ((target (org-files-db--get result 'target))
           (file (org-files-db--get target 'file))
           (heading (org-files-db--get target 'heading))
           (nodes
            (or (org-files-db--get heading 'outline_path)
                (org-files-db--get target 'outline_path)
                (org-files-db--get target 'node_path)
                nil))
           (path-data (org-files-db--path-data nodes))
           (headings (cdr path-data))
           (heading-title (org-files-db--get heading 'title))
           (root
            (or (org-files-db--get file 'title)
                (car path-data))))
      (when (or file heading nodes)
        (org-files-db--outline-data
         root
         (org-files-db--append-heading-title headings heading-title))))))

(defun org-files-db--format-outline-data (data definition)
  "Format outline DATA according to column DEFINITION."
  (if (null data)
      ""
    (let* ((options (org-files-db--outline-options definition))
           (headings (cdr data))
           (root (car data))
           (include-root (plist-get options :include-root))
           (include-match (plist-get options :include-match)))
      (unless include-match
        (setq headings (butlast headings)))
      (string-join
       (append (when (and include-root root) (list root)) headings)
       (plist-get options :separator)))))

(defun org-files-db--outline-path (result &optional definition)
  "Return RESULT outline path formatted for DEFINITION."
  (org-files-db--format-outline-data
   (org-files-db--generic-outline-data result)
   (or definition 'outline-path)))

(defun org-files-db--source-outline-path (result &optional definition)
  "Return link RESULT source outline path formatted for DEFINITION."
  (org-files-db--format-outline-data
   (org-files-db--source-outline-data result)
   (or definition 'source-outline-path)))

(defun org-files-db--target-outline-path (result &optional definition)
  "Return link RESULT target outline path formatted for DEFINITION."
  (org-files-db--format-outline-data
   (org-files-db--target-outline-data result)
   (or definition 'target-outline-path)))

(defun org-files-db--format-list-value (value)
  "Format list VALUE as a compact string."
  (if (seq-every-p #'stringp value)
      (string-join (if (vectorp value) (append value nil) value) ",")
    (string-join
     (mapcar (lambda (item)
               (cond
                ((stringp item) item)
                ((numberp item) (number-to-string item))
                ((symbolp item) (symbol-name item))
                (t (format "%s" item))))
             value)
     ",")))

(defconst org-files-db--presentation-uncomputed
  (make-symbol "org-files-db-presentation-uncomputed")
  "Sentinel for presentation-source values that have not been calculated.")

(defvar org-files-db--candidate-lookups
  (make-hash-table :test #'eq :weakness 'key)
  "Weak map from prepared candidate lists to direct result vectors.")

(cl-defstruct org-files-db--presentation-column
  "Normalized column definition used by one presentation."
  name
  definition
  extractor
  face
  face-function
  required-include
  width-kind
  width-limit
  truncate-position
  truncate-marker
  truncate-marker-width
  outline-separator
  outline-include-root
  outline-include-match)

(cl-defstruct org-files-db--presentation-source
  "Lazily normalized values shared by all rows for one result."
  result
  (cached-kind org-files-db--presentation-uncomputed)
  (cached-title org-files-db--presentation-uncomputed)
  (cached-explicit-title org-files-db--presentation-uncomputed)
  (cached-file-path org-files-db--presentation-uncomputed)
  (cached-file-name org-files-db--presentation-uncomputed)
  (cached-file-title org-files-db--presentation-uncomputed)
  (cached-line-number org-files-db--presentation-uncomputed)
  (cached-byte-start org-files-db--presentation-uncomputed)
  (cached-todo-keyword org-files-db--presentation-uncomputed)
  (cached-todo-type org-files-db--presentation-uncomputed)
  (cached-priority org-files-db--presentation-uncomputed)
  (cached-tags org-files-db--presentation-uncomputed)
  (cached-level org-files-db--presentation-uncomputed)
  (cached-outline-data org-files-db--presentation-uncomputed)
  (cached-source-outline-data org-files-db--presentation-uncomputed)
  (cached-target-outline-data org-files-db--presentation-uncomputed)
  raw-values)

(cl-defstruct org-files-db--presentation-cell
  "One calculated presentation cell."
  logical-value
  display
  display-width
  source-face
  face)

(cl-defstruct org-files-db--presentation-row
  "One prepared completion row."
  result
  source
  original-position
  row-source
  row-value
  cells
  sort-keys)

(cl-defstruct org-files-db--presentation-sort
  "One normalized sort specification used by a presentation."
  name
  column
  column-index
  kind
  direction
  missing
  case-fold)

(cl-defstruct org-files-db--presentation
  "Complete eagerly prepared result presentation."
  columns
  sources
  rows
  widths
  face-cache
  candidates
  lookup
  timings
  phase-metrics)

(defconst org-files-db--large-presentation-row-count 5000
  "Minimum result count for bounded large-presentation GC handling.")

(defconst org-files-db--large-presentation-gc-threshold (* 64 1024 1024)
  "Minimum temporary GC threshold used while preparing large result sets.")

(defmacro org-files-db--presentation-source-cached
    (source accessor &rest body)
  "Return SOURCE ACCESSOR value, calculating BODY once when needed."
  (declare (indent 2) (debug t))
  `(let ((value (,accessor ,source)))
     (if (eq value org-files-db--presentation-uncomputed)
         (let ((calculated (progn ,@body)))
           (setf (,accessor ,source) calculated)
           calculated)
       value)))

(defun org-files-db--presentation-source-raw-value (source key)
  "Return SOURCE raw KEY value, caching arbitrary columns."
  (let ((entry (assq key
                     (org-files-db--presentation-source-raw-values source))))
    (if entry
        (cdr entry)
      (let ((value
             (org-files-db--get
              (org-files-db--presentation-source-result source)
              key)))
        (push (cons key value)
              (org-files-db--presentation-source-raw-values source))
        value))))

(defun org-files-db--presentation-source-kind (source)
  "Return the cached kind for SOURCE."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-kind
    (org-files-db--kind
     (org-files-db--presentation-source-result source))))

(defun org-files-db--presentation-source-file-path (source)
  "Return the cached source file path for SOURCE."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-file-path
    (org-files-db--result-file
     (org-files-db--presentation-source-result source))))

(defun org-files-db--presentation-source-explicit-title (source)
  "Return SOURCE's cached explicit title."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-explicit-title
    (org-files-db--get
     (org-files-db--presentation-source-result source)
     'title)))

(defun org-files-db--presentation-source-title (source)
  "Return the cached readable title for SOURCE."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-title
    (or (org-files-db--presentation-source-explicit-title source)
        (org-files-db--presentation-source-raw-value source 'raw_description)
        (org-files-db--presentation-source-raw-value source 'raw_target)
        (org-files-db--presentation-source-raw-value source 'name)
        (when-let* ((file
                     (org-files-db--presentation-source-file-path source)))
          (file-name-nondirectory file))
        "Result")))

(defun org-files-db--presentation-source-file-name (source)
  "Return the cached source file name for SOURCE."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-file-name
    (when-let* ((file (org-files-db--presentation-source-file-path source)))
      (file-name-nondirectory file))))

(defun org-files-db--presentation-source-file-title (source)
  "Return the cached file title for SOURCE."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-file-title
    (or (org-files-db--presentation-source-explicit-title source)
        (when-let* ((file
                     (org-files-db--presentation-source-file-path source)))
          (file-name-base file)))))

(defun org-files-db--presentation-source-line-number (source)
  "Return the cached line number for SOURCE."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-line-number
    (org-files-db--result-line
     (org-files-db--presentation-source-result source))))

(defun org-files-db--presentation-source-byte-start (source)
  "Return the cached byte start for SOURCE."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-byte-start
    (org-files-db--result-byte-start
     (org-files-db--presentation-source-result source))))

(defun org-files-db--presentation-source-todo-keyword (source)
  "Return the cached TODO keyword for SOURCE."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-todo-keyword
    (org-files-db--get
     (org-files-db--presentation-source-result source)
     'todo_keyword)))

(defun org-files-db--presentation-source-todo-type (source)
  "Return the cached TODO type for SOURCE."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-todo-type
    (org-files-db--get
     (org-files-db--presentation-source-result source)
     'todo_type)))

(defun org-files-db--presentation-source-priority (source)
  "Return the cached priority for SOURCE."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-priority
    (org-files-db--get
     (org-files-db--presentation-source-result source)
     'priority)))

(defun org-files-db--presentation-source-tags (source)
  "Return the cached aggregate tags for SOURCE."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-tags
    (let ((result (org-files-db--presentation-source-result source)))
      (or (org-files-db--get result 'all_tags)
          (org-files-db--get result 'tags)))))

(defun org-files-db--presentation-source-level (source)
  "Return the cached heading level for SOURCE."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-level
    (let ((result (org-files-db--presentation-source-result source)))
      (or (org-files-db--get result 'level)
          (org-files-db--get result 'heading_level)
          1))))

(defun org-files-db--presentation-source-outline-data (source)
  "Return cached generic outline data for SOURCE."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-outline-data
    (org-files-db--generic-outline-data
     (org-files-db--presentation-source-result source))))

(defun org-files-db--presentation-source-source-outline-data (source)
  "Return cached link-source outline data for SOURCE."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-source-outline-data
    (org-files-db--source-outline-data
     (org-files-db--presentation-source-result source))))

(defun org-files-db--presentation-source-target-outline-data (source)
  "Return cached link-target outline data for SOURCE."
  (org-files-db--presentation-source-cached
      source org-files-db--presentation-source-cached-target-outline-data
    (org-files-db--target-outline-data
     (org-files-db--presentation-source-result source))))

(defun org-files-db--column-width-spec (definition)
  "Return the width specification from column DEFINITION."
  (or (plist-get (org-files-db--column-properties definition) :width) 'auto))

(defun org-files-db--validate-width-spec (spec column)
  "Validate width SPEC for COLUMN and return it."
  (pcase spec
    ('auto spec)
    (`(max ,n)
     (unless (and (integerp n) (> n 0))
       (user-error "Column %s has an invalid maximum width" column))
     spec)
    (`(fixed ,n)
     (unless (and (integerp n) (> n 0))
       (user-error "Column %s has an invalid fixed width" column))
     spec)
    (_ (user-error "Column %s has an invalid width specification: %S"
                   column spec))))

(defun org-files-db--truncate-options (definition)
  "Return validated truncation options for column DEFINITION."
  (let* ((properties (org-files-db--column-properties definition))
         (specified (and (plist-member properties :truncate)
                         (plist-get properties :truncate))))
    (unless (or (null specified) (listp specified))
      (user-error "Column %s has invalid truncation options: %S"
                  (org-files-db--column-name definition) specified))
    (let ((position
           (if (and specified (plist-member specified :position))
               (plist-get specified :position)
             org-files-db-truncate-position))
          (marker
           (if (and specified (plist-member specified :marker))
               (plist-get specified :marker)
             org-files-db-truncate-marker)))
      (unless (memq position '(left middle right))
        (user-error "Column %s has invalid truncation position: %S"
                    (org-files-db--column-name definition) position))
      (unless (stringp marker)
        (user-error "Column %s has a non-string truncation marker"
                    (org-files-db--column-name definition)))
      (list :position position :marker marker))))

(defun org-files-db--presentation-extractor (name)
  "Return the presentation extractor function for column NAME."
  (pcase name
    ('todo-keyword #'org-files-db--presentation-extract-todo-keyword)
    ('todo-type #'org-files-db--presentation-extract-todo-type)
    ('priority #'org-files-db--presentation-extract-priority)
    ('title #'org-files-db--presentation-extract-title)
    ('outline-path #'org-files-db--presentation-extract-outline-path)
    ('source-outline-path
     #'org-files-db--presentation-extract-source-outline-path)
    ('target-outline-path
     #'org-files-db--presentation-extract-target-outline-path)
    ('tags #'org-files-db--presentation-extract-tags)
    ('file-name #'org-files-db--presentation-extract-file-name)
    ('file-title #'org-files-db--presentation-extract-file-title)
    ('file-path #'org-files-db--presentation-extract-file-path)
    ('line-number #'org-files-db--presentation-extract-line-number)
    ('byte-start #'org-files-db--presentation-extract-byte-start)
    ('byte-end #'org-files-db--presentation-extract-byte-end)
    ('file-id #'org-files-db--presentation-extract-file-id)
    ('parent-id #'org-files-db--presentation-extract-parent-id)
    ('scheduled-raw #'org-files-db--presentation-extract-scheduled-raw)
    ('deadline-raw #'org-files-db--presentation-extract-deadline-raw)
    ('closed-raw #'org-files-db--presentation-extract-closed-raw)
    ('link-type #'org-files-db--presentation-extract-link-type)
    ('link-path #'org-files-db--presentation-extract-link-path)
    ((or 'status 'resolution-status)
     #'org-files-db--presentation-extract-resolution-status)
    ((or 'link-target 'raw-target)
     #'org-files-db--presentation-extract-raw-target)
    ((or 'link-description 'raw-description)
     #'org-files-db--presentation-extract-raw-description)
    (_ #'org-files-db--presentation-extract-generic)))

(defun org-files-db--presentation-static-face (name)
  "Return a static face for column NAME, or nil."
  (pcase name
    ('priority 'org-priority)
    ('tags 'org-tag)
    ((or 'scheduled-raw 'deadline-raw 'closed-raw 'date) 'org-date)
    (_ nil)))

(defun org-files-db--presentation-face-function (name)
  "Return a dynamic face function for column NAME, or nil."
  (pcase name
    ('todo-keyword #'org-files-db--presentation-todo-face)
    ((or 'title 'outline-path 'source-outline-path 'target-outline-path)
     #'org-files-db--presentation-level-face)
    (_ nil)))

(defun org-files-db--presentation-required-include (name)
  "Return the CLI include required by column NAME, or nil."
  (pcase name
    ((or 'outline-path 'source-outline-path) 'path)
    ('target-outline-path 'target)
    (_ nil)))

(defun org-files-db--normalize-column (definition)
  "Return one normalized presentation column for DEFINITION."
  (let* ((name (org-files-db--column-name definition))
         (properties (org-files-db--column-properties definition)))
    (unless (symbolp name)
      (user-error "Invalid org-files-db column: %S" definition))
    (unless (zerop (mod (length properties) 2))
      (user-error "Column %s has malformed property options" name))
    (let ((tail properties))
      (while tail
        (unless (keywordp (pop tail))
          (user-error "Column %s has a non-keyword option" name))
        (pop tail)))
    (let* ((width-spec
            (org-files-db--validate-width-spec
             (org-files-db--column-width-spec definition)
             name))
           (width-kind
            (pcase width-spec
              ('auto 'auto)
              (`(max ,_) 'max)
              (`(fixed ,_) 'fixed)))
           (width-limit
            (pcase width-spec
              (`(max ,n) n)
              (`(fixed ,n) n)
              (_ nil)))
           (truncate (org-files-db--truncate-options definition))
           (outline
            (when (memq name
                        '(outline-path source-outline-path
                                       target-outline-path))
              (org-files-db--outline-options definition))))
      (make-org-files-db--presentation-column
       :name name
       :definition definition
       :extractor (org-files-db--presentation-extractor name)
       :face (org-files-db--presentation-static-face name)
       :face-function (org-files-db--presentation-face-function name)
       :required-include
       (org-files-db--presentation-required-include name)
       :width-kind width-kind
       :width-limit width-limit
       :truncate-position (plist-get truncate :position)
       :truncate-marker (plist-get truncate :marker)
       :truncate-marker-width
       (string-width (plist-get truncate :marker))
       :outline-separator
       (and outline (plist-get outline :separator))
       :outline-include-root
       (and outline (plist-get outline :include-root))
       :outline-include-match
       (and outline (plist-get outline :include-match))))))

(defun org-files-db--normalized-columns-p (columns)
  "Return non-nil when COLUMNS is a normalized column vector."
  (and (vectorp columns)
       (or (zerop (length columns))
           (org-files-db--presentation-column-p (aref columns 0)))))

(defun org-files-db--normalize-columns (columns)
  "Compile COLUMNS into one normalized presentation vector."
  (if (org-files-db--normalized-columns-p columns)
      columns
    (vconcat (mapcar #'org-files-db--normalize-column columns))))

(defconst org-files-db--sort-columns-by-context
  '((heading
     title todo-keyword todo-type priority outline-path tags file-title
     file-name file-path line-number scheduled-raw deadline-raw closed-raw)
    (file
     file-title file-name file-path tags line-number)
    (link
     link-type link-target link-description resolution-status
     source-outline-path target-outline-path file-name file-path line-number)
    (search
     title outline-path file-name file-path line-number rank))
  "Public sort columns supported for each result context.")

(defconst org-files-db--row-sort-columns
  '((tag . tags)
    (property-name . effective-properties)
    (property-value . effective-properties)
    (keyword-name . keywords)
    (keyword-value . keywords))
  "Singular sort columns reserved for row-expanded presentations.")

(defconst org-files-db--sort-missing
  (make-symbol "org-files-db-sort-missing")
  "Sentinel stored for missing normalized sort values.")

(defun org-files-db--sort-context (context)
  "Return canonical sort CONTEXT symbol."
  (pcase context
    ((or 'heading 'headings) 'heading)
    ((or 'file 'files) 'file)
    ((or 'link 'links) 'link)
    ('search 'search)
    (_ (user-error "Unsupported org-files-db sort context: %S" context))))

(defun org-files-db--default-sort (context)
  "Return configured default sort specification for CONTEXT."
  (pcase (org-files-db--sort-context context)
    ('heading org-files-db-heading-sort)
    ('file org-files-db-file-sort)
    ('link org-files-db-link-sort)
    ('search org-files-db-search-sort)))

(defun org-files-db--effective-sort (context sort supplied-p)
  "Return effective SORT for CONTEXT.
When SUPPLIED-P is non-nil, SORT wins even when it is nil."
  (if supplied-p sort (org-files-db--default-sort context)))

(defun org-files-db--sort-user-error (origin format-string &rest arguments)
  "Signal a sort validation error for ORIGIN using FORMAT-STRING and ARGUMENTS."
  (user-error "%s: %s"
              (or origin "Sorting")
              (apply #'format format-string arguments)))

(defun org-files-db--sort-column-index (columns name)
  "Return the first index of column NAME in normalized COLUMNS, or nil."
  (let ((index 0)
        found)
    (while (and (< index (length columns)) (null found))
      (when (eq (org-files-db--presentation-column-name (aref columns index))
                name)
        (setq found index))
      (setq index (1+ index)))
    found))

(defun org-files-db--sort-kind (name)
  "Return normalized comparison kind for sort column NAME."
  (pcase name
    ((or 'line-number 'rank) 'number)
    ('priority 'priority)
    ((or 'scheduled-raw 'deadline-raw 'closed-raw) 'timestamp)
    ((or 'outline-path 'source-outline-path 'target-outline-path) 'outline)
    ('tags 'tags)
    (_ 'text)))

(defun org-files-db--normalize-sort-spec
    (spec columns context origin &optional row-source)
  "Normalize one sort SPEC for COLUMNS and CONTEXT.
ORIGIN is used in validation errors.  ROW-SOURCE is reserved for expanded rows."
  (unless (and (consp spec) (proper-list-p spec) (symbolp (car spec)))
    (org-files-db--sort-user-error origin "Malformed sort specification: %S" spec))
  (let* ((name (car spec))
         (properties (cdr spec))
         (row-requirement (assq name org-files-db--row-sort-columns))
         (allowed (cdr (assq (org-files-db--sort-context context)
                             org-files-db--sort-columns-by-context))))
    (unless (zerop (mod (length properties) 2))
      (org-files-db--sort-user-error
       origin "Sort column %s has malformed options" name))
    (when row-requirement
      (unless (eq row-source (cdr row-requirement))
        (org-files-db--sort-user-error
         origin "Sort column %s requires :row-source %s"
         name (cdr row-requirement))))
    (unless (or row-requirement (memq name allowed))
      (org-files-db--sort-user-error
       origin "Sort column %s is unavailable for %s results"
       name (org-files-db--sort-context context)))
    (let ((tail properties))
      (while tail
        (let ((key (pop tail)))
          (pop tail)
          (unless (memq key '(:direction :missing :case-fold))
            (org-files-db--sort-user-error
             origin "Sort column %s has unsupported option %S" name key)))))
    (let* ((direction (if (plist-member properties :direction)
                          (plist-get properties :direction)
                        'asc))
           (missing (if (plist-member properties :missing)
                        (plist-get properties :missing)
                      'last))
           (case-fold (if (plist-member properties :case-fold)
                          (plist-get properties :case-fold)
                        t))
           (column-index (org-files-db--sort-column-index columns name))
           (column (if column-index
                       (aref columns column-index)
                     (org-files-db--normalize-column (list name)))))
      (unless (memq direction '(asc desc))
        (org-files-db--sort-user-error
         origin "Sort column %s has unsupported direction %S" name direction))
      (unless (memq missing '(first last))
        (org-files-db--sort-user-error
         origin "Sort column %s has unsupported missing position %S" name missing))
      (unless (memq case-fold '(nil t))
        (org-files-db--sort-user-error
         origin "Sort column %s has non-boolean :case-fold %S" name case-fold))
      (make-org-files-db--presentation-sort
       :name name
       :column column
       :column-index column-index
       :kind (org-files-db--sort-kind name)
       :direction direction
       :missing missing
       :case-fold case-fold))))

(defun org-files-db--normalize-sort
    (sort columns context &optional origin row-source)
  "Normalize SORT using presentation COLUMNS for CONTEXT.
ORIGIN is included in validation errors.  ROW-SOURCE supports future expanded
rows.  Nil SORT returns an empty vector and therefore preserves source order."
  (if (and (vectorp sort)
           (or (zerop (length sort))
               (org-files-db--presentation-sort-p (aref sort 0))))
      sort
    (unless (or (null sort) (and (listp sort) (proper-list-p sort)))
      (org-files-db--sort-user-error origin "Malformed sort value: %S" sort))
    (vconcat
     (mapcar (lambda (spec)
               (org-files-db--normalize-sort-spec
                spec columns context origin row-source))
             sort))))

(defun org-files-db--sort-includes (sort columns context &optional origin row-source)
  "Return CLI includes required by SORT for COLUMNS and CONTEXT.
ORIGIN identifies validation errors; ROW-SOURCE describes expanded rows."
  (let ((sorts (org-files-db--normalize-sort
                sort (org-files-db--normalize-columns columns)
                context origin row-source))
        includes)
    (cl-loop for spec across sorts
             for include =
             (org-files-db--presentation-column-required-include
              (org-files-db--presentation-sort-column spec))
             when (and include (not (memq include includes)))
             do (setq includes (append includes (list include))))
    includes))

(defun org-files-db--sort-missing-value-p (value)
  "Return non-nil when raw sort VALUE should be treated as missing."
  (or (null value)
      (and (stringp value) (string-empty-p value))
      (and (sequencep value) (not (stringp value)) (zerop (length value)))))

(defun org-files-db--sort-normalize-text (value case-fold)
  "Normalize textual sort VALUE, applying CASE-FOLD once."
  (if (org-files-db--sort-missing-value-p value)
      org-files-db--sort-missing
    (let ((text (org-files-db--presentation-display-value value)))
      (if case-fold (downcase text) text))))

(defun org-files-db--sort-normalize-number (value)
  "Normalize numeric sort VALUE or return the missing sentinel."
  (cond
   ((numberp value) value)
   ((and (stringp value)
         (string-match-p
          "\\`[[:space:]]*[+-]?[0-9]+\\(?:\\.[0-9]+\\)?[[:space:]]*\\'"
          value))
    (string-to-number value))
   (t org-files-db--sort-missing)))

(defun org-files-db--sort-normalize-priority (value case-fold)
  "Normalize Org priority VALUE, respecting CASE-FOLD."
  (cond
   ((characterp value)
    (org-files-db--sort-normalize-text (char-to-string value) case-fold))
   (t (org-files-db--sort-normalize-text value case-fold))))

(defun org-files-db--sort-normalize-timestamp (value)
  "Normalize Org timestamp VALUE chronologically or mark it missing."
  (if (not (and (stringp value) (not (string-empty-p value))))
      org-files-db--sort-missing
    (condition-case nil
        (float-time (org-time-string-to-time value))
      (error org-files-db--sort-missing))))

(defun org-files-db--sort-normalize-string-sequence (values case-fold)
  "Normalize string sequence VALUES once, respecting CASE-FOLD."
  (if (org-files-db--sort-missing-value-p values)
      org-files-db--sort-missing
    (let ((items (cond
                  ((vectorp values) (append values nil))
                  ((listp values) values)
                  (t (list values)))))
      (vconcat
       (mapcar (lambda (value)
                 (let ((text (org-files-db--presentation-display-value value)))
                   (if case-fold (downcase text) text)))
               items)))))

(defun org-files-db--presentation-outline-sort-value (source sort)
  "Return structural outline sort value from SOURCE for normalized SORT."
  (let* ((name (org-files-db--presentation-sort-name sort))
         (column (org-files-db--presentation-sort-column sort))
         (data
          (pcase name
            ('source-outline-path
             (org-files-db--presentation-source-source-outline-data source))
            ('target-outline-path
             (org-files-db--presentation-source-target-outline-data source))
            (_
             (if (eq (org-files-db--presentation-source-kind source) 'link)
                 (org-files-db--presentation-source-source-outline-data source)
               (org-files-db--presentation-source-outline-data source)))))
         (headings (and data (copy-sequence (cdr data))))
         (root (and data (car data))))
    (if (null data)
        org-files-db--sort-missing
      (unless (org-files-db--presentation-column-outline-include-match column)
        (setq headings (butlast headings)))
      (org-files-db--sort-normalize-string-sequence
       (if (and
            (org-files-db--presentation-column-outline-include-root column)
            root)
           (cons root headings)
         headings)
       (org-files-db--presentation-sort-case-fold sort)))))

(defun org-files-db--presentation-sort-raw-value (row sort)
  "Return ROW raw value for normalized SORT, reusing displayed cells."
  (let ((column-index (org-files-db--presentation-sort-column-index sort)))
    (if (and column-index (org-files-db--presentation-row-cells row))
        (org-files-db--presentation-cell-logical-value
         (aref (org-files-db--presentation-row-cells row) column-index))
      (let ((source (org-files-db--presentation-row-source row))
            (column (org-files-db--presentation-sort-column sort)))
        (funcall (org-files-db--presentation-column-extractor column)
                 source column)))))

(defun org-files-db--presentation-sort-key (row sort)
  "Calculate one normalized sort key for ROW according to SORT."
  (let ((kind (org-files-db--presentation-sort-kind sort)))
    (pcase kind
      ('outline
       (org-files-db--presentation-outline-sort-value
        (org-files-db--presentation-row-source row) sort))
      ('tags
       (org-files-db--sort-normalize-string-sequence
        (org-files-db--presentation-sort-raw-value row sort)
        (org-files-db--presentation-sort-case-fold sort)))
      ('number
       (org-files-db--sort-normalize-number
        (org-files-db--presentation-sort-raw-value row sort)))
      ('priority
       (org-files-db--sort-normalize-priority
        (org-files-db--presentation-sort-raw-value row sort)
        (org-files-db--presentation-sort-case-fold sort)))
      ('timestamp
       (org-files-db--sort-normalize-timestamp
        (org-files-db--presentation-sort-raw-value row sort)))
      (_
       (org-files-db--sort-normalize-text
        (org-files-db--presentation-sort-raw-value row sort)
        (org-files-db--presentation-sort-case-fold sort))))))

(defun org-files-db--presentation-prepare-sort-keys (rows sorts)
  "Calculate all SORTS once for every entry in ROWS."
  (dotimes (row-index (length rows))
    (let ((keys (make-vector (length sorts) nil))
          (row (aref rows row-index)))
      (dotimes (sort-index (length sorts))
        (aset keys sort-index
              (org-files-db--presentation-sort-key
               row (aref sorts sort-index))))
      (setf (org-files-db--presentation-row-sort-keys row) keys)))
  rows)

(defun org-files-db--sort-compare-string-sequences (left right)
  "Compare normalized string sequences LEFT and RIGHT.
Return -1, 0, or 1 without allocating comparison strings."
  (let ((index 0)
        (left-length (length left))
        (right-length (length right))
        result)
    (while (and (null result)
                (< index left-length)
                (< index right-length))
      (let ((left-value (aref left index))
            (right-value (aref right index)))
        (cond
         ((string< left-value right-value) (setq result -1))
         ((string< right-value left-value) (setq result 1))))
      (setq index (1+ index)))
    (or result
        (cond
         ((< left-length right-length) -1)
         ((> left-length right-length) 1)
         (t 0)))))

(defun org-files-db--sort-compare-values (left right kind)
  "Compare normalized LEFT and RIGHT according to KIND."
  (pcase kind
    ((or 'number 'timestamp)
     (cond ((< left right) -1) ((> left right) 1) (t 0)))
    ((or 'outline 'tags)
     (org-files-db--sort-compare-string-sequences left right))
    (_
     (cond ((string< left right) -1)
           ((string< right left) 1)
           (t 0)))))

(defun org-files-db--presentation-compare-rows (left right sorts)
  "Return non-nil when LEFT should sort before RIGHT according to SORTS."
  (let ((index 0)
        comparison)
    (while (and (zerop (or comparison 0)) (< index (length sorts)))
      (let* ((sort (aref sorts index))
             (left-value
              (aref (org-files-db--presentation-row-sort-keys left) index))
             (right-value
              (aref (org-files-db--presentation-row-sort-keys right) index))
             (left-missing (eq left-value org-files-db--sort-missing))
             (right-missing (eq right-value org-files-db--sort-missing)))
        (setq comparison
              (cond
               ((and left-missing right-missing) 0)
               (left-missing
                (if (eq (org-files-db--presentation-sort-missing sort) 'first)
                    -1 1))
               (right-missing
                (if (eq (org-files-db--presentation-sort-missing sort) 'first)
                    1 -1))
               (t
                (let ((value
                       (org-files-db--sort-compare-values
                        left-value right-value
                        (org-files-db--presentation-sort-kind sort))))
                  (if (eq (org-files-db--presentation-sort-direction sort)
                          'desc)
                      (- value)
                    value))))))
      (setq index (1+ index)))
    (if (zerop (or comparison 0))
        (< (org-files-db--presentation-row-original-position left)
           (org-files-db--presentation-row-original-position right))
      (< comparison 0))))

(defun org-files-db--presentation-sort-rows (rows sorts)
  "Sort ROWS in place according to cached SORTS and return ROWS."
  (if (zerop (length sorts))
      rows
    (cl-sort
     rows
     (lambda (left right)
       (org-files-db--presentation-compare-rows left right sorts)))))

(defun org-files-db--sort-results
    (results columns sort context &optional origin row-source)
  "Return RESULTS ordered by SORT using the shared presentation row pipeline.
COLUMNS supplies public column options used by normalized sort descriptors.
CONTEXT determines the public sort columns.  ORIGIN is used in validation
errors.  ROW-SOURCE is reserved for expanded rows."
  (if (or (null sort)
          (and (vectorp sort) (zerop (length sort))))
      results
    (let* ((normalized-columns
            (org-files-db--normalize-columns (or columns nil)))
           (sorts (org-files-db--normalize-sort
                   sort normalized-columns context origin row-source))
           (sources (org-files-db--presentation-build-sources results))
           (rows (org-files-db--presentation-build-rows sources)))
      (org-files-db--presentation-prepare-sort-keys rows sorts)
      (org-files-db--presentation-sort-rows rows sorts)
      (mapcar #'org-files-db--presentation-row-result (append rows nil)))))

(defun org-files-db--format-outline-data-for-column (data column)
  "Format outline DATA using normalized presentation COLUMN."
  (if (null data)
      ""
    (let ((headings (cdr data))
          (root (car data)))
      (unless (org-files-db--presentation-column-outline-include-match column)
        (setq headings (butlast headings)))
      (string-join
       (if (and
            (org-files-db--presentation-column-outline-include-root column)
            root)
           (cons root headings)
         headings)
       (org-files-db--presentation-column-outline-separator column)))))

(defun org-files-db--presentation-extract-todo-keyword (source _column)
  "Extract the TODO keyword from SOURCE."
  (org-files-db--presentation-source-todo-keyword source))

(defun org-files-db--presentation-extract-todo-type (source _column)
  "Extract the TODO type from SOURCE."
  (org-files-db--presentation-source-todo-type source))

(defun org-files-db--presentation-extract-priority (source _column)
  "Extract the priority from SOURCE."
  (org-files-db--presentation-source-priority source))

(defun org-files-db--presentation-extract-title (source _column)
  "Extract the title from SOURCE."
  (org-files-db--presentation-source-title source))

(defun org-files-db--presentation-extract-outline-path (source column)
  "Extract an outline path from SOURCE using COLUMN."
  (org-files-db--format-outline-data-for-column
   (if (eq (org-files-db--presentation-source-kind source) 'link)
       (org-files-db--presentation-source-source-outline-data source)
     (org-files-db--presentation-source-outline-data source))
   column))

(defun org-files-db--presentation-extract-source-outline-path (source column)
  "Extract a link source outline path from SOURCE using COLUMN."
  (org-files-db--format-outline-data-for-column
   (org-files-db--presentation-source-source-outline-data source)
   column))

(defun org-files-db--presentation-extract-target-outline-path (source column)
  "Extract a resolved target outline path from SOURCE using COLUMN."
  (org-files-db--format-outline-data-for-column
   (org-files-db--presentation-source-target-outline-data source)
   column))

(defun org-files-db--presentation-extract-tags (source _column)
  "Extract aggregate tags from SOURCE."
  (org-files-db--presentation-source-tags source))

(defun org-files-db--presentation-extract-file-name (source _column)
  "Extract the file name from SOURCE."
  (org-files-db--presentation-source-file-name source))

(defun org-files-db--presentation-extract-file-title (source _column)
  "Extract the file title from SOURCE."
  (org-files-db--presentation-source-file-title source))

(defun org-files-db--presentation-extract-file-path (source _column)
  "Extract the file path from SOURCE."
  (org-files-db--presentation-source-file-path source))

(defun org-files-db--presentation-extract-line-number (source _column)
  "Extract the line number from SOURCE."
  (org-files-db--presentation-source-line-number source))

(defun org-files-db--presentation-extract-byte-start (source _column)
  "Extract the byte start from SOURCE."
  (org-files-db--presentation-source-byte-start source))

(defun org-files-db--presentation-extract-byte-end (source _column)
  "Extract the byte end from SOURCE."
  (org-files-db--get
   (org-files-db--presentation-source-result source)
   'location 'byte_end))

(defun org-files-db--presentation-extract-file-id (source _column)
  "Extract the file identifier from SOURCE."
  (org-files-db--presentation-source-raw-value source 'file_id))

(defun org-files-db--presentation-extract-parent-id (source _column)
  "Extract the parent identifier from SOURCE."
  (org-files-db--presentation-source-raw-value source 'parent_id))

(defun org-files-db--presentation-extract-scheduled-raw (source _column)
  "Extract the raw scheduled value from SOURCE."
  (org-files-db--presentation-source-raw-value source 'scheduled_raw))

(defun org-files-db--presentation-extract-deadline-raw (source _column)
  "Extract the raw deadline value from SOURCE."
  (org-files-db--presentation-source-raw-value source 'deadline_raw))

(defun org-files-db--presentation-extract-closed-raw (source _column)
  "Extract the raw closed value from SOURCE."
  (org-files-db--presentation-source-raw-value source 'closed_raw))

(defun org-files-db--presentation-extract-link-type (source _column)
  "Extract the link type from SOURCE."
  (org-files-db--presentation-source-raw-value source 'link_type))

(defun org-files-db--presentation-extract-link-path (source _column)
  "Extract the parsed link path from SOURCE."
  (org-files-db--presentation-source-raw-value source 'link_path))

(defun org-files-db--presentation-extract-resolution-status (source _column)
  "Extract the resolution status from SOURCE."
  (org-files-db--presentation-source-raw-value source 'resolution_status))

(defun org-files-db--presentation-extract-raw-target (source _column)
  "Extract the raw link target from SOURCE."
  (org-files-db--presentation-source-raw-value source 'raw_target))

(defun org-files-db--presentation-extract-raw-description (source _column)
  "Extract the raw link description from SOURCE."
  (org-files-db--presentation-source-raw-value source 'raw_description))

(defun org-files-db--presentation-extract-generic (source column)
  "Extract COLUMN's generic raw value from SOURCE."
  (org-files-db--presentation-source-raw-value
   source
   (org-files-db--presentation-column-name column)))

(defun org-files-db--presentation-todo-face (source _column)
  "Return the TODO face for SOURCE."
  (if (equal (org-files-db--presentation-source-todo-type source) "closed")
      'org-done
    'org-todo))

(defconst org-files-db--level-faces
  [org-level-1 org-level-2 org-level-3 org-level-4
               org-level-5 org-level-6 org-level-7 org-level-8]
  "Org heading faces indexed by zero-based presentation level.")

(defun org-files-db--presentation-level-face (source _column)
  "Return the Org outline level face for SOURCE."
  (let ((level (or (org-files-db--presentation-source-level source) 1)))
    (aref org-files-db--level-faces (1- (max 1 (min 8 level))))))

(defun org-files-db--presentation-display-value (value)
  "Return logical VALUE as a compact display string."
  (cond
   ((null value) "")
   ((or (listp value) (vectorp value))
    (org-files-db--format-list-value value))
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun org-files-db--column-value (result definition)
  "Return RESULT value for column DEFINITION as a string."
  (let* ((column (org-files-db--normalize-column definition))
         (source
          (make-org-files-db--presentation-source :result result))
         (value
          (funcall (org-files-db--presentation-column-extractor column)
                   source column)))
    (org-files-db--presentation-display-value value)))

(defun org-files-db--column-face (result column)
  "Return a suitable Org face for RESULT COLUMN."
  (let* ((definition (if (consp column) column (list column)))
         (normalized (org-files-db--normalize-column definition))
         (source
          (make-org-files-db--presentation-source :result result)))
    (or (org-files-db--presentation-column-face normalized)
        (when-let* ((function
                     (org-files-db--presentation-column-face-function
                      normalized)))
          (funcall function source normalized)))))

(defun org-files-db--sanitized-face (face)
  "Return a face plist based on FACE without size, weight, or slant."
  (when (facep face)
    (let (plist)
      (dolist (attribute '(:foreground :background :underline :overline
                                       :strike-through :box :inverse-video
                                       :extend))
        (let ((value (face-attribute face attribute nil t)))
          (unless (eq value 'unspecified)
            (push attribute plist)
            (push value plist))))
      (nreverse plist))))

(defun org-files-db--presentation-build-sources (results)
  "Return a source vector for RESULTS."
  (let* ((count (length results))
         (sources (make-vector count nil))
         (index 0))
    (mapc
     (lambda (result)
       (aset sources index
             (make-org-files-db--presentation-source :result result))
       (setq index (1+ index)))
     results)
    sources))

(defun org-files-db--presentation-build-rows (sources)
  "Return one empty presentation row for each entry in SOURCES."
  (let ((rows (make-vector (length sources) nil)))
    (cl-loop for source across sources
             for index from 0
             for result = (org-files-db--presentation-source-result source)
             do (aset rows index
                      (make-org-files-db--presentation-row
                       :result result
                       :source source
                       :original-position index)))
    rows))

(defun org-files-db--presentation-column-source-face (source column)
  "Return SOURCE face for normalized COLUMN."
  (or (org-files-db--presentation-column-face column)
      (when-let* ((function
                   (org-files-db--presentation-column-face-function column)))
        (funcall function source column))))

(defun org-files-db--presentation-populate-cells (rows columns)
  "Calculate and cache every displayed cell in ROWS for COLUMNS."
  (let ((row-count (length rows))
        (column-count (length columns)))
    (dotimes (row-index row-count)
      (let* ((row (aref rows row-index))
             (source (org-files-db--presentation-row-source row))
             (cells (make-vector column-count nil)))
        (dotimes (column-index column-count)
          (let* ((column (aref columns column-index))
                 (value
                  (funcall
                   (org-files-db--presentation-column-extractor column)
                   source column))
                 (display (org-files-db--presentation-display-value value)))
            (aset
             cells column-index
             (make-org-files-db--presentation-cell
              :logical-value value
              :display display
              :display-width (string-width display)
              :source-face
              (and (> (length display) 0)
                   (org-files-db--presentation-column-source-face
                    source column))))))
        (setf (org-files-db--presentation-row-cells row) cells))))
  rows)

(defun org-files-db--presentation-populate-cells-widths-and-faces
    (rows columns)
  "Populate ROWS for COLUMNS and return widths and a sanitized face cache.
Each displayed value, display width, source face, and final face is calculated
once.  Width accumulation stops for fixed columns and for maximum-width
columns as soon as their limit is reached."
  (let* ((row-count (length rows))
         (column-count (length columns))
         (widths (make-vector column-count 1))
         (width-active (make-vector column-count t))
         (face-cache (make-hash-table :test #'eq)))
    (dotimes (column-index column-count)
      (let ((column (aref columns column-index)))
        (when (eq (org-files-db--presentation-column-width-kind column)
                  'fixed)
          (aset widths column-index
                (org-files-db--presentation-column-width-limit column))
          (aset width-active column-index nil))))
    (dotimes (row-index row-count)
      (let* ((row (aref rows row-index))
             (source (org-files-db--presentation-row-source row))
             (cells (make-vector column-count nil)))
        (dotimes (column-index column-count)
          (let* ((column (aref columns column-index))
                 (value
                  (funcall
                   (org-files-db--presentation-column-extractor column)
                   source column))
                 (display (org-files-db--presentation-display-value value))
                 (display-width (string-width display))
                 (source-face
                  (and (> (length display) 0)
                       (org-files-db--presentation-column-source-face
                        source column)))
                 (face
                  (when source-face
                    (let ((cached
                           (gethash
                            source-face face-cache
                            org-files-db--presentation-uncomputed)))
                      (if (eq cached org-files-db--presentation-uncomputed)
                          (let ((sanitized
                                 (org-files-db--sanitized-face source-face)))
                            (puthash source-face sanitized face-cache)
                            sanitized)
                        cached)))))
            (aset
             cells column-index
             (make-org-files-db--presentation-cell
              :logical-value value
              :display display
              :display-width display-width
              :source-face source-face
              :face face))
            (when (aref width-active column-index)
              (let ((kind
                     (org-files-db--presentation-column-width-kind column))
                    (limit
                     (org-files-db--presentation-column-width-limit column)))
                (when (> display-width (aref widths column-index))
                  (aset widths column-index display-width))
                (when (and (eq kind 'max)
                           (>= (aref widths column-index) limit))
                  (aset widths column-index limit)
                  (aset width-active column-index nil))))))
        (setf (org-files-db--presentation-row-cells row) cells)))
    (cons widths face-cache)))

(defun org-files-db--presentation-calculate-widths (rows columns)
  "Return final shared widths for cached ROWS and normalized COLUMNS."
  (let* ((column-count (length columns))
         (row-count (length rows))
         (widths (make-vector column-count 1)))
    (dotimes (column-index column-count)
      (let* ((column (aref columns column-index))
             (kind (org-files-db--presentation-column-width-kind column))
             (limit (org-files-db--presentation-column-width-limit column)))
        (if (eq kind 'fixed)
            (aset widths column-index limit)
          (let ((needed 1)
                (row-index 0)
                (maximum-p (eq kind 'max)))
            (while (and (< row-index row-count)
                        (or (not maximum-p) (< needed limit)))
              (let* ((row (aref rows row-index))
                     (cell
                      (aref (org-files-db--presentation-row-cells row)
                            column-index))
                     (cell-width
                      (org-files-db--presentation-cell-display-width cell)))
                (when (> cell-width needed)
                  (setq needed cell-width)))
              (setq row-index (1+ row-index)))
            (aset widths column-index
                  (if maximum-p (min needed limit) needed))))))
    widths))

(defun org-files-db--presentation-prepare-faces (rows)
  "Sanitize distinct source faces once for ROWS and return the face cache."
  (let ((cache (make-hash-table :test #'eq))
        (row-count (length rows)))
    (dotimes (row-index row-count)
      (let* ((row (aref rows row-index))
             (cells (org-files-db--presentation-row-cells row))
             (cell-count (length cells)))
        (dotimes (cell-index cell-count)
          (let* ((cell (aref cells cell-index))
                 (source-face
                  (org-files-db--presentation-cell-source-face cell)))
            (when source-face
              (let ((face
                     (gethash source-face cache
                              org-files-db--presentation-uncomputed)))
                (when (eq face org-files-db--presentation-uncomputed)
                  (setq face (org-files-db--sanitized-face source-face))
                  (puthash source-face face cache))
                (setf (org-files-db--presentation-cell-face cell) face)))))))
    cache))

(defun org-files-db--string-suffix-to-width (string width &optional total-width)
  "Return the longest suffix of STRING whose display width is at most WIDTH.
TOTAL-WIDTH may supply the previously calculated display width of STRING."
  (let ((total-width (or total-width (string-width string))))
    (truncate-string-to-width
     string total-width (max 0 (- total-width width)))))

(defun org-files-db--truncate-presentation-value
    (value value-width width column)
  "Truncate VALUE of VALUE-WIDTH to WIDTH using normalized COLUMN."
  (if (<= value-width width)
      value
    (let* ((position
            (org-files-db--presentation-column-truncate-position column))
           (marker
            (org-files-db--presentation-column-truncate-marker column))
           (marker-width
            (org-files-db--presentation-column-truncate-marker-width column)))
      (if (>= marker-width width)
          (truncate-string-to-width marker width)
        (let* ((available (- width marker-width))
               (left-width (/ (+ available 1) 2))
               (right-width (- available left-width)))
          (pcase position
            ('left
             (concat marker
                     (org-files-db--string-suffix-to-width
                      value available value-width)))
            ('middle
             (concat (truncate-string-to-width value left-width)
                     marker
                     (org-files-db--string-suffix-to-width
                      value right-width value-width)))
            (_
             (concat (truncate-string-to-width value available)
                     marker))))))))

(defun org-files-db--truncate-column-value (value width definition)
  "Truncate VALUE to WIDTH according to column DEFINITION."
  (let ((column (org-files-db--normalize-column definition)))
    (org-files-db--truncate-presentation-value
     value (string-width value) width column)))

(defun org-files-db--format-presentation-cell (cell column width)
  "Format cached CELL with normalized COLUMN to WIDTH."
  (let* ((display (org-files-db--presentation-cell-display cell))
         (display-width
          (org-files-db--presentation-cell-display-width cell))
         (text
          (org-files-db--truncate-presentation-value
           display display-width width column))
         (text-width
          (if (eq text display)
              display-width
            (string-width text)))
         (padding (max 0 (- width text-width)))
         (face (org-files-db--presentation-cell-face cell)))
    (concat (if face (propertize text 'face face) text)
            (make-string padding ?\s))))

(defconst org-files-db--presentation-padding-vector-limit 4096
  "Largest padding width stored in a direct vector cache.")

(defun org-files-db--presentation-make-padding-cache (widths)
  "Return an efficient padding cache suitable for presentation WIDTHS."
  (let ((maximum 0))
    (dotimes (index (length widths))
      (setq maximum (max maximum (aref widths index))))
    (if (<= maximum org-files-db--presentation-padding-vector-limit)
        (make-vector (1+ maximum) nil)
      (make-hash-table :test #'eql))))

(defun org-files-db--format-presentation-row (row columns widths)
  "Format cached ROW once using normalized COLUMNS and WIDTHS."
  (org-files-db--format-presentation-row-data
   row columns widths
   (org-files-db--presentation-make-padding-cache widths)))

(defun org-files-db--presentation-padding (width cache)
  "Return WIDTH spaces reused through presentation CACHE."
  (cond
   ((zerop width) "")
   ((vectorp cache)
    (or (aref cache width)
        (let ((padding (make-string width ?\s)))
          (aset cache width padding)
          padding)))
   (t
    (or (gethash width cache)
        (let ((padding (make-string width ?\s)))
          (puthash width padding cache)
          padding)))))

(defun org-files-db--format-presentation-row-data
    (row columns widths padding-cache)
  "Return one complete propertized completion string for prepared ROW.
COLUMNS and WIDTHS describe the table, and PADDING-CACHE reuses space strings.
The returned string keeps complete cell values for matching.  Visually
truncated cells use `display' properties instead of removing logical text."
  (let ((column-count (length columns)))
    (if (= column-count 1)
        (org-files-db--format-single-presentation-row-data
         row (aref columns 0) (aref widths 0) padding-cache)
      (org-files-db--format-multiple-presentation-row-data
       row columns widths padding-cache))))

(defun org-files-db--format-presentation-cell-segment
    (cell column width padding-cache)
  "Return one searchable, visually formatted segment for CELL.
COLUMN and WIDTH describe the presentation column.  PADDING-CACHE reuses
space strings.  When CELL exceeds WIDTH, keep the complete underlying value
and attach the truncated representation with a `display' text property."
  (let* ((value (org-files-db--presentation-cell-display cell))
         (value-width (org-files-db--presentation-cell-display-width cell))
         (face (org-files-db--presentation-cell-face cell)))
    (if (> value-width width)
        (let* ((text
                (org-files-db--truncate-presentation-value
                 value value-width width column))
               (text-width (string-width text))
               (padding-width (max 0 (- width text-width)))
               (replacement
                (concat text
                        (org-files-db--presentation-padding
                         padding-width padding-cache)))
               (segment (copy-sequence value)))
          (when (and face (> (length segment) 0))
            (add-text-properties 0 (length segment) (list 'face face) segment))
          (when (and face (> (length text) 0))
            (add-text-properties 0 (length text) (list 'face face) replacement))
          (when (> (length segment) 0)
            (add-text-properties
             0 (length segment) (list 'display replacement) segment))
          segment)
      (let* ((padding-width (max 0 (- width value-width)))
             (segment
              (concat value
                      (org-files-db--presentation-padding
                       padding-width padding-cache))))
        (when (and face (> (length value) 0))
          (add-text-properties 0 (length value) (list 'face face) segment))
        segment))))

(defun org-files-db--format-single-presentation-row-data
    (row column width padding-cache)
  "Return one searchable, visually formatted candidate body for ROW.
COLUMN and WIDTH control formatting, and PADDING-CACHE reuses space strings."
  (org-files-db--format-presentation-cell-segment
   (aref (org-files-db--presentation-row-cells row) 0)
   column width padding-cache))

(defun org-files-db--format-multiple-presentation-row-data
    (row columns widths padding-cache)
  "Return one searchable, visually formatted candidate body for ROW.
COLUMNS and WIDTHS control formatting, and PADDING-CACHE reuses space strings."
  (let* ((column-count (length columns))
         (cells (org-files-db--presentation-row-cells row))
         segments)
    (dotimes (column-index column-count)
      (push
       (org-files-db--format-presentation-cell-segment
        (aref cells column-index)
        (aref columns column-index)
        (aref widths column-index)
        padding-cache)
       segments)
      (when (< column-index (1- column-count))
        (push "  " segments)))
    (apply #'concat (nreverse segments))))

(defconst org-files-db--candidate-identity-base #x1900
  "Number of Unicode private-use characters used for candidate identities.")

(defun org-files-db--candidate-identity (index)
  "Return a compact identity suffix for zero-based INDEX."
  (let ((value (1+ index))
        characters)
    ;; Build the character sequence first.  Mutating a multibyte string with
    ;; `aset' is unsafe when replacement characters use a different internal
    ;; byte width, even when both are non-ASCII characters.
    (while (> value 0)
      (push (+ #xe000 (% value org-files-db--candidate-identity-base))
            characters)
      (setq value (/ value org-files-db--candidate-identity-base)))
    (apply #'string #x2063 characters)))

(defun org-files-db--candidate-identity-index (candidate)
  "Return the zero-based identity encoded at the end of CANDIDATE."
  (when (stringp candidate)
    (let* ((end (length candidate))
           (start end))
      (while (and (> start 0)
                  (let ((character (aref candidate (1- start))))
                    (and (>= character #xe000)
                         (< character
                            (+ #xe000 org-files-db--candidate-identity-base)))))
        (setq start (1- start)))
      (when (and (< start end)
                 (> start 0)
                 (= (aref candidate (1- start)) #x2063))
        (let ((value 0)
              (position start))
          (while (< position end)
            (setq value
                  (+ (* value org-files-db--candidate-identity-base)
                     (- (aref candidate position) #xe000))
                  position (1+ position)))
          (and (> value 0) (1- value)))))))

(defun org-files-db--presentation-format-candidates (rows columns widths)
  "Format ROWS once using COLUMNS and WIDTHS, and return a result vector."
  (let* ((row-count (length rows))
         (lookup (make-vector row-count nil))
         (padding-cache
          (org-files-db--presentation-make-padding-cache widths))
         (candidates (make-list row-count nil))
         (candidate-tail candidates))
    (dotimes (index row-count)
      (let* ((row (aref rows index))
             (result (org-files-db--presentation-row-result row))
             (row-source (org-files-db--presentation-row-row-source row))
             (candidate-body
              (org-files-db--format-presentation-row-data
               row columns widths padding-cache))
             (candidate-body-length (length candidate-body))
             (candidate
              (concat candidate-body
                      (org-files-db--candidate-identity index)))
             (properties
              (list 'org-files-db-result result
                    'org-files-db-visible-length candidate-body-length
                    'consult--candidate result
                    'rear-nonsticky t)))
        (when row-source
          (setq properties
                (append
                 properties
                 (list
                  'org-files-db-presentation-row row
                  'org-files-db-row-source row-source
                  'org-files-db-row-value
                  (org-files-db--presentation-row-row-value row)))))
        (add-text-properties 0 candidate-body-length properties candidate)
        (add-text-properties
         candidate-body-length (length candidate)
         '(display "" invisible t)
         candidate)
        (aset lookup index result)
        (setcar candidate-tail candidate)
        (setq candidate-tail (cdr candidate-tail))))
    (cons candidates lookup)))

(defun org-files-db--elapsed-seconds (started)
  "Return elapsed seconds since floating-point time STARTED."
  (- (float-time) started))

(defvar org-files-db--presentation-allocation-metrics-function nil
  "Optional function used to measure presentation allocation.
The function is called without arguments to return a starting snapshot and
with that snapshot after each measured phase to return its allocation data.")

(defmacro org-files-db--measure-presentation-phase
    (phase timing-variable metrics-variable &rest body)
  "Measure PHASE while evaluating BODY.
Set TIMING-VARIABLE and add details to METRICS-VARIABLE."
  (declare (indent 3) (debug t))
  `(let* ((started (float-time))
          (gcs-before (if (boundp 'gcs-done) gcs-done 0))
          (gc-time-before (if (boundp 'gc-elapsed) gc-elapsed 0.0))
          (allocation-function
           org-files-db--presentation-allocation-metrics-function)
          (allocation-before
           (and allocation-function (funcall allocation-function))))
     (prog1 (progn ,@body)
       (setq ,timing-variable (org-files-db--elapsed-seconds started))
       (push
        (list :phase ,phase
              :elapsed ,timing-variable
              :garbage-collections
              (- (if (boundp 'gcs-done) gcs-done 0) gcs-before)
              :garbage-collection-time
              (- (if (boundp 'gc-elapsed) gc-elapsed 0.0) gc-time-before)
              :allocation
              (and allocation-before
                   (funcall allocation-function allocation-before)))
        ,metrics-variable))))

(defun org-files-db--prepare-presentation-1
    (results columns &optional sort context origin row-source)
  "Eagerly prepare RESULTS for completion using COLUMNS without GC policy.
SORT is an optional user-facing sort specification for CONTEXT.  ORIGIN is
included in sort validation errors.  ROW-SOURCE is reserved for expanded rows."
  (let ((total-started (float-time))
        normalized-columns normalized-sorts sources rows widths face-cache
        candidates lookup
        column-normalization source-construction row-construction
        value-extraction sort-key-preparation sorting width-calculation
        face-preparation
        candidate-formatting phase-metrics)
    (setq normalized-columns
          (org-files-db--measure-presentation-phase
              :column-normalization column-normalization phase-metrics
            (org-files-db--normalize-columns columns)))
    (setq normalized-sorts
          (if sort
              (org-files-db--normalize-sort
               sort normalized-columns context origin row-source)
            []))
    (setq sources
          (org-files-db--measure-presentation-phase
              :presentation-source-construction
              source-construction phase-metrics
            (org-files-db--presentation-build-sources results)))
    (setq rows
          (org-files-db--measure-presentation-phase
              :presentation-row-construction row-construction phase-metrics
            (org-files-db--presentation-build-rows sources)))
    (if (zerop (length normalized-sorts))
        (progn
          (org-files-db--measure-presentation-phase
              :result-value-extraction value-extraction phase-metrics
            (pcase-let ((`(,prepared-widths . ,prepared-face-cache)
                         (org-files-db--presentation-populate-cells-widths-and-faces
                          rows normalized-columns)))
              (setq widths prepared-widths
                    face-cache prepared-face-cache))
            nil)
          (setq sort-key-preparation 0.0
                sorting 0.0
                width-calculation 0.0
                face-preparation 0.0))
      (org-files-db--measure-presentation-phase
          :result-value-extraction value-extraction phase-metrics
        (org-files-db--presentation-populate-cells rows normalized-columns))
      (org-files-db--measure-presentation-phase
          :sort-key-preparation sort-key-preparation phase-metrics
        (org-files-db--presentation-prepare-sort-keys rows normalized-sorts))
      (setq rows
            (org-files-db--measure-presentation-phase
                :sorting sorting phase-metrics
              (org-files-db--presentation-sort-rows rows normalized-sorts)))
      (setq widths
            (org-files-db--measure-presentation-phase
                :shared-width-calculation width-calculation phase-metrics
              (org-files-db--presentation-calculate-widths
               rows normalized-columns)))
      (setq face-cache
            (org-files-db--measure-presentation-phase
                :face-preparation face-preparation phase-metrics
              (org-files-db--presentation-prepare-faces rows))))
    (org-files-db--measure-presentation-phase
        :candidate-formatting candidate-formatting phase-metrics
      (pcase-let ((`(,prepared-candidates . ,prepared-lookup)
                   (org-files-db--presentation-format-candidates
                    rows normalized-columns widths)))
        (setq candidates prepared-candidates
              lookup prepared-lookup)))
    (when candidates
      (puthash candidates lookup org-files-db--candidate-lookups))
    (make-org-files-db--presentation
     :columns normalized-columns
     :sources sources
     :rows rows
     :widths widths
     :face-cache face-cache
     :candidates candidates
     :lookup lookup
     :phase-metrics (nreverse phase-metrics)
     :timings
     (list :column-normalization column-normalization
           :presentation-source-construction source-construction
           :presentation-row-construction row-construction
           :result-value-extraction value-extraction
           :sort-key-preparation sort-key-preparation
           :sorting sorting
           :shared-width-calculation width-calculation
           :face-preparation face-preparation
           :candidate-formatting candidate-formatting
           :boundary-garbage-collection 0.0
           :total (org-files-db--elapsed-seconds total-started)))))

(defun org-files-db--prepare-presentation
    (results columns &optional sort context origin row-source)
  "Eagerly prepare RESULTS for completion using COLUMNS and optional SORT.
CONTEXT and ORIGIN describe validation context; ROW-SOURCE describes expanded
rows.  Large preparations receive bounded temporary allocation headroom.
Discard intermediate source, row, and face-cache references before completion,
but do not force a full garbage collection on the interactive path."
  (if (< (length results) org-files-db--large-presentation-row-count)
      (org-files-db--prepare-presentation-1
       results columns sort context origin row-source)
    (let* ((bounded-gc-p
            (and (< gc-cons-threshold
                    org-files-db--large-presentation-gc-threshold)
                 (< gc-cons-percentage 1.0)))
           (started (float-time))
           (gc-cons-threshold
            (if bounded-gc-p
                org-files-db--large-presentation-gc-threshold
              gc-cons-threshold))
           (presentation
            (org-files-db--prepare-presentation-1
             results columns sort context origin row-source))
           (boundary-started (float-time))
           (boundary-gcs-before
            (if (boundp 'gcs-done) gcs-done 0))
           (boundary-gc-time-before
            (if (boundp 'gc-elapsed) gc-elapsed 0.0)))
      (setf (org-files-db--presentation-sources presentation) nil
            (org-files-db--presentation-rows presentation) nil
            (org-files-db--presentation-face-cache presentation) nil)
      (let* ((boundary-time
              (org-files-db--elapsed-seconds boundary-started))
             (boundary-gcs
              (- (if (boundp 'gcs-done) gcs-done 0)
                 boundary-gcs-before))
             (boundary-gc-time
              (- (if (boundp 'gc-elapsed) gc-elapsed 0.0)
                 boundary-gc-time-before))
             (timings
              (org-files-db--presentation-timings presentation)))
        (setf (org-files-db--presentation-timings presentation)
              (plist-put
               (plist-put timings :boundary-garbage-collection boundary-time)
               :total (org-files-db--elapsed-seconds started)))
        (setf (org-files-db--presentation-phase-metrics presentation)
              (nconc
               (org-files-db--presentation-phase-metrics presentation)
               (list
                (list :phase :boundary-garbage-collection
                      :elapsed boundary-time
                      :garbage-collections boundary-gcs
                      :garbage-collection-time boundary-gc-time
                      :allocation nil))))
        presentation))))

(defun org-files-db--column-widths (results columns)
  "Calculate shared widths for RESULTS and COLUMNS from cached cells."
  (let* ((normalized (org-files-db--normalize-columns columns))
         (sources (org-files-db--presentation-build-sources results))
         (rows (org-files-db--presentation-build-rows sources))
         (prepared
          (org-files-db--presentation-populate-cells-widths-and-faces
           rows normalized)))
    (append (car prepared) nil)))

(defun org-files-db--format-cell (result definition width)
  "Format RESULT column DEFINITION to WIDTH."
  (let* ((columns (org-files-db--normalize-columns (list definition)))
         (sources (org-files-db--presentation-build-sources (list result)))
         (rows (org-files-db--presentation-build-rows sources)))
    (org-files-db--presentation-populate-cells-widths-and-faces rows columns)
    (org-files-db--format-presentation-cell
     (aref (org-files-db--presentation-row-cells (aref rows 0)) 0)
     (aref columns 0)
     width)))

(defun org-files-db--format-result (result columns widths)
  "Format RESULT according to COLUMNS and WIDTHS."
  (let* ((normalized (org-files-db--normalize-columns columns))
         (sources (org-files-db--presentation-build-sources (list result)))
         (rows (org-files-db--presentation-build-rows sources))
         (width-vector (if (vectorp widths) widths (vconcat widths))))
    (org-files-db--presentation-populate-cells-widths-and-faces rows normalized)
    (org-files-db--format-presentation-row
     (aref rows 0) normalized width-vector)))

(defun org-files-db--make-candidates
    (results columns &optional sort context origin row-source)
  "Return completion candidates for RESULTS using COLUMNS and optional SORT.
CONTEXT and ORIGIN describe validation context; ROW-SOURCE describes expanded
rows."
  (org-files-db--presentation-candidates
   (org-files-db--prepare-presentation
    results columns sort context origin row-source)))

(defun org-files-db--completion-table (candidates)
  "Return a completion table for CANDIDATES."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata
          (category . ,org-files-db--completion-category)
          (display-sort-function . identity)
          (cycle-sort-function . identity))
      (complete-with-action action candidates string predicate))))

(defun org-files-db--candidate-result (selected candidates)
  "Return the original result for SELECTED among CANDIDATES."
  (or (and (stringp selected)
           (> (length selected) 0)
           (get-text-property 0 'org-files-db-result selected))
      (when-let* ((index
                   (org-files-db--candidate-identity-index selected)))
        (let ((lookup
               (gethash candidates org-files-db--candidate-lookups)))
          (unless lookup
            (setq lookup (make-vector (length candidates) nil))
            (let ((position 0))
              (dolist (candidate candidates)
                (aset lookup position
                      (and (> (length candidate) 0)
                           (get-text-property
                            0 'org-files-db-result candidate)))
                (setq position (1+ position))))
            (when candidates
              (puthash candidates lookup org-files-db--candidate-lookups)))
          (and (< index (length lookup))
               (aref lookup index))))))

(defun org-files-db--read-presentation (presentation &optional prompt)
  "Read one result from prepared PRESENTATION using PROMPT."
  (let ((candidates (org-files-db--presentation-candidates presentation)))
    (unless candidates
      (user-error "The query returned no results"))
    (let ((selected
           (completing-read
            (or prompt "Result: ")
            (org-files-db--completion-table candidates)
            nil t)))
      (or (org-files-db--candidate-result selected candidates)
          (user-error "Selected result is no longer available")))))

(defun org-files-db--read-result (results columns &optional prompt)
  "Read one result from RESULTS displayed with COLUMNS using PROMPT."
  (unless results
    (user-error "The query returned no results"))
  (org-files-db--read-presentation
   (org-files-db--prepare-presentation results columns)
   prompt))

(defun org-files-db--default-columns (target results)
  "Return default columns for TARGET and RESULTS."
  (pcase target
    ('headings org-files-db-heading-columns)
    ('files org-files-db-file-columns)
    ('links org-files-db-link-columns)
    ('search org-files-db-search-columns)
    (_
     (pcase (org-files-db--kind (car results))
       ((or 'heading 'root) org-files-db-heading-columns)
       ('file org-files-db-file-columns)
       ('link org-files-db-link-columns)
       (_ org-files-db-heading-columns)))))

(defun org-files-db--query-target (query)
  "Infer the orgfdb query target from QUERY."
  (let* ((form (if (stringp query)
                   (condition-case nil
                       (let ((read-eval nil))
                         (car (read-from-string query)))
                     (error nil))
                 query))
         (head (car-safe form)))
    (if (memq head '(headings links files)) head 'headings)))

(defun org-files-db--fetch-query
    (query columns config-file &optional origin sort)
  "Fetch QUERY using COLUMNS, effective CONFIG-FILE, and optional SORT.
Return a plist containing normalized :results, :columns, :sort, :target, and
:includes.  ORIGIN identifies configuration and CLI errors."
  (let* ((requested-columns columns)
         (inferred-target (org-files-db--query-target query))
         (initial-columns
          (or requested-columns
              (org-files-db--default-columns inferred-target nil)))
         (normalized-columns
          (org-files-db--normalize-columns initial-columns))
         (request-sort
          (org-files-db--normalize-sort
           sort normalized-columns inferred-target origin))
         (includes
          (delete-dups
           (append
            (org-files-db--column-includes normalized-columns)
            (org-files-db--sort-includes
             request-sort normalized-columns inferred-target origin))))
         (response
          (org-files-db--execute-query
           query config-file (or origin "Query") includes))
         (results
          (org-files-db--results-with-config
           (org-files-db--normalize-results response)
           config-file))
         (target (or (org-files-db--response-target response)
                     inferred-target))
         (final-columns
          (if (or requested-columns (eq target inferred-target))
              normalized-columns
            (org-files-db--normalize-columns
             (org-files-db--default-columns target results))))
         (final-sort
          (if (and (eq target inferred-target)
                   (eq final-columns normalized-columns))
              request-sort
            (org-files-db--normalize-sort sort final-columns target origin))))
    (list :results results
          :columns final-columns
          :sort final-sort
          :target target
          :includes includes)))

(defun org-files-db--fetch-search
    (expression columns scope config-file &optional origin sort)
  "Fetch EXPRESSION using COLUMNS, SCOPE, effective CONFIG-FILE, and SORT.
Return a plist containing normalized :results, :columns, and :sort.  ORIGIN
identifies configuration and CLI errors."
  (let* ((response
          (org-files-db--execute-search
           expression scope config-file (or origin "Search")))
         (results
          (org-files-db--results-with-config
           (org-files-db--normalize-results response)
           config-file))
         (normalized-columns
          (org-files-db--normalize-columns
           (or columns
               (org-files-db--default-columns 'search results))))
         (normalized-sort
          (org-files-db--normalize-sort
           sort normalized-columns 'search origin)))
    (list :results results
          :columns normalized-columns
          :sort normalized-sort
          :scope scope)))

(defun org-files-db--present-presentation (presentation action &optional prompt)
  "Present prepared PRESENTATION, then apply ACTION using PROMPT."
  (let ((result (org-files-db--read-presentation presentation prompt)))
    (funcall (or action org-files-db-query-action) result)
    result))

(defun org-files-db--present-results
    (results columns action &optional prompt sort context origin row-source)
  "Present RESULTS in COLUMNS, then apply ACTION using PROMPT and optional SORT.
CONTEXT and ORIGIN describe validation context; ROW-SOURCE describes expanded
rows."
  (org-files-db--present-presentation
   (org-files-db--prepare-presentation
    results columns sort context origin row-source)
   action prompt))

;;;###autoload
(cl-defun org-files-db-check-setup
    (&key (config-file nil config-file-supplied-p))
  "Check the orgfdb executable, configuration, and read-only database access.
When CONFIG-FILE is omitted, inherit `org-files-db-config-file'; an explicit
nil disables --config for the read-only database check."
  (interactive)
  (let* ((executable (org-files-db--resolve-executable))
         (config (org-files-db--resolve-config-file
                  config-file config-file-supplied-p "Setup check"))
         (version (string-trim
                   (org-files-db--call-raw '("--version"))))
         (read-check
          (condition-case err
              (progn
                (org-files-db--call
                 "headings"
                 (append '("--format" "json" "--no-root")
                         (org-files-db--config-arguments
                          config "Setup check")))
                "ok")
            (error (error-message-string err))))
         (report
          `((executable . ,executable)
            (config . ,config)
            (version . ,version)
            (read-check . ,read-check))))
    (when (called-interactively-p 'interactive)
      (with-current-buffer (get-buffer-create "*org-files-db setup*")
        (erase-buffer)
        (insert (format "Executable: %s\n" executable))
        (insert (format "Configuration: %s\n" (or config "<none>")))
        (insert (format "Version: %s\n" version))
        (insert (format "Read-only check: %s\n" read-check))
        (goto-char (point-min))
        (special-mode)
        (display-buffer (current-buffer))))
    report))

(provide 'org-files-db-core)

;;; org-files-db-core.el ends here
