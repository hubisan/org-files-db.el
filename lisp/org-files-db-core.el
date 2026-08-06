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

(define-error 'org-files-db-error "org-files-db error")
(define-error 'org-files-db-cli-error "orgfdb command failed" 'org-files-db-error)
(define-error 'org-files-db-cli-usage-error
              "Invalid orgfdb command usage" 'org-files-db-cli-error)

(defconst org-files-db--completion-category 'org-files-db-result)

(defconst org-files-db--result-config-key 'org-files-db--config-file
  "Internal result key containing the effective orgfdb configuration file.")

(defvar org-files-db-query-history nil
  "Minibuffer history for orgfdb query expressions.")

(defconst org-files-db--missing-key (make-symbol "org-files-db-missing-key")
  "Sentinel used to distinguish missing JSON object keys from nil values.")

(defun org-files-db--alist-value (object key)
  "Return from OBJECT the value associated with KEY.
OBJECT may be an alist or hash table, and its keys may be symbols or strings."
  (let* ((symbol-key (if (symbolp key) key (intern key)))
         (string-key (if (stringp key) key (symbol-name key))))
    (cond
     ((hash-table-p object)
      (or (gethash symbol-key object)
          (gethash string-key object)))
     ((listp object)
      (or (alist-get symbol-key object)
          (alist-get string-key object nil nil #'string=)))
     (t nil))))

(defun org-files-db--has-key-p (object key)
  "Return non-nil when OBJECT contains KEY."
  (let ((symbol-key (if (symbolp key) key (intern key)))
        (string-key (if (stringp key) key (symbol-name key))))
    (cond
     ((hash-table-p object)
      (or (not (eq (gethash symbol-key object org-files-db--missing-key)
                   org-files-db--missing-key))
          (not (eq (gethash string-key object org-files-db--missing-key)
                   org-files-db--missing-key))))
     ((listp object)
      (or (assq symbol-key object)
          (assoc string-key object #'string=)))
     (t nil))))

(defun org-files-db--get (object &rest keys)
  "Return the nested value in OBJECT selected by KEYS."
  (dolist (key keys object)
    (setq object (org-files-db--alist-value object key))))

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

(defun org-files-db--run-process (arguments)
  "Run orgfdb synchronously with ARGUMENTS.
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

(defun org-files-db--call-raw (arguments)
  "Run orgfdb with ARGUMENTS and return stdout."
  (pcase-let* ((result (org-files-db--run-process arguments))
               (status (plist-get result :status))
               (stdout (plist-get result :stdout))
               (stderr (plist-get result :stderr)))
    (if (zerop status)
        stdout
      (org-files-db--signal-cli-error status stderr))))

(defun org-files-db--parse-json-as (text object-type)
  "Parse JSON TEXT using OBJECT-TYPE and list arrays."
  (condition-case err
      (json-parse-string text
                         :object-type object-type
                         :array-type 'list
                         :null-object nil
                         :false-object nil)
    (error
     (signal 'org-files-db-error
             (list (format "Invalid JSON from orgfdb: %s"
                           (error-message-string err)))))))

(defun org-files-db--parse-json (text)
  "Parse JSON TEXT into alists and lists."
  (org-files-db--parse-json-as text 'alist))

(defun org-files-db--call (command arguments)
  "Run orgfdb COMMAND with ARGUMENTS and parse its JSON output."
  (org-files-db--parse-json
   (org-files-db--call-raw (cons command arguments))))

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
    (org-files-db--get response 'results))
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
    (cons (cons org-files-db--result-config-key config-file)
          (assq-delete-all org-files-db--result-config-key
                           (copy-sequence result))))
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
  (when-let* ((root
               (seq-find
                (lambda (node)
                  (and (or (listp node) (hash-table-p node))
                       (memq (org-files-db--kind node) '(file root))))
                nodes)))
    (org-files-db--node-title root)))

(defun org-files-db--path-heading-titles (nodes)
  "Return heading titles represented by NODES."
  (delq nil
        (mapcar
         (lambda (node)
           (let* ((object-p (or (listp node) (hash-table-p node)))
                  (kind (and object-p (org-files-db--kind node))))
             (when (or (stringp node)
                       (eq kind 'heading)
                       (and object-p (null kind)))
               (let ((title (org-files-db--node-title node)))
                 (unless (string-empty-p title) title)))))
         nodes)))

(defun org-files-db--append-heading-title (headings title)
  "Append TITLE to HEADINGS unless it is already the final component."
  (if (or (not (and (stringp title) (not (string-empty-p title))))
          (equal (car (last headings)) title))
      headings
    (append headings (list title))))

(defun org-files-db--outline-data (root headings)
  "Return normalized outline data for ROOT and HEADINGS."
  (list :root (and (stringp root) (not (string-empty-p root)) root)
        :headings (delq nil headings)))

(defun org-files-db--generic-outline-data (result)
  "Return generic outline data for RESULT."
  (let* ((nodes (or (org-files-db--result-path-nodes result) nil))
         (root (org-files-db--path-root-title nodes))
         (headings (org-files-db--path-heading-titles nodes)))
    (unless headings
      (let ((title (org-files-db--result-title result)))
        (setq headings (list title))
        (when (equal root title)
          (setq root nil))))
    (org-files-db--outline-data root headings)))

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
         (headings (org-files-db--path-heading-titles nodes))
         (heading-title
          (or (org-files-db--get heading 'title)
              (when (eq (org-files-db--kind source) 'heading)
                (org-files-db--get source 'title))))
         (root
          (or (org-files-db--get source 'file 'title)
              (org-files-db--path-root-title nodes))))
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
           (headings (org-files-db--path-heading-titles nodes))
           (heading-title (org-files-db--get heading 'title))
           (root
            (or (org-files-db--get file 'title)
                (org-files-db--path-root-title nodes))))
      (when (or file heading nodes)
        (org-files-db--outline-data
         root
         (org-files-db--append-heading-title headings heading-title))))))

(defun org-files-db--format-outline-data (data definition)
  "Format outline DATA according to column DEFINITION."
  (if (null data)
      ""
    (let* ((options (org-files-db--outline-options definition))
           (headings (copy-sequence (plist-get data :headings)))
           (root (plist-get data :root))
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
  (string-join
   (mapcar (lambda (item)
             (if (stringp item) item (format "%s" item)))
           value)
   ","))

(defconst org-files-db--presentation-uncomputed
  (make-symbol "org-files-db-presentation-uncomputed")
  "Sentinel for presentation-source values that have not been calculated.")

(defvar org-files-db--candidate-lookups
  (make-hash-table :test #'eq :weakness 'key)
  "Weak map from prepared candidate lists to direct result lookup tables.")

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

(cl-defstruct org-files-db--presentation
  "Complete eagerly prepared result presentation."
  columns
  sources
  rows
  widths
  face-cache
  candidates
  lookup
  timings)

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

(defun org-files-db--format-outline-data-for-column (data column)
  "Format outline DATA using normalized presentation COLUMN."
  (if (null data)
      ""
    (let ((headings (copy-sequence (plist-get data :headings)))
          (root (plist-get data :root)))
      (unless (org-files-db--presentation-column-outline-include-match column)
        (setq headings (butlast headings)))
      (string-join
       (append
        (when (and
               (org-files-db--presentation-column-outline-include-root column)
               root)
          (list root))
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

(defun org-files-db--presentation-level-face (source _column)
  "Return the `outline-level' face for SOURCE."
  (let ((level
         (max 1
              (min 8
                   (or (org-files-db--presentation-source-level source)
                       1)))))
    (intern (format "org-level-%d" level))))

(defun org-files-db--presentation-display-value (value)
  "Return logical VALUE as a compact display string."
  (cond
   ((null value) "")
   ((listp value) (org-files-db--format-list-value value))
   ((stringp value) value)
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
  (cl-loop for row across rows
           for source = (org-files-db--presentation-row-source row)
           for cells = (make-vector (length columns) nil)
           do (cl-loop for column across columns
                       for column-index from 0
                       for value = (funcall
                                    (org-files-db--presentation-column-extractor
                                     column)
                                    source column)
                       for display = (org-files-db--presentation-display-value
                                      value)
                       do (aset
                           cells column-index
                           (make-org-files-db--presentation-cell
                            :logical-value value
                            :display display
                            :display-width (string-width display)
                            :source-face
                            (org-files-db--presentation-column-source-face
                             source column))))
           do (setf (org-files-db--presentation-row-cells row) cells))
  rows)

(defun org-files-db--presentation-calculate-widths (rows columns)
  "Return final shared widths for cached ROWS and normalized COLUMNS."
  (let ((widths (make-vector (length columns) 1)))
    (cl-loop for column across columns
             for column-index from 0
             for kind = (org-files-db--presentation-column-width-kind column)
             for limit = (org-files-db--presentation-column-width-limit column)
             do (if (eq kind 'fixed)
                    (aset widths column-index limit)
                  (let ((needed 1)
                        (row-index 0)
                        (row-count (length rows)))
                    (while (and (< row-index row-count)
                                (not (and (eq kind 'max)
                                          (>= needed limit))))
                      (let* ((row (aref rows row-index))
                             (cell
                              (aref (org-files-db--presentation-row-cells row)
                                    column-index)))
                        (setq needed
                              (max
                               needed
                               (org-files-db--presentation-cell-display-width
                                cell))))
                      (setq row-index (1+ row-index)))
                    (aset widths column-index
                          (if (eq kind 'max)
                              (min needed limit)
                            needed)))))
    widths))

(defun org-files-db--presentation-prepare-faces (rows)
  "Sanitize distinct source faces once for ROWS and return the face cache."
  (let ((cache (make-hash-table :test #'eq)))
    (cl-loop for row across rows
             for cells = (org-files-db--presentation-row-cells row)
             do (cl-loop for cell across cells
                         for source-face =
                         (org-files-db--presentation-cell-source-face cell)
                         when source-face
                         do (let ((face
                                   (gethash
                                    source-face cache
                                    org-files-db--presentation-uncomputed)))
                              (when (eq face
                                        org-files-db--presentation-uncomputed)
                                (setq face
                                      (org-files-db--sanitized-face source-face))
                                (puthash source-face face cache))
                              (setf
                               (org-files-db--presentation-cell-face cell)
                               face))))
    cache))

(defun org-files-db--string-suffix-to-width (string width)
  "Return the longest suffix of STRING whose display width is at most WIDTH."
  (let ((position (length string))
        (used 0)
        done)
    (while (and (> position 0) (not done))
      (let* ((previous (1- position))
             (character-width
              (string-width (substring string previous position))))
        (if (> (+ used character-width) width)
            (setq done t)
          (setq used (+ used character-width)
                position previous))))
    (substring string position)))

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
                     (org-files-db--string-suffix-to-width value available)))
            ('middle
             (concat (truncate-string-to-width value left-width)
                     marker
                     (org-files-db--string-suffix-to-width value right-width)))
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

(defun org-files-db--format-presentation-row (row columns widths)
  "Format cached ROW once using normalized COLUMNS and WIDTHS."
  (let* ((column-count (length columns))
         (parts (make-vector column-count nil))
         (cells (org-files-db--presentation-row-cells row)))
    (dotimes (column-index column-count)
      (aset parts column-index
            (org-files-db--format-presentation-cell
             (aref cells column-index)
             (aref columns column-index)
             (aref widths column-index))))
    (mapconcat #'identity parts "  ")))

(defconst org-files-db--candidate-identity-base #x1900
  "Number of Unicode private-use characters used for candidate identities.")

(defun org-files-db--presentation-search-suffix (row widths)
  "Return hidden untruncated search text for prepared ROW and WIDTHS."
  (let ((cells (org-files-db--presentation-row-cells row))
        values)
    (dotimes (index (length cells))
      (let ((cell (aref cells index)))
        (when (> (org-files-db--presentation-cell-display-width cell)
                 (aref widths index))
          (push (org-files-db--presentation-cell-display cell) values))))
    (when values
      (propertize
       (concat "\u2063" (mapconcat #'identity (nreverse values) "\u2063"))
       'display ""
       'invisible t))))

(defun org-files-db--candidate-identity (index)
  "Return a compact hidden identity suffix for zero-based INDEX."
  (let ((value (1+ index))
        characters)
    (while (> value 0)
      (push (+ #xe000 (% value org-files-db--candidate-identity-base))
            characters)
      (setq value (/ value org-files-db--candidate-identity-base)))
    (propertize
     (apply #'string #x2063 characters)
     'display ""
     'invisible t)))

(defun org-files-db--presentation-format-candidates (rows columns widths)
  "Format ROWS once using COLUMNS and WIDTHS, and return a direct lookup."
  (let ((lookup (make-hash-table :test #'equal))
        candidates)
    (dotimes (index (length rows))
      (let* ((row (aref rows index))
             (result (org-files-db--presentation-row-result row))
             (display
              (org-files-db--format-presentation-row row columns widths))
             (candidate
              (concat display
                      (or (org-files-db--presentation-search-suffix row widths) "")
                      (org-files-db--candidate-identity index))))
        (add-text-properties
         0 (length candidate)
         (list 'org-files-db-result result
               'org-files-db-visible-length (length display)
               'org-files-db-presentation-row row
               'org-files-db-row-source
               (org-files-db--presentation-row-row-source row)
               'org-files-db-row-value
               (org-files-db--presentation-row-row-value row)
               'consult--candidate result
               'rear-nonsticky t)
         candidate)
        (puthash candidate result lookup)
        (push candidate candidates)))
    (cons (nreverse candidates) lookup)))

(defun org-files-db--elapsed-seconds (started)
  "Return elapsed seconds since floating-point time STARTED."
  (- (float-time) started))

(defun org-files-db--prepare-presentation (results columns)
  "Eagerly prepare RESULTS for completion using COLUMNS."
  (let ((total-started (float-time))
        normalized-columns sources rows widths face-cache candidates lookup
        column-normalization source-construction row-construction
        value-extraction width-calculation face-preparation
        candidate-formatting)
    (let ((started (float-time)))
      (setq normalized-columns (org-files-db--normalize-columns columns)
            column-normalization (org-files-db--elapsed-seconds started)))
    (let ((started (float-time)))
      (setq sources (org-files-db--presentation-build-sources results)
            source-construction (org-files-db--elapsed-seconds started)))
    (let ((started (float-time)))
      (setq rows (org-files-db--presentation-build-rows sources)
            row-construction (org-files-db--elapsed-seconds started)))
    (let ((started (float-time)))
      (org-files-db--presentation-populate-cells rows normalized-columns)
      (setq value-extraction (org-files-db--elapsed-seconds started)))
    (let ((started (float-time)))
      (setq widths
            (org-files-db--presentation-calculate-widths
             rows normalized-columns)
            width-calculation (org-files-db--elapsed-seconds started)))
    (let ((started (float-time)))
      (setq face-cache (org-files-db--presentation-prepare-faces rows)
            face-preparation (org-files-db--elapsed-seconds started)))
    (let ((started (float-time)))
      (pcase-let ((`(,prepared-candidates . ,prepared-lookup)
                   (org-files-db--presentation-format-candidates
                    rows normalized-columns widths)))
        (setq candidates prepared-candidates
              lookup prepared-lookup
              candidate-formatting
              (org-files-db--elapsed-seconds started))))
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
     :timings
     (list :column-normalization column-normalization
           :presentation-source-construction source-construction
           :presentation-row-construction row-construction
           :result-value-extraction value-extraction
           :sort-key-preparation 0.0
           :sorting 0.0
           :shared-width-calculation width-calculation
           :face-preparation face-preparation
           :candidate-formatting candidate-formatting
           :total (org-files-db--elapsed-seconds total-started)))))

(defun org-files-db--column-widths (results columns)
  "Calculate shared widths for RESULTS and COLUMNS from cached cells."
  (let* ((normalized (org-files-db--normalize-columns columns))
         (sources (org-files-db--presentation-build-sources results))
         (rows (org-files-db--presentation-build-rows sources)))
    (org-files-db--presentation-populate-cells rows normalized)
    (append
     (org-files-db--presentation-calculate-widths rows normalized)
     nil)))

(defun org-files-db--format-cell (result definition width)
  "Format RESULT column DEFINITION to WIDTH."
  (let* ((columns (org-files-db--normalize-columns (list definition)))
         (sources (org-files-db--presentation-build-sources (list result)))
         (rows (org-files-db--presentation-build-rows sources)))
    (org-files-db--presentation-populate-cells rows columns)
    (org-files-db--presentation-prepare-faces rows)
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
    (org-files-db--presentation-populate-cells rows normalized)
    (org-files-db--presentation-prepare-faces rows)
    (org-files-db--format-presentation-row
     (aref rows 0) normalized width-vector)))

(defun org-files-db--make-candidates (results columns)
  "Return completion candidates for RESULTS using COLUMNS."
  (org-files-db--presentation-candidates
   (org-files-db--prepare-presentation results columns)))

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
      (let* ((lookup
              (gethash candidates org-files-db--candidate-lookups))
             (value
              (and lookup
                   (gethash selected lookup
                            org-files-db--presentation-uncomputed))))
        (unless lookup
          (setq lookup (make-hash-table :test #'equal))
          (dolist (candidate candidates)
            (when-let* ((result
                         (get-text-property
                          0 'org-files-db-result candidate)))
              (puthash candidate result lookup)))
          (when candidates
            (puthash candidates lookup org-files-db--candidate-lookups))
          (setq value
                (gethash selected lookup
                         org-files-db--presentation-uncomputed)))
        (unless (eq value org-files-db--presentation-uncomputed)
          value))))

(defun org-files-db--read-result (results columns &optional prompt)
  "Read one result from RESULTS displayed with COLUMNS using PROMPT."
  (unless results
    (user-error "The query returned no results"))
  (let* ((presentation
          (org-files-db--prepare-presentation results columns))
         (candidates
          (org-files-db--presentation-candidates presentation))
         (selected
          (completing-read
           (or prompt "Result: ")
           (org-files-db--completion-table candidates)
           nil t)))
    (or (org-files-db--candidate-result selected candidates)
        (user-error "Selected result is no longer available"))))

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

(defun org-files-db--present-results (results columns action &optional prompt)
  "Present RESULTS in COLUMNS, then apply ACTION using PROMPT."
  (let ((result (org-files-db--read-result results columns prompt)))
    (funcall (or action org-files-db-query-action) result)
    result))

(defconst org-files-db--benchmark-phases
  '(:column-normalization
    :presentation-source-construction
    :presentation-row-construction
    :result-value-extraction
    :sort-key-preparation
    :sorting
    :shared-width-calculation
    :face-preparation
    :candidate-formatting
    :total)
  "Presentation timing phases reported by benchmark helpers.")

(defun org-files-db--benchmark-phase-summary (runs phase)
  "Return timing statistics for PHASE across benchmark RUNS."
  (let* ((values
          (sort
           (mapcar (lambda (run) (plist-get run phase)) runs)
           #'<))
         (count (length values))
         (middle (/ count 2))
         (median
          (if (cl-oddp count)
              (nth middle values)
            (/ (+ (nth (1- middle) values)
                  (nth middle values))
               2.0))))
    (list :minimum (car values)
          :median median
          :maximum (car (last values)))))

(cl-defun org-files-db--benchmark-presentation
    (results columns &key (iterations 5))
  "Benchmark eager presentation of RESULTS with COLUMNS.
Return minimum, median, and maximum timings across ITERATIONS without opening
completion."
  (unless (and (integerp iterations) (> iterations 0))
    (user-error "Benchmark iterations must be a positive integer"))
  (let ((gc-before (if (boundp 'gcs-done)
                       (symbol-value 'gcs-done)
                     0))
        runs)
    (dotimes (_ iterations)
      (push
       (org-files-db--presentation-timings
        (org-files-db--prepare-presentation results columns))
       runs))
    (setq runs (nreverse runs))
    (list :result-count (length results)
          :column-count (length columns)
          :iterations iterations
          :garbage-collections
          (- (if (boundp 'gcs-done)
                 (symbol-value 'gcs-done)
               0)
             gc-before)
          :phases
          (mapcar
           (lambda (phase)
             (cons phase
                   (org-files-db--benchmark-phase-summary runs phase)))
           org-files-db--benchmark-phases)
          :runs runs)))

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
