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
  "Configuration file passed to orgfdb.
When nil, do not pass the --config option."
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

(defvar org-files-db-query-history nil
  "Minibuffer history for Query Model expressions.")

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

(defun org-files-db--validated-config-file ()
  "Return the expanded configured file name, or nil."
  (when org-files-db-config-file
    (unless (stringp org-files-db-config-file)
      (user-error "Org-files-db-config-file must be a file name or nil"))
    (let ((file (expand-file-name org-files-db-config-file)))
      (unless (file-readable-p file)
        (user-error "Orgfdb configuration file is not readable: %s" file))
      file)))

(defun org-files-db--config-arguments ()
  "Return command arguments for the configured orgfdb file."
  (when-let* ((file (org-files-db--validated-config-file)))
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

(defun org-files-db--parse-json (text)
  "Parse JSON TEXT into alists and lists."
  (condition-case err
      (json-parse-string text
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil)
    (error
     (signal 'org-files-db-error
             (list (format "Invalid JSON from orgfdb: %s"
                           (error-message-string err)))))))

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
  "Return QUERY in the textual Query Model v0 representation."
  (cond
   ((stringp query)
    (if (string-empty-p (string-trim query))
        (user-error "Query must not be empty")
      query))
   ((consp query) (prin1-to-string query))
   (t (user-error "Query must be a Query Model v0 list or string"))))

(defun org-files-db--query-arguments (query)
  "Return command arguments for Query Model v0 QUERY."
  (append '("--format" "json" "--output" "flat" "--include" "path")
          (org-files-db--config-arguments)
          (list (org-files-db--query-string query))))

(defun org-files-db--execute-query (query)
  "Execute Query Model v0 QUERY and return the response envelope."
  (org-files-db--call "query" (org-files-db--query-arguments query)))

(defun org-files-db--validate-search-scope (scope)
  "Return validated search SCOPE."
  (let ((scope (or scope 'all)))
    (unless (memq scope '(all title body))
      (user-error "Unsupported orgfdb search scope: %S" scope))
    scope))

(defun org-files-db--search-arguments (expression &optional scope)
  "Return orgfdb arguments for EXPRESSION and SCOPE."
  (unless (and (stringp expression)
               (not (string-empty-p (string-trim expression))))
    (user-error "Search expression must be a non-empty string"))
  (let ((scope (org-files-db--validate-search-scope scope)))
    (append '("--format" "json")
            (pcase scope
              ('title '("--title"))
              ('body '("--body"))
              (_ nil))
            (org-files-db--config-arguments)
            (list expression))))

(defun org-files-db--execute-search (expression &optional scope)
  "Execute one FTS5 search for EXPRESSION in SCOPE."
  (org-files-db--call
   "search"
   (org-files-db--search-arguments expression scope)))

(defun org-files-db--normalize-results (response)
  "Return a result list from orgfdb RESPONSE."
  (cond
   ((null response) nil)
   ((org-files-db--has-key-p response 'results)
    (org-files-db--get response 'results))
   ((and (listp response)
         (listp (car response)))
    response)
   (t
    (signal 'org-files-db-error
            (list "Unexpected orgfdb JSON response shape")))))

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
            (response
             (org-files-db--execute-query
              `(headings (property "ID" ,id :inherit nil))))
            (results (org-files-db--normalize-results response)))
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
   ((listp node)
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
      (org-files-db--get result 'heading_path)))

(defun org-files-db--outline-path (result)
  "Return RESULT outline path as a display string.
Synthetic file/root nodes are omitted from the displayed heading path."
  (let* ((nodes (org-files-db--result-path-nodes result))
         (nodes
          (seq-remove
           (lambda (node)
             (memq (org-files-db--kind node) '(file root)))
           nodes)))
    (if nodes
        (string-join (mapcar #'org-files-db--node-title nodes) " » ")
      (org-files-db--result-title result))))

(defun org-files-db--format-list-value (value)
  "Format list VALUE as a compact string."
  (string-join
   (mapcar (lambda (item)
             (if (stringp item) item (format "%s" item)))
           value)
   ","))

(defun org-files-db--column-value (result column)
  "Return RESULT value for COLUMN as a string."
  (let ((value
         (pcase column
           ('todo-keyword (org-files-db--get result 'todo_keyword))
           ('todo-type (org-files-db--get result 'todo_type))
           ('priority (org-files-db--get result 'priority))
           ('title (org-files-db--result-title result))
           ('outline-path (org-files-db--outline-path result))
           ('source-outline-path (org-files-db--outline-path result))
           ('tags (or (org-files-db--get result 'all_tags)
                      (org-files-db--get result 'tags)))
           ('file-name
            (when-let* ((file (org-files-db--result-file result)))
              (file-name-nondirectory file)))
           ('file-title
            (or (org-files-db--get result 'title)
                (when-let* ((file (org-files-db--result-file result)))
                  (file-name-base file))))
           ('file-path (org-files-db--result-file result))
           ('line-number (org-files-db--result-line result))
           ('byte-start (org-files-db--result-byte-start result))
           ('byte-end (org-files-db--get result 'location 'byte_end))
           ('file-id (org-files-db--get result 'file_id))
           ('parent-id (org-files-db--get result 'parent_id))
           ('scheduled-raw (org-files-db--get result 'scheduled_raw))
           ('deadline-raw (org-files-db--get result 'deadline_raw))
           ('closed-raw (org-files-db--get result 'closed_raw))
           ('link-type (org-files-db--get result 'link_type))
           ('link-target (org-files-db--get result 'raw_target))
           ('link-description (org-files-db--get result 'raw_description))
           ('link-path (org-files-db--get result 'link_path))
           ('raw-target (org-files-db--get result 'raw_target))
           ('raw-description (org-files-db--get result 'raw_description))
           ('status (org-files-db--get result 'resolution_status))
           ('resolution-status (org-files-db--get result 'resolution_status))
           ('rank (org-files-db--get result 'rank))
           (_ (org-files-db--get result column)))))
    (cond
     ((null value) "")
     ((listp value) (org-files-db--format-list-value value))
     ((stringp value) value)
     (t (format "%s" value)))))

(defun org-files-db--column-face (result column)
  "Return a suitable Org face for RESULT COLUMN."
  (pcase column
    ('todo-keyword
     (if (equal (org-files-db--get result 'todo_type) "closed")
         'org-done
       'org-todo))
    ('priority 'org-priority)
    ('tags 'org-tag)
    ((or 'scheduled-raw 'deadline-raw 'closed-raw 'date) 'org-date)
    ((or 'title 'outline-path 'source-outline-path)
     (let* ((level (or (org-files-db--get result 'level)
                       (org-files-db--get result 'heading_level)
                       1))
            (level (max 1 (min 8 level))))
       (intern (format "org-level-%d" level))))
    (_ nil)))

(defun org-files-db--sanitized-face (face)
  "Return a face plist based on FACE without size, weight, or slant."
  (when (facep face)
    (let (plist)
      (dolist (attribute '(:foreground :background :underline :overline
                                       :strike-through :box :inverse-video :extend))
        (let ((value (face-attribute face attribute nil t)))
          (unless (eq value 'unspecified)
            (setq plist (append plist (list attribute value))))))
      plist)))

(defun org-files-db--column-width-spec (definition)
  "Return the width specification from column DEFINITION."
  (or (plist-get (cdr definition) :width) 'auto))

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

(defun org-files-db--column-widths (results columns)
  "Calculate shared widths for RESULTS and COLUMNS."
  (mapcar
   (lambda (definition)
     (let* ((column (car definition))
            (spec (org-files-db--validate-width-spec
                   (org-files-db--column-width-spec definition)
                   column))
            (needed
             (max 1
                  (seq-reduce
                   #'max
                   (mapcar (lambda (result)
                             (string-width
                              (org-files-db--column-value result column)))
                           results)
                   1))))
       (pcase spec
         ('auto needed)
         (`(max ,n) (min needed n))
         (`(fixed ,n) n))))
   columns))

(defun org-files-db--format-cell (result column width)
  "Format RESULT COLUMN to WIDTH."
  (let* ((raw (org-files-db--column-value result column))
         (text (if (> (string-width raw) width)
                   (truncate-string-to-width raw width nil nil "…")
                 raw))
         (padding (max 0 (- width (string-width text))))
         (face (org-files-db--sanitized-face
                (org-files-db--column-face result column))))
    (concat (if face (propertize text 'face face) text)
            (make-string padding ?\s))))

(defun org-files-db--format-result (result columns widths)
  "Format RESULT according to COLUMNS and WIDTHS."
  (string-join
   (cl-mapcar
    (lambda (definition width)
      (org-files-db--format-cell result (car definition) width))
    columns widths)
   "  "))

(defun org-files-db--make-candidates (results columns)
  "Return completion candidates for RESULTS using COLUMNS."
  (let ((widths (org-files-db--column-widths results columns))
        (index 0))
    (mapcar
     (lambda (result)
       (let* ((display (org-files-db--format-result result columns widths))
              (suffix (propertize (format "\u2063%d" (cl-incf index))
                                  'display ""
                                  'invisible t))
              (candidate (concat display suffix)))
         (add-text-properties
          0 (length candidate)
          (list 'org-files-db-result result
                'consult--candidate result
                'rear-nonsticky t)
          candidate)
         candidate))
     results)))

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
  (when-let* ((candidate (car (member selected candidates))))
    (get-text-property 0 'org-files-db-result candidate)))

(defun org-files-db--read-result (results columns &optional prompt)
  "Read one result from RESULTS displayed with COLUMNS using PROMPT."
  (unless results
    (user-error "The query returned no results"))
  (let* ((candidates (org-files-db--make-candidates results columns))
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

;;;###autoload
(defun org-files-db-check-setup ()
  "Check the orgfdb executable, configuration, and read-only database access."
  (interactive)
  (let* ((executable (org-files-db--resolve-executable))
         (config (org-files-db--validated-config-file))
         (version (string-trim
                   (org-files-db--call-raw '("--version"))))
         (read-check
          (condition-case err
              (progn
                (org-files-db--call
                 "headings"
                 (append '("--format" "json" "--no-root")
                         (org-files-db--config-arguments)))
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
