;;; org-files-db-process.el --- Process foundation for org-files-db -*- lexical-binding: t; -*-

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

;; Executable, configuration, and short-lived orgfdb process support.
;; Watcher process management is separate because the watcher is long-lived.

;;; Code:

(require 'json)
(require 'org)
(require 'subr-x)

(defgroup org-files-db nil
  "Emacs interface for orgfdb."
  :group 'org
  :prefix "org-files-db-")

(define-error 'org-files-db-error "org-files-db error")
(define-error 'org-files-db-cli-error
              "orgfdb command failed" 'org-files-db-error)
(define-error 'org-files-db-cli-usage-error
              "Invalid orgfdb command usage" 'org-files-db-cli-error)

(defcustom org-files-db-executable "orgfdb"
  "Path or command name of the orgfdb executable."
  :type 'string
  :group 'org-files-db)

(defcustom org-files-db-configs nil
  "Named orgfdb configuration files.
Each entry has the form (NAME . FILE).  NAME is a unique non-empty string.
FILE is the configuration file for that name."
  :type '(alist :key-type string :value-type file)
  :group 'org-files-db)

(defcustom org-files-db-default-config nil
  "Name of the default entry in `org-files-db-configs'."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'org-files-db)

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
      (user-error "Cannot find orgfdb executable: %s"
                  org-files-db-executable))
    (unless (file-executable-p path)
      (user-error "Orgfdb executable is not executable: %s" path))
    path))

(defun org-files-db--validated-configs ()
  "Return validated `org-files-db-configs'.
Signal a user error for invalid entries or duplicate names.
If configurations exist, require a valid `org-files-db-default-config'."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (entry org-files-db-configs)
      (unless (and (consp entry)
                   (stringp (car entry))
                   (not (string-empty-p (car entry)))
                   (stringp (cdr entry))
                   (not (string-empty-p (cdr entry))))
        (user-error "Invalid org-files-db configuration entry: %S" entry))
      (when (gethash (car entry) seen)
        (user-error "Duplicate org-files-db configuration name: %s" (car entry)))
      (puthash (car entry) t seen))
    (if org-files-db-configs
        (progn
          (unless (and (stringp org-files-db-default-config)
                       (not (string-empty-p org-files-db-default-config)))
            (user-error "Org-files-db-default-config is not configured"))
          (unless (gethash org-files-db-default-config seen)
            (user-error "Unknown default org-files-db configuration: %s"
                        org-files-db-default-config)))
      (when org-files-db-default-config
        (user-error "Unknown default org-files-db configuration: %s"
                    org-files-db-default-config)))
    org-files-db-configs))

(defun org-files-db--config-entry (name)
  "Return the configuration entry for NAME.
Signal a user error if NAME does not exist."
  (org-files-db--validated-configs)
  (or (assoc name org-files-db-configs)
      (user-error "Unknown org-files-db configuration: %s" name)))

(defun org-files-db--config-name (&optional name)
  "Return a validated configuration NAME.
Use `org-files-db-default-config' when NAME is nil."
  (let ((effective-name (or name org-files-db-default-config)))
    (unless (and (stringp effective-name)
                 (not (string-empty-p effective-name)))
      (user-error "No org-files-db configuration is selected"))
    (car (org-files-db--config-entry effective-name))))

(defun org-files-db--config-file (&optional name)
  "Return the readable configuration file for NAME.
Use `org-files-db-default-config' when NAME is nil."
  (let* ((effective-name (org-files-db--config-name name))
         (entry (org-files-db--config-entry effective-name))
         (file (expand-file-name (cdr entry))))
    (unless (and (file-regular-p file) (file-readable-p file))
      (user-error "Org-files-db configuration is not readable: %s" file))
    file))

(defun org-files-db--config-arguments (&optional name)
  "Return orgfdb configuration arguments for configuration NAME.
Use `org-files-db-default-config' when NAME is nil."
  (list "--config" (org-files-db--config-file name)))

(defun org-files-db--read-config-name ()
  "Read and return one configured orgfdb configuration name."
  (let* ((configs (org-files-db--validated-configs))
         (names (mapcar #'car configs)))
    (unless names
      (user-error "No org-files-db configurations are configured"))
    (org-files-db--config-name
     (completing-read "orgfdb configuration: " names nil t nil nil
                      org-files-db-default-config))))

(defun org-files-db--interactive-config-name (&optional prefix)
  "Return the configuration name for an interactive command.
When PREFIX is non-nil, let the user select a configured name.
Otherwise, return `org-files-db-default-config'."
  (if prefix
      (org-files-db--read-config-name)
    (org-files-db--config-name)))

(defun org-files-db--buffer-string (buffer)
  "Return BUFFER contents without text properties."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-files-db--run-process (arguments &optional input)
  "Run orgfdb synchronously with ARGUMENTS and optional standard INPUT.
Return a plist with :status, :stdout, and :stderr."
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
          (when (and input (process-live-p process))
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
  "Return an orgfdb error message for STATUS and STDERR."
  (let ((text (string-trim (or stderr ""))))
    (if (string-empty-p text)
        (format "orgfdb exited with status %d" status)
      (format "orgfdb exited with status %d: %s" status text))))

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
  "Run orgfdb with ARGUMENTS and optional standard INPUT.
Return stdout as a string."
  (let* ((result (org-files-db--run-process arguments input))
         (status (plist-get result :status))
         (stdout (plist-get result :stdout))
         (stderr (plist-get result :stderr)))
    (if (zerop status)
        stdout
      (org-files-db--signal-cli-error status stderr))))

(defun org-files-db--parse-json (text)
  "Parse JSON TEXT into alists and vectors."
  (condition-case err
      (json-parse-string text
                         :object-type 'alist
                         :array-type 'array
                         :null-object nil
                         :false-object :false)
    (error
     (signal 'org-files-db-error
             (list (format "Invalid JSON from orgfdb: %s"
                           (error-message-string err)))))))

(defun org-files-db--call-json (arguments &optional input)
  "Run orgfdb with ARGUMENTS and optional standard INPUT.
Parse and return the JSON stdout."
  (org-files-db--parse-json
   (org-files-db--call-raw arguments input)))

;;;###autoload
(defun org-files-db-check-setup (&optional config-name)
  "Check orgfdb and the selected configuration CONFIG-NAME.
Use the default configuration when CONFIG-NAME is nil.
With an interactive prefix argument, let the user select a configuration."
  (interactive (list (org-files-db--interactive-config-name
                      current-prefix-arg)))
  (let* ((name (org-files-db--config-name config-name))
         (executable (org-files-db--resolve-executable))
         (config-file (org-files-db--config-file name))
         (version (string-trim
                   (org-files-db--call-raw '("--version"))))
         (read-check
          (condition-case err
              (progn
                (org-files-db--call-json
                 (append '("status" "--format" "json")
                         (org-files-db--config-arguments name)))
                "ok")
            (error (error-message-string err))))
         (report
          `((executable . ,executable)
            (config . ,name)
            (config-file . ,config-file)
            (version . ,version)
            (read-check . ,read-check))))
    (when (called-interactively-p 'interactive)
      (with-current-buffer (get-buffer-create "*org-files-db setup*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Executable: %s\n" executable))
          (insert (format "Configuration: %s\n" name))
          (insert (format "Configuration file: %s\n" config-file))
          (insert (format "Version: %s\n" version))
          (insert (format "Read-only check: %s\n" read-check))
          (goto-char (point-min))
          (special-mode))
        (display-buffer (current-buffer))))
    report))

(provide 'org-files-db-process)

;;; org-files-db-process.el ends here
