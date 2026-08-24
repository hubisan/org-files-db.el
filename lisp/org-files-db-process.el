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

;; Public executable and configuration options for the rebuilt client.
;; Short-lived process execution is added in the next rebuild step.

;;; Code:

(require 'org)
(require 'subr-x)

(defgroup org-files-db nil
  "Emacs interface for orgfdb."
  :group 'org
  :prefix "org-files-db-")

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
            (user-error "org-files-db-default-config is not configured"))
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
      (user-error "org-files-db configuration is not readable: %s" file))
    file))

(provide 'org-files-db-process)

;;; org-files-db-process.el ends here
