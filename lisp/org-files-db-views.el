;;; org-files-db-views.el --- Named view foundation for org-files-db -*- lexical-binding: t; -*-

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

;; Named view validation for the rebuilt client.
;; View execution and Rust cache integration are added in later rebuild steps.

;;; Code:

(require 'org-files-db-core)
(require 'org-files-db-process)
(require 'subr-x)

(defun org-files-db-views--name (view)
  "Return the validated name of VIEW."
  (let ((name (car-safe view)))
    (unless (and (stringp name) (not (string-empty-p name)))
      (user-error "Invalid org-files-db view name: %S" name))
    name))

(defun org-files-db-views--config-name (view)
  "Return the validated effective configuration name for VIEW."
  (let* ((properties (cdr view))
         (has-config (plist-member properties :config))
         (config (if has-config
                     (plist-get properties :config)
                   org-files-db-default-config)))
    (unless (and (stringp config) (not (string-empty-p config)))
      (user-error "View `%s' has no valid configuration"
                  (org-files-db-views--name view)))
    (org-files-db-process--config-name config)))

(defun org-files-db-views--validate-views ()
  "Validate `org-files-db-views' and return it."
  (let ((seen (make-hash-table :test #'equal)))
    (org-files-db-process--validated-configs)
    (dolist (view org-files-db-views)
      (unless (consp view)
        (user-error "Invalid org-files-db view: %S" view))
      (let ((name (org-files-db-views--name view)))
        (when (gethash name seen)
          (user-error "Duplicate org-files-db view name: %s" name))
        (puthash name t seen))
      (org-files-db-process--config-file
       (org-files-db-views--config-name view)))
    org-files-db-views))

(provide 'org-files-db-views)

;;; org-files-db-views.el ends here
