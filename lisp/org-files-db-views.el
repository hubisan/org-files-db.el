;;; org-files-db-views.el --- Predefined views -*- lexical-binding: t; -*-

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

;; Named wrappers around the generic query and search entry points.

;;; Code:

(require 'org-files-db-query)
(require 'org-files-db-search)

(defcustom org-files-db-views nil
  "Named predefined query and search views.
Each entry has the form (NAME :command COMMAND ...), where COMMAND is
`query' or `search'."
  :type '(repeat sexp)
  :group 'org-files-db)

(defun org-files-db--validate-view-name (view)
  "Return VIEW's validated name."
  (let ((name (car-safe view)))
    (unless (and (stringp name) (not (string-empty-p name)))
      (user-error "Invalid org-files-db view name in %S" view))
    name))

(defun org-files-db--view-command (view)
  "Return VIEW's validated command."
  (let ((command (plist-get (cdr view) :command)))
    (unless (memq command '(query search))
      (user-error "View `%s' has invalid :command %S"
                  (org-files-db--validate-view-name view) command))
    command))

(defun org-files-db--validate-views ()
  "Validate `org-files-db-views' and return it."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (view org-files-db-views)
      (unless (and (listp view) (cdr view))
        (user-error "Malformed org-files-db view: %S" view))
      (let ((name (org-files-db--validate-view-name view)))
        (when (gethash name seen)
          (user-error "Duplicate org-files-db view name: %s" name))
        (puthash name t seen)
        (org-files-db--view-command view)))
    org-files-db-views))

;;;###autoload
(defun org-files-db-get-view (name)
  "Return a copy of predefined view NAME."
  (unless (stringp name)
    (user-error "View name must be a string"))
  (org-files-db--validate-views)
  (if-let* ((view (assoc name org-files-db-views #'string=)))
      (copy-tree view)
    (user-error "Unknown org-files-db view: %s" name)))

(defun org-files-db--view-names (command)
  "Return configured view names for COMMAND."
  (org-files-db--validate-views)
  (mapcar #'car
          (seq-filter
           (lambda (view) (eq (org-files-db--view-command view) command))
           org-files-db-views)))

(defun org-files-db--read-view-name (command)
  "Read a configured view name for COMMAND."
  (let ((names (org-files-db--view-names command)))
    (unless names
      (user-error "No org-files-db %s views are configured" command))
    (completing-read
     (format "%s view: " (capitalize (symbol-name command)))
     names nil t nil nil (car names))))

(defun org-files-db--view-action (view)
  "Return VIEW's optional action function."
  (let ((action (plist-get (cdr view) :action)))
    (when (and action (not (functionp action)))
      (user-error "View `%s' has invalid :action %S" (car view) action))
    action))

;;;###autoload
(defun org-files-db-query-view (&optional name)
  "Execute predefined query view NAME.
Interactively, offer only query views through standard completion."
  (interactive)
  (let* ((name (or name (org-files-db--read-view-name 'query)))
         (view (org-files-db-get-view name)))
    (unless (eq (org-files-db--view-command view) 'query)
      (user-error "View `%s' is not a query view" name))
    (let ((query (plist-get (cdr view) :query))
          (columns (plist-get (cdr view) :columns))
          (action (org-files-db--view-action view)))
      (unless query
        (user-error "Query view `%s' has no :query" name))
      (org-files-db-query query columns action))))

;;;###autoload
(defun org-files-db-search-view (&optional name)
  "Execute predefined search view NAME.
Interactively, offer only search views through standard completion."
  (interactive)
  (let* ((name (or name (org-files-db--read-view-name 'search)))
         (view (org-files-db-get-view name)))
    (unless (eq (org-files-db--view-command view) 'search)
      (user-error "View `%s' is not a search view" name))
    (let ((expression (plist-get (cdr view) :expression))
          (columns (plist-get (cdr view) :columns))
          (action (org-files-db--view-action view))
          (scope (or (plist-get (cdr view) :scope) 'all)))
      (unless (and (stringp expression)
                   (not (string-empty-p expression)))
        (user-error "Search view `%s' has no valid :expression" name))
      (org-files-db-search expression columns action scope))))

(provide 'org-files-db-views)

;;; org-files-db-views.el ends here
