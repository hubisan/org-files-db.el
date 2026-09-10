;;; org-files-db-views.el --- Predefined views for org-files-db -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Hubmann

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Flat predefined structural query views for the rebuilt orgfdb client.
;; Rust cache lifecycle management lives in `org-files-db-cache'.

;;; Code:

(require 'cl-lib)
(require 'org-files-db-actions)
(require 'org-files-db-core)
(require 'org-files-db-presentation)
(require 'org-files-db-process)
(require 'org-files-db-query)
(require 'subr-x)

(declare-function org-files-db-cache--run-view "org-files-db-cache" (name))

(defvar org-files-db-cache-mode)

(defconst org-files-db-views--allowed-keys
  '(:config :query :columns :sort :row-source :cache :action)
  "Keys accepted in one flat predefined view definition.")

(cl-defstruct (org-files-db-views--resolved
               (:constructor org-files-db-views--resolved-create))
  "One fully resolved predefined view definition."
  name
  config
  config-file
  query
  query-string
  target
  columns
  sort
  row-source
  cache
  action
  action-includes)

(defun org-files-db-views--name (view)
  "Return the validated name of VIEW."
  (let ((name (car-safe view)))
    (unless (and (stringp name) (not (string-empty-p name)))
      (user-error "Invalid org-files-db view name: %S" name))
    name))

(defun org-files-db-views--properties (view)
  "Return the validated flat property list from VIEW."
  (let ((properties (cdr view))
        seen)
    (unless (and (listp properties)
                 (proper-list-p properties)
                 (cl-evenp (length properties)))
      (user-error "Invalid property list for org-files-db view `%s'"
                  (org-files-db-views--name view)))
    (let ((rest properties))
      (while rest
        (let ((key (pop rest)))
          (pop rest)
          (unless (memq key org-files-db-views--allowed-keys)
            (user-error "Unsupported key %S in org-files-db view `%s'"
                        key (org-files-db-views--name view)))
          (when (memq key seen)
            (user-error "Duplicate key %S in org-files-db view `%s'"
                        key (org-files-db-views--name view)))
          (push key seen))))
    properties))

(defun org-files-db-views--config-name (view)
  "Return the validated effective configuration name for VIEW."
  (let* ((properties (org-files-db-views--properties view))
         (has-config (plist-member properties :config))
         (config (if has-config
                     (plist-get properties :config)
                   org-files-db-default-config)))
    (unless (and (stringp config) (not (string-empty-p config)))
      (user-error "View `%s' has no valid configuration"
                  (org-files-db-views--name view)))
    (org-files-db-process--config-name config)))

(defun org-files-db-views--copy-data (value)
  "Return an independent snapshot copy of configuration VALUE."
  (cond
   ((stringp value) (copy-sequence value))
   ((consp value)
    (cons (org-files-db-views--copy-data (car value))
          (org-files-db-views--copy-data (cdr value))))
   ((vectorp value)
    (vconcat (mapcar #'org-files-db-views--copy-data value)))
   (t value)))

(defun org-files-db-views--normalize-includes (includes)
  "Return canonical string names for explicit query INCLUDES."
  (let (names)
    (dolist (include includes)
      (let ((name
             (cond
              ((symbolp include) (symbol-name include))
              ((and (stringp include) (not (string-empty-p include))) include)
              (t (user-error "Invalid orgfdb query include: %S" include)))))
        (push name names)))
    (sort (delete-dups names) #'string<)))

(defun org-files-db-views--resolve (view)
  "Return the complete effective definition for VIEW."
  (let* ((name (org-files-db-views--name view))
         (properties (org-files-db-views--properties view)))
    (unless (plist-member properties :query)
      (user-error "View `%s' has no :query" name))
    (let* ((query (plist-get properties :query))
           (target (org-files-db-query--target query))
           (config (org-files-db-views--config-name view))
           (config-file (org-files-db-process--config-file config))
           (columns
            (or (plist-get properties :columns)
                (org-files-db-presentation--default-columns target)))
           (sort
            (if (plist-member properties :sort)
                (plist-get properties :sort)
              (org-files-db-presentation--default-sort target)))
           (row-source (plist-get properties :row-source))
           (cache (plist-get properties :cache))
           (action
            (org-files-db-query--effective-action
             target (plist-get properties :action)))
           (action-includes
            (org-files-db-views--normalize-includes
             (org-files-db-actions--required-includes action))))
      (unless (memq cache '(nil t))
        (user-error "View `%s' has invalid :cache value: %S" name cache))
      (org-files-db-presentation--spec-json columns sort row-source)
      (org-files-db-views--resolved-create
       :name (copy-sequence name)
       :config (copy-sequence config)
       :config-file (copy-sequence config-file)
       :query (org-files-db-views--copy-data query)
       :query-string (copy-sequence (org-files-db-query--string query))
       :target target
       :columns (org-files-db-views--copy-data columns)
       :sort (org-files-db-views--copy-data sort)
       :row-source (org-files-db-views--copy-data row-source)
       :cache cache
       :action action
       :action-includes (org-files-db-views--copy-data action-includes)))))

(defun org-files-db-views--resolved-views ()
  "Validate and return all predefined views as resolved snapshots."
  (let ((seen (make-hash-table :test #'equal))
        resolved)
    (org-files-db-process--validated-configs)
    (dolist (view org-files-db-views)
      (unless (consp view)
        (user-error "Invalid org-files-db view: %S" view))
      (let ((name (org-files-db-views--name view)))
        (when (gethash name seen)
          (user-error "Duplicate org-files-db view name: %s" name))
        (puthash name t seen))
      (push (org-files-db-views--resolve view) resolved))
    (nreverse resolved)))

(defun org-files-db-views--validate-views ()
  "Validate `org-files-db-views' and return it."
  (org-files-db-views--resolved-views)
  org-files-db-views)

(defun org-files-db-views--get (name)
  "Return the validated predefined view named NAME."
  (unless (and (stringp name) (not (string-empty-p name)))
    (user-error "Invalid org-files-db view name: %S" name))
  (org-files-db-views--validate-views)
  (or (assoc name org-files-db-views)
      (user-error "Unknown org-files-db view: %s" name)))

(defun org-files-db-views--names ()
  "Return validated predefined view names in configured order."
  (mapcar #'org-files-db-views--name
          (org-files-db-views--validate-views)))

(defun org-files-db-views--read-name ()
  "Read and return one predefined view name."
  (let ((names (org-files-db-views--names)))
    (unless names
      (user-error "No org-files-db views are configured"))
    (completing-read "org-files-db view: " names nil t)))

(defun org-files-db-views--run-one-shot (resolved)
  "Run RESOLVED through the normal one-shot query path."
  (org-files-db-query
   (org-files-db-views--resolved-query resolved)
   :config (org-files-db-views--resolved-config resolved)
   :columns (org-files-db-views--resolved-columns resolved)
   :sort (org-files-db-views--resolved-sort resolved)
   :row-source (org-files-db-views--resolved-row-source resolved)
   :action (org-files-db-views--resolved-action resolved)))

;;;###autoload
(defun org-files-db-view (&optional name)
  "Run predefined view NAME and return the selected original result.
When NAME is nil, read one configured view name interactively."
  (interactive)
  (let ((name (or name (org-files-db-views--read-name))))
    (if (bound-and-true-p org-files-db-cache-mode)
        (org-files-db-cache--run-view name)
      (org-files-db-views--run-one-shot
       (org-files-db-views--resolve
        (org-files-db-views--get name))))))

(provide 'org-files-db-views)

;;; org-files-db-views.el ends here
