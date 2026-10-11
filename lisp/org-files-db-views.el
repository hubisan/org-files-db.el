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
(require 'org-files-db-results)
(require 'seq)
(require 'subr-x)

(declare-function org-files-db-cache--run-view "org-files-db-cache" (name))
(declare-function org-files-db-cache--view-presentation "org-files-db-cache" (name))

(declare-function org-files-db-cache--resolved-signature "org-files-db-cache" (resolved))

(defvar org-files-db-cache-mode)

(defconst org-files-db-views--allowed-keys
  '(:config :query :columns :sort :row-source :cache :action :display)
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
  display)

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
           (display (plist-get properties :display))
           (action
            (org-files-db-query--effective-action
             target (plist-get properties :action))))
      (unless (memq cache '(nil t))
        (user-error "View `%s' has invalid :cache value: %S" name cache))
      (unless (memq display '(nil buffer))
        (user-error "View `%s' has invalid :display value: %S" name display))
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
       :display display))))

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
  "Run predefined view NAME and return the selected action record.
When NAME is nil, read one configured view name interactively. A view with
`:display buffer' is shown in a result buffer instead, and the buffer is
returned."
  (interactive)
  (let* ((name (or name (org-files-db-views--read-name)))
         (resolved (org-files-db-views--resolve
                    (org-files-db-views--get name))))
    (cond
     ((eq (org-files-db-views--resolved-display resolved) 'buffer)
      (org-files-db-show-view name))
     ((org-files-db-views--candidate-cache-enabled-p)
      (let ((entry (org-files-db-views--candidates name)))
        (org-files-db-query--run-presentation-action
         (car entry)
         (org-files-db-views--resolved-target resolved)
         (org-files-db-views--resolved-action resolved)
         (cdr entry))))
     ((bound-and-true-p org-files-db-cache-mode)
      (org-files-db-cache--run-view name))
     (t (org-files-db-views--run-one-shot resolved)))))

(defun org-files-db-views--presentation (name)
  "Return the presentation of predefined view NAME without completion.
Use the Rust cache when `org-files-db-cache-mode' is active."
  (if (bound-and-true-p org-files-db-cache-mode)
      (org-files-db-cache--view-presentation name)
    (org-files-db-views--one-shot-presentation
     (org-files-db-views--resolve (org-files-db-views--get name)))))

(defun org-files-db-views--one-shot-presentation (resolved)
  "Return the presentation of RESOLVED from one orgfdb query."
  (org-files-db-query-results
   (org-files-db-views--resolved-query resolved)
   :config (org-files-db-views--resolved-config resolved)
   :columns (org-files-db-views--resolved-columns resolved)
   :sort (org-files-db-views--resolved-sort resolved)
   :row-source (org-files-db-views--resolved-row-source resolved)))

;;; Candidate cache

(cl-defstruct (org-files-db-views--candidate-entry
               (:constructor org-files-db-views--candidate-entry-create))
  "Built candidates of one view with the key they are valid for."
  name
  key
  presentation
  candidates)

(defvar org-files-db-views--candidate-entries nil
  "Most recently used first list of `org-files-db-views--candidate-entry'.")

(defun org-files-db-views--candidate-cache-enabled-p ()
  "Return non-nil when built view candidates are kept."
  (and (integerp org-files-db-view-candidate-cache-size)
       (> org-files-db-view-candidate-cache-size 0)))

(defun org-files-db-views--candidate-cache-clear ()
  "Forget all kept view candidates."
  (setq org-files-db-views--candidate-entries nil))

(defun org-files-db-views--date ()
  "Return the current date used in candidate cache keys."
  (format-time-string "%F"))

(defun org-files-db-views--candidate-key (resolved)
  "Return the cache key of RESOLVED, asking orgfdb for the index state."
  (let ((status (org-files-db-process--call-json
                 (append '("status" "--format" "json")
                         (org-files-db-process--config-arguments
                          (org-files-db-views--resolved-config resolved))))))
    (list (alist-get 'database_id status)
          (alist-get 'generation status)
          (org-files-db-cache--resolved-signature resolved)
          (org-files-db-views--date))))

(defun org-files-db-views--candidates (name)
  "Return the cons (PRESENTATION . CANDIDATES) of predefined view NAME.
Reuse kept candidates while the index, the view definition and the date are
unchanged. Do not modify the returned candidates."
  (if (not (org-files-db-views--candidate-cache-enabled-p))
      (let ((presentation (org-files-db-views--presentation name)))
        (cons presentation
              (org-files-db-presentation--candidates presentation)))
    (let* ((resolved (org-files-db-views--resolve
                      (org-files-db-views--get name)))
           (key (org-files-db-views--candidate-key resolved))
           (entry (seq-find
                   (lambda (candidate-entry)
                     (equal (org-files-db-views--candidate-entry-name
                             candidate-entry)
                            name))
                   org-files-db-views--candidate-entries)))
      (unless (and entry
                   (equal (org-files-db-views--candidate-entry-key entry) key))
        (let ((presentation (org-files-db-views--presentation name)))
          (setq entry
                (org-files-db-views--candidate-entry-create
                 :name name
                 :key key
                 :presentation presentation
                 :candidates
                 (org-files-db-presentation--candidates presentation)))))
      (setq org-files-db-views--candidate-entries
            (cons entry
                  (seq-remove
                   (lambda (other)
                     (equal (org-files-db-views--candidate-entry-name other)
                            name))
                   org-files-db-views--candidate-entries)))
      (when (> (length org-files-db-views--candidate-entries)
               org-files-db-view-candidate-cache-size)
        (setq org-files-db-views--candidate-entries
              (seq-take org-files-db-views--candidate-entries
                        org-files-db-view-candidate-cache-size)))
      (cons (org-files-db-views--candidate-entry-presentation entry)
            (org-files-db-views--candidate-entry-candidates entry)))))

;;;###autoload
(defun org-files-db-show-view (&optional name)
  "Show predefined view NAME in a result buffer and return it.
When NAME is nil, read one configured view name interactively. In the
buffer, RET runs the action of the view, o exports an outline and g runs the
view again."
  (interactive)
  (let* ((name (or name (org-files-db-views--read-name)))
         (resolved (org-files-db-views--resolve
                    (org-files-db-views--get name))))
    (org-files-db-results--show
     (format "*org-files-db: %s*" name)
     (if (org-files-db-views--candidate-cache-enabled-p)
         (lambda () (org-files-db-views--candidates name))
       (lambda () (org-files-db-views--presentation name)))
     (org-files-db-views--resolved-action resolved))))

(provide 'org-files-db-views)

;;; org-files-db-views.el ends here
