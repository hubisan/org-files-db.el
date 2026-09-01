;;; org-files-db-query.el --- Structural queries for org-files-db -*- lexical-binding: t; -*-

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

;; Structural query data access and the public interactive query command.
;; All result display data comes from the shared Rust presentation pipeline.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-files-db-process)
(require 'org-files-db-presentation)
(require 'org-files-db-actions)

(defvar org-files-db-query--history nil
  "History for interactive structural org-files-db queries.")

(defun org-files-db-query--read-query ()
  "Read and return one structural org-files-db query form."
  (org-files-db-query--form
   (read-from-minibuffer "orgfdb query: " nil nil nil
                         'org-files-db-query--history)))

(defun org-files-db-query--form (query)
  "Return QUERY as one Emacs Lisp structural query form."
  (cond
   ((consp query) query)
   ((stringp query)
    (condition-case err
        (pcase-let ((`(,form . ,position) (read-from-string query)))
          (unless (string-match-p "\\`[[:space:]]*\\'"
                                  (substring query position))
            (user-error "Query contains trailing data: %s" query))
          form)
      (error
       (user-error "Cannot read org-files-db query: %s"
                   (error-message-string err)))))
   (t
    (user-error "Query must be an Emacs Lisp form or string: %S" query))))

(defun org-files-db-query--target (query)
  "Return the structural query target for QUERY."
  (let* ((form (org-files-db-query--form query))
         (target (and (consp form) (car form))))
    (unless (memq target '(headings files links))
      (user-error "Unsupported org-files-db query target: %S" target))
    target))

(defun org-files-db-query--string (query)
  "Return QUERY as the structural expression sent to orgfdb."
  (if (stringp query)
      query
    (prin1-to-string (org-files-db-query--form query))))

(cl-defun org-files-db-query-results
    (query &key config columns (sort nil sort-supplied-p) row-source)
  "Return prepared Rust presentation data for structural QUERY.
CONFIG is a name from `org-files-db-configs'.  When CONFIG is nil, use the
configured default.  COLUMNS, SORT, and ROW-SOURCE use the flat Emacs
presentation forms.  This function does not open completion or run an action."
  (let* ((target (org-files-db-query--target query))
         (config-name (org-files-db-process--config-name config))
         (effective-columns (or columns (org-files-db-presentation--default-columns target)))
         (effective-sort
          (if sort-supplied-p sort (org-files-db-presentation--default-sort target)))
         (spec-json
          (org-files-db-presentation--spec-json
           effective-columns effective-sort row-source))
         (arguments
          (append
           (list "query"
                 "--format" "presentation-json"
                 "--presentation-spec-json" spec-json)
           (org-files-db-process--config-arguments config-name)
           (list (org-files-db-query--string query)))))
    (let ((presentation
           (org-files-db-presentation--decode
            (org-files-db-process--call-json arguments))))
      (setf (org-files-db-presentation-config presentation) config-name)
      presentation)))

(defun org-files-db-query--effective-action (target action)
  "Return ACTION or the configured default action for TARGET."
  (let ((effective-action (or action (org-files-db-actions--default-action target))))
    (unless (functionp effective-action)
      (user-error "Org-files-db action is not callable: %S" effective-action))
    effective-action))

(defun org-files-db-query--run-presentation-action (presentation target &optional action)
  "Select one row from PRESENTATION and run ACTION for TARGET.
Use the configured default action when ACTION is nil.  Return the selected
original result."
  (let* ((effective-action (org-files-db-query--effective-action target action))
         (result (org-files-db-presentation--read presentation))
         (org-files-db-actions--current-action-config
          (org-files-db-presentation-config presentation)))
    (funcall effective-action result)
    result))

;;;###autoload
(cl-defun org-files-db-query
    (query &key config columns (sort nil sort-supplied-p) row-source action)
  "Run structural QUERY, select one result, and execute its action.
CONFIG is a name from `org-files-db-configs'.  COLUMNS, SORT, and ROW-SOURCE
override the configured presentation defaults.  ACTION overrides the default
action for the query target.  Return the selected original result.

With an interactive prefix argument, select the configuration before running
the query."
  (interactive
   (list (org-files-db-query--read-query)
         :config (org-files-db-process--interactive-config-name current-prefix-arg)))
  (let* ((target (org-files-db-query--target query))
         (effective-action (org-files-db-query--effective-action target action))
         (arguments (list :config config
                          :columns columns
                          :row-source row-source))
         (arguments (if sort-supplied-p
                        (append arguments (list :sort sort))
                      arguments))
         (presentation (apply #'org-files-db-query-results query arguments)))
    (org-files-db-query--run-presentation-action presentation target effective-action)))

(provide 'org-files-db-query)

;;; org-files-db-query.el ends here
