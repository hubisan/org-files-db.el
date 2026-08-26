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

;; Structural query data access for the rebuilt client.
;; Interactive completion and action execution are added in a later step.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-files-db-presentation)

(defun org-files-db--query-form (query)
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

(defun org-files-db--query-target (query)
  "Return the structural query target for QUERY."
  (let* ((form (org-files-db--query-form query))
         (target (and (consp form) (car form))))
    (unless (memq target '(headings files links))
      (user-error "Unsupported org-files-db query target: %S" target))
    target))

(defun org-files-db--query-string (query)
  "Return QUERY as the structural expression sent to orgfdb."
  (if (stringp query)
      query
    (prin1-to-string (org-files-db--query-form query))))

(cl-defun org-files-db-query-results
    (query &key config columns (sort nil sort-supplied-p) row-source)
  "Return prepared Rust presentation data for structural QUERY.
CONFIG is a name from `org-files-db-configs'.  When CONFIG is nil, use the
configured default.  COLUMNS, SORT, and ROW-SOURCE use the flat Emacs
presentation forms.  This function does not open completion or run an action."
  (let* ((target (org-files-db--query-target query))
         (config-name (org-files-db--config-name config))
         (effective-columns (or columns (org-files-db--default-columns target)))
         (effective-sort
          (if sort-supplied-p sort (org-files-db--default-sort target)))
         (spec-json
          (org-files-db--presentation-spec-json
           effective-columns effective-sort row-source))
         (arguments
          (append
           (list "query"
                 "--format" "presentation-json"
                 "--presentation-spec-json" spec-json)
           (org-files-db--config-arguments config-name)
           (list (org-files-db--query-string query)))))
    (org-files-db--decode-presentation
     (org-files-db--call-json arguments))))

(provide 'org-files-db-query)

;;; org-files-db-query.el ends here
