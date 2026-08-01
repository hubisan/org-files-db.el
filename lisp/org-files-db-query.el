;;; org-files-db-query.el --- Query support for org-files-db -*- lexical-binding: t; -*-

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

;; Query Model v0 execution and standard Emacs completion.

;;; Code:

(require 'org-files-db-core)

(defun org-files-db--query-target (query)
  "Infer the Query Model target from QUERY."
  (let* ((form (if (stringp query)
                   (condition-case nil
                       (let ((read-eval nil))
                         (car (read-from-string query)))
                     (error nil))
                 query))
         (head (car-safe form)))
    (if (memq head '(headings links files)) head 'headings)))

(defun org-files-db--query-arguments (query)
  "Return command arguments for Query Model v0 QUERY."
  (append '("--format" "json" "--output" "flat" "--include" "path")
          (org-files-db--config-arguments)
          (list (org-files-db--query-string query))))

(defun org-files-db--execute-query (query)
  "Execute Query Model v0 QUERY and return the response envelope."
  (org-files-db--call "query" (org-files-db--query-arguments query)))

;;;###autoload
(defun org-files-db-query (query &optional columns action)
  "Execute QUERY and select a result displayed with COLUMNS.
QUERY is a Query Model v0 Lisp form or its textual representation.
ACTION receives the selected original result.  When omitted, use
`org-files-db-query-action'."
  (interactive (list (org-files-db--read-sexp "Query: ") nil nil))
  (let* ((response (org-files-db--execute-query query))
         (results (org-files-db--normalize-results response))
         (target (or (org-files-db--response-target response)
                     (org-files-db--query-target query)))
         (columns (or columns
                      (org-files-db--default-columns target results))))
    (org-files-db--present-results results columns action "Query result: ")))

(provide 'org-files-db-query)

;;; org-files-db-query.el ends here
