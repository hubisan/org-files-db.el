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

;; Query execution and standard Emacs completion.

;;; Code:

(require 'cl-lib)
(require 'org-files-db-core)
(require 'org-files-db-actions)

(defvar read-eval)

(defun org-files-db-query--target (query)
  "Infer the orgfdb query target from QUERY."
  (let* ((form (if (stringp query)
                   (condition-case nil
                       (let ((read-eval nil))
                         (car (read-from-string query)))
                     (error nil))
                 query))
         (head (car-safe form)))
    (if (memq head '(headings links files)) head 'headings)))

;;;###autoload
(cl-defun org-files-db-query
    (query &optional columns action
           &key (config-file nil config-file-supplied-p))
  "Execute QUERY and select a result displayed with COLUMNS.
QUERY is an orgfdb query represented as an Emacs Lisp form or string.
ACTION receives the selected result object.  When omitted, use
`org-files-db-query-action'.  When CONFIG-FILE is omitted, inherit
`org-files-db-config-file'; an explicit nil disables --config for this call."
  (interactive (list (org-files-db--read-sexp "Query: ") nil nil))
  (let* ((effective-config-file
          (org-files-db--resolve-config-file
           config-file config-file-supplied-p "Query"))
         (response (org-files-db--execute-query
                    query effective-config-file "Query"))
         (results
          (org-files-db--results-with-config
           (org-files-db--normalize-results response)
           effective-config-file))
         (target (or (org-files-db--response-target response)
                     (org-files-db-query--target query)))
         (columns (or columns
                      (org-files-db--default-columns target results))))
    (org-files-db--present-results results columns action "Query result: ")))

(provide 'org-files-db-query)

;;; org-files-db-query.el ends here
