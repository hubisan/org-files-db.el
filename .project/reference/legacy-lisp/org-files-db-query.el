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


;;;###autoload
(cl-defun org-files-db-query
    (query &optional columns action
           &key (config-file nil config-file-supplied-p)
           (sort nil sort-supplied-p))
  "Execute QUERY and select a result displayed with COLUMNS.
QUERY is an orgfdb query represented as an Emacs Lisp form or string.
ACTION receives the selected result object.  When omitted, use
`org-files-db-query-action'.  When CONFIG-FILE is omitted, inherit
`org-files-db-config-file'; an explicit nil disables --config for this call.
When SORT is omitted, use the configured default for the query result type;
an explicit nil preserves the order returned by orgfdb.
Configured columns automatically request any required path or target context."
  (interactive (list (org-files-db--read-sexp "Query: ") nil nil))
  (let* ((inferred-target (org-files-db--query-target query))
         (effective-sort
          (org-files-db--effective-sort
           inferred-target sort sort-supplied-p))
         (effective-config-file
          (org-files-db--resolve-config-file
           config-file config-file-supplied-p "Query"))
         (fetched
          (org-files-db--fetch-query
           query columns effective-config-file "Query" effective-sort))
         (target (plist-get fetched :target)))
    (org-files-db--present-results
     (plist-get fetched :results)
     (plist-get fetched :columns)
     action
     "Query result: "
     (plist-get fetched :sort)
     target
     "Query")))
(provide 'org-files-db-query)

;;; org-files-db-query.el ends here
