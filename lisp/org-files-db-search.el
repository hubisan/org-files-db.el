;;; org-files-db-search.el --- Full-text search -*- lexical-binding: t; -*-

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

;; One-shot and optional Consult-powered live search over the orgfdb FTS5
;; index.

;;; Code:

(require 'org-files-db-core)
(require 'org-files-db-actions)

(declare-function consult--dynamic-collection "consult" (fun &rest options))
(declare-function consult--lookup-candidate "consult"
                  (selected candidates input narrow))
(declare-function consult--read "consult" (candidates &rest options))

(defcustom org-files-db-search-min-input 3
  "Minimum number of characters required before live search starts."
  :type 'natnum
  :group 'org-files-db)

;;;###autoload
(defun org-files-db-search (expression &optional columns action scope)
  "Search for EXPRESSION and select a result displayed with COLUMNS.
ACTION receives the selected original result.  SCOPE is one of `all',
`title', or `body'."
  (interactive
   (list (read-string "FTS5 search: ") nil nil 'all))
  (let* ((response (org-files-db--execute-search expression scope))
         (results (org-files-db--normalize-results response))
         (columns (or columns
                      (org-files-db--default-columns 'search results))))
    (org-files-db--present-results
     results columns action "Search result: ")))

(defun org-files-db-search--status-candidate (message)
  "Return a non-selectable live-search status candidate for MESSAGE."
  (propertize (format "[%s]" message)
              'org-files-db-status message
              'consult--candidate nil))

(defun org-files-db-search--live-candidates (expression columns scope)
  "Return live candidates for EXPRESSION using COLUMNS and SCOPE.
This function starts an asynchronous orgfdb process.  When Consult interrupts
it because the minibuffer input changed, unwind cleanup cancels the obsolete
process."
  (let (done response failure process)
    (setq process
          (org-files-db--start-process
           "search"
           (org-files-db--search-arguments expression scope)
           (lambda (value error-data)
             (setq response value
                   failure error-data
                   done t))))
    (unwind-protect
        (progn
          (while (and (not done) (process-live-p process))
            (accept-process-output process 0.05))
          (unless done
            (setq failure (list :status (process-exit-status process)
                                :stderr "Search process ended unexpectedly")))
          (if failure
              (list
               (org-files-db-search--status-candidate
                (or (plist-get failure :stderr)
                    "Invalid search expression")))
            (let ((results (org-files-db--normalize-results response)))
              (if results
                  (org-files-db--make-candidates results columns)
                (list (org-files-db-search--status-candidate "No matches"))))))
      (when (and process (not done))
        (org-files-db--cancel-process process)))))

(defun org-files-db-search--consult-dynamic-collection (function &rest arguments)
  "Create a Consult dynamic collection from FUNCTION and ARGUMENTS."
  (apply #'consult--dynamic-collection function arguments))

(defun org-files-db-search--consult-read (collection &rest arguments)
  "Read one candidate from COLLECTION using Consult ARGUMENTS."
  (apply #'consult--read collection arguments))

;;;###autoload
(defun org-files-db-search-live (&optional columns action scope)
  "Search orgfdb interactively while input changes.
COLUMNS controls candidate display, ACTION handles the selected result, and
SCOPE is one of `all', `title', or `body'.  This command requires Consult."
  (interactive)
  (unless (require 'consult nil t)
    (user-error "Live search requires the Consult package"))
  (let* ((scope (org-files-db--validate-search-scope scope))
         (columns (or columns org-files-db-search-columns))
         (collection
          (org-files-db-search--consult-dynamic-collection
           (lambda (input)
             (org-files-db-search--live-candidates input columns scope))
           :min-input org-files-db-search-min-input))
         (result
          (org-files-db-search--consult-read
           collection
           :prompt "FTS5 search: "
           :category org-files-db--completion-category
           :require-match t
           :sort nil
           :lookup #'consult--lookup-candidate)))
    (unless result
      (user-error "No selectable org-files-db search result"))
    (funcall (or action org-files-db-query-action) result)
    result))

(provide 'org-files-db-search)

;;; org-files-db-search.el ends here
