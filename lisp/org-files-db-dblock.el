;;; org-files-db-dblock.el --- Org dynamic blocks -*- lexical-binding: t; -*-

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

;; Dynamic blocks that render query, search, view, and backlink results using
;; the package's existing execution and Org rendering layers.

;;; Code:

(require 'org)
(require 'org-files-db-actions)
(require 'org-files-db-views)

(defun org-files-db--dblock-value (value)
  "Return dynamic-block VALUE in its useful scalar form."
  (cond
   ((stringp value) value)
   ((symbolp value) value)
   ((null value) nil)
   (t (format "%s" value))))

(defun org-files-db--dblock-layout (params)
  "Return validated layout from dynamic-block PARAMS."
  (let* ((value (or (plist-get params :layout) 'flat))
         (layout (if (stringp value) (intern value) value)))
    (unless (memq layout '(flat outline))
      (user-error "Unsupported org-files-db dynamic-block layout: %S" value))
    layout))

(defun org-files-db--dblock-base-level ()
  "Return the level of the heading containing the current dynamic block."
  (save-excursion
    (condition-case nil
        (progn
          (org-back-to-heading t)
          (org-outline-level))
      (error 0))))

(defun org-files-db--dblock-query-definition (params)
  "Return the Query Model expression represented by PARAMS."
  (let ((query (plist-get params :query))
        (view-name (plist-get params :view)))
    (when (and query view-name)
      (user-error "Use exactly one of :query or :view"))
    (cond
     (query (org-files-db--dblock-value query))
     (view-name
      (let* ((view (org-files-db-get-view
                    (format "%s" (org-files-db--dblock-value view-name))))
             (command (org-files-db--view-command view)))
        (unless (eq command 'query)
          (user-error "View `%s' is not a query view" (car view)))
        (or (plist-get (cdr view) :query)
            (user-error "Query view `%s' has no :query" (car view)))))
     (t (user-error "Dynamic query block requires :query or :view")))))

(defun org-files-db--dblock-search-definition (params)
  "Return (EXPRESSION . SCOPE) represented by PARAMS."
  (let ((expression (plist-get params :expression))
        (view-name (plist-get params :view))
        (scope (plist-get params :scope)))
    (when (and expression view-name)
      (user-error "Use exactly one of :expression or :view"))
    (cond
     (expression
      (cons (format "%s" (org-files-db--dblock-value expression))
            (or (and scope
                     (if (stringp scope) (intern scope) scope))
                'all)))
     (view-name
      (let* ((view (org-files-db-get-view
                    (format "%s" (org-files-db--dblock-value view-name))))
             (command (org-files-db--view-command view)))
        (unless (eq command 'search)
          (user-error "View `%s' is not a search view" (car view)))
        (cons (or (plist-get (cdr view) :expression)
                  (user-error "Search view `%s' has no :expression" (car view)))
              (or (plist-get (cdr view) :scope) 'all))))
     (t (user-error "Dynamic search block requires :expression or :view")))))

(defun org-files-db--dblock-insert-name (params)
  "Insert optional generated result name from PARAMS."
  (when-let* ((name (plist-get params :block-name)))
    (insert (format "#+name: %s\n" name))))

(defun org-files-db--dblock-render (results params)
  "Insert RESULTS according to dynamic-block PARAMS."
  (org-files-db--dblock-insert-name params)
  (insert
   (org-files-db--render-org-results
    results
    (org-files-db--dblock-layout params)
    (org-files-db--dblock-base-level))))

(defun org-files-db--dblock-protect (name function)
  "Run FUNCTION and report failures as dynamic block NAME errors."
  (condition-case err
      (funcall function)
    (error
     (signal 'org-files-db-error
             (list (format "%s dynamic block failed: %s"
                           name (error-message-string err)))))))

;;;###autoload
(defun org-files-db-dblock-insert-query (query)
  "Insert and update an org-files-db query dynamic block for QUERY."
  (interactive
   (list (prin1-to-string (org-files-db--read-sexp "Query: "))))
  (unless (derived-mode-p 'org-mode)
    (user-error "Org-files-db dynamic blocks require Org mode"))
  (org-create-dblock
   (list :name "org-files-db-query" :query query :layout 'flat))
  (org-update-dblock))

;;;###autoload
(defun org-dblock-write:org-files-db-query (params)
  "Write an org-files-db query dynamic block using PARAMS."
  (org-files-db--dblock-protect
   "org-files-db-query"
   (lambda ()
     (let* ((query (org-files-db--dblock-query-definition params))
            (response (org-files-db--execute-query query))
            (results (org-files-db--normalize-results response)))
       (org-files-db--dblock-render results params)))))

;;;###autoload
(defun org-files-db-dblock-insert-search (expression)
  "Insert and update an org-files-db search dynamic block for EXPRESSION."
  (interactive (list (read-string "FTS5 search: ")))
  (unless (derived-mode-p 'org-mode)
    (user-error "Org-files-db dynamic blocks require Org mode"))
  (org-create-dblock
   (list :name "org-files-db-search"
         :expression expression
         :scope 'all
         :layout 'flat))
  (org-update-dblock))

;;;###autoload
(defun org-dblock-write:org-files-db-search (params)
  "Write an org-files-db search dynamic block using PARAMS."
  (org-files-db--dblock-protect
   "org-files-db-search"
   (lambda ()
     (pcase-let* ((`(,expression . ,scope)
                   (org-files-db--dblock-search-definition params))
                  (response (org-files-db--execute-search expression scope))
                  (results (org-files-db--normalize-results response)))
       (org-files-db--dblock-render results params)))))

;;;###autoload
(defun org-files-db-dblock-insert-backlinks (&optional scope)
  "Insert and update a backlink dynamic block for SCOPE."
  (interactive
   (list (intern
          (completing-read "Backlink scope: " '("heading" "file")
                           nil t nil nil "heading"))))
  (unless (derived-mode-p 'org-mode)
    (user-error "Org-files-db dynamic blocks require Org mode"))
  (org-create-dblock
   (list :name "org-files-db-backlinks"
         :scope (or scope 'heading)
         :layout 'flat))
  (org-update-dblock))

(defun org-files-db--dblock-backlink-query (params)
  "Return the backlink query described by PARAMS."
  (let* ((value (or (plist-get params :scope) 'heading))
         (scope (if (stringp value) (intern value) value)))
    (pcase scope
      ('file
       (unless buffer-file-name
         (user-error "File backlinks require a file-visiting buffer"))
       `(links
         (target
          (files (file-path ,(expand-file-name buffer-file-name) :exact t)))))
      ('heading
       (save-excursion
         (org-back-to-heading t)
         (org-files-db--backlinks-query-at-point t)))
      (_ (user-error "Unsupported backlink scope: %S" value)))))

;;;###autoload
(defun org-dblock-write:org-files-db-backlinks (params)
  "Write an org-files-db backlink dynamic block using PARAMS."
  (org-files-db--dblock-protect
   "org-files-db-backlinks"
   (lambda ()
     (let* ((query (org-files-db--dblock-backlink-query params))
            (response (org-files-db--execute-query query))
            (results (org-files-db--normalize-results response)))
       (org-files-db--dblock-render results params)))))

(defun org-files-db--register-dynamic-blocks ()
  "Register all org-files-db dynamic block types."
  (org-dynamic-block-define
   "org-files-db-query" #'org-files-db-dblock-insert-query)
  (org-dynamic-block-define
   "org-files-db-search" #'org-files-db-dblock-insert-search)
  (org-dynamic-block-define
   "org-files-db-backlinks" #'org-files-db-dblock-insert-backlinks))

(org-files-db--register-dynamic-blocks)

(provide 'org-files-db-dblock)

;;; org-files-db-dblock.el ends here
