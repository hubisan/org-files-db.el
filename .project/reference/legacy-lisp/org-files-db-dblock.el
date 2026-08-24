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

(require 'cl-lib)
(require 'org)
(require 'org-files-db-core)
(require 'org-files-db-export)
(require 'org-files-db-views)

(defun org-files-db-dblock--value (value)
  "Return dynamic-block VALUE in its useful scalar form."
  (cond
   ((stringp value) value)
   ((symbolp value) value)
   ((null value) nil)
   (t (format "%s" value))))

(defun org-files-db-dblock--layout (params)
  "Return validated layout from dynamic-block PARAMS."
  (let* ((value (or (plist-get params :layout) 'flat))
         (layout (if (stringp value) (intern value) value)))
    (unless (memq layout '(flat outline))
      (user-error "Unsupported org-files-db dynamic-block layout: %S" value))
    layout))

(defun org-files-db-dblock--query-includes (params)
  "Return query includes required to render dynamic-block PARAMS."
  (when (eq (org-files-db-dblock--layout params) 'outline)
    '(path)))

(defun org-files-db-dblock--base-level ()
  "Return the level of the heading containing the current dynamic block."
  (save-excursion
    (condition-case nil
        (progn
          (org-back-to-heading t)
          (org-outline-level))
      (error 0))))

(defun org-files-db-dblock--view (params command)
  "Return PARAMS view after validating that it uses COMMAND."
  (when-let* ((view-name (plist-get params :view)))
    (let* ((view (org-files-db-views-get
                  (format "%s" (org-files-db-dblock--value view-name))))
           (actual-command (plist-get (cdr view) :command)))
      (unless (eq actual-command command)
        (user-error "View `%s' is not a %s view" (car view) command))
      view)))

(defun org-files-db-dblock--config-value (value)
  "Return the configuration path represented by dynamic-block VALUE.
The symbol or string `none' disables --config."
  (let ((value (org-files-db-dblock--value value)))
    (cond
     ((or (eq value 'none) (equal value "none")) nil)
     ((or (null value) (equal value "nil"))
      (user-error "Use :config-file none to disable dynamic-block configuration"))
     ((stringp value) value)
     ((symbolp value) (symbol-name value))
     (t (user-error "Invalid dynamic-block :config-file value: %S" value)))))

(defun org-files-db-dblock--config-file (params command)
  "Return the effective configuration for PARAMS and COMMAND."
  (let ((view (org-files-db-dblock--view params command)))
    (when (and view (plist-member params :config-file))
      (user-error "Do not combine :view and :config-file in one dynamic block"))
    (if view
        (let ((properties (cdr view)))
          (org-files-db--resolve-config-file
           (plist-get properties :config-file)
           (not (null (plist-member properties :config-file)))
           (format "View `%s'" (car view))))
      (let ((supplied-p (not (null (plist-member params :config-file)))))
        (org-files-db--resolve-config-file
         (and supplied-p
              (org-files-db-dblock--config-value
               (plist-get params :config-file)))
         supplied-p
         (format "Dynamic %s block" command))))))

(defun org-files-db-dblock--sort-value (value)
  "Return safely parsed dynamic-block sort VALUE."
  (cond
   ((null value) nil)
   ((listp value) value)
   ((eq value 'nil) nil)
   ((stringp value)
    (if (string= (string-trim value) "nil")
        nil
      (condition-case err
          (let* ((parsed (read-from-string value))
                 (sort (car parsed))
                 (rest (substring value (cdr parsed))))
            (unless (string-match-p "\\`[[:space:]]*\\'" rest)
              (user-error "Invalid dynamic-block :sort value: %S" value))
            sort)
        (error
         (user-error "Invalid dynamic-block :sort value %S: %s"
                     value (error-message-string err))))))
   (t (user-error "Invalid dynamic-block :sort value: %S" value))))

(defun org-files-db-dblock--sort (params command context columns)
  "Return effective validated sort for PARAMS, COMMAND, CONTEXT, and COLUMNS."
  (let* ((view (org-files-db-dblock--view params command))
         (direct-p (not (null (plist-member params :sort))))
         (view-properties (and view (cdr view)))
         (view-p (and view
                      (not (null (plist-member view-properties :sort)))))
         (sort
          (cond
           (direct-p (org-files-db-dblock--sort-value (plist-get params :sort)))
           (view-p (plist-get view-properties :sort))
           (t nil)))
         (supplied-p (or direct-p view-p))
         (effective (org-files-db--effective-sort context sort supplied-p))
         (origin
          (if view
              (format "View `%s' dynamic %s block" (car view) command)
            (format "Dynamic %s block" command))))
    (org-files-db--normalize-sort effective columns context origin)))

(defun org-files-db-dblock--columns (params command context)
  "Return columns relevant to PARAMS, COMMAND, and sort CONTEXT."
  (let ((view (org-files-db-dblock--view params command)))
    (org-files-db--normalize-columns
     (or (and view (plist-get (cdr view) :columns))
         (org-files-db--default-columns context nil)))))

(defun org-files-db-dblock--query-definition (params)
  "Return the query expression represented by PARAMS."
  (let ((query (plist-get params :query))
        (view-name (plist-get params :view)))
    (when (and query view-name)
      (user-error "Use exactly one of :query or :view"))
    (cond
     (query (org-files-db-dblock--value query))
     (view-name
      (let ((view (org-files-db-dblock--view params 'query)))
        (or (plist-get (cdr view) :query)
            (user-error "Query view `%s' has no :query" (car view)))))
     (t (user-error "Dynamic query block requires :query or :view")))))

(defun org-files-db-dblock--search-definition (params)
  "Return (EXPRESSION . SCOPE) represented by PARAMS."
  (let ((expression (plist-get params :expression))
        (view-name (plist-get params :view))
        (scope (plist-get params :scope)))
    (when (and expression view-name)
      (user-error "Use exactly one of :expression or :view"))
    (cond
     (expression
      (cons (format "%s" (org-files-db-dblock--value expression))
            (or (and scope
                     (if (stringp scope) (intern scope) scope))
                'all)))
     (view-name
      (let ((view (org-files-db-dblock--view params 'search)))
        (cons (or (plist-get (cdr view) :expression)
                  (user-error "Search view `%s' has no :expression" (car view)))
              (or (plist-get (cdr view) :scope) 'all))))
     (t (user-error "Dynamic search block requires :expression or :view")))))

(defun org-files-db-dblock--insert-name (params)
  "Insert optional generated result name from PARAMS."
  (when-let* ((name (plist-get params :block-name)))
    (insert (format "#+name: %s\n" name))))

(defun org-files-db-dblock--render (results params)
  "Insert RESULTS according to dynamic-block PARAMS."
  (org-files-db-dblock--insert-name params)
  (insert
   (org-files-db-export-render-results
    results
    (org-files-db-dblock--layout params)
    (org-files-db-dblock--base-level))))

(defun org-files-db-dblock--protect (name function)
  "Run FUNCTION and report failures as dynamic block NAME errors."
  (condition-case err
      (funcall function)
    (error
     (signal 'org-files-db-error
             (list (format "%s dynamic block failed: %s"
                           name (error-message-string err)))))))

;;;###autoload
(cl-defun org-files-db-dblock-insert-query
    (query &key (config-file nil config-file-supplied-p)
           (sort nil sort-supplied-p))
  "Insert and update an org-files-db query dynamic block for QUERY.
CONFIG-FILE overrides the global configuration; an explicit nil writes
:config-file none.  SORT optionally controls result ordering."
  (interactive
   (list (prin1-to-string (org-files-db--read-sexp "Query: "))))
  (unless (derived-mode-p 'org-mode)
    (user-error "Org-files-db dynamic blocks require Org mode"))
  (let ((params (list :name "org-files-db-query"
                      :query query
                      :layout 'flat)))
    (when config-file-supplied-p
      (setq params (plist-put params :config-file
                              (or config-file 'none))))
    (when sort-supplied-p
      (setq params (plist-put params :sort (prin1-to-string sort))))
    (org-create-dblock params))
  (org-update-dblock))

(defun org-dblock-write:org-files-db-query (params)
  "Write an org-files-db query dynamic block using PARAMS."
  (org-files-db-dblock--protect
   "org-files-db-query"
   (lambda ()
     (let* ((query (org-files-db-dblock--query-definition params))
            (context (org-files-db--query-target query))
            (columns (org-files-db-dblock--columns params 'query context))
            (sort (org-files-db-dblock--sort
                   params 'query context columns))
            (config-file (org-files-db-dblock--config-file params 'query))
            (includes
             (delete-dups
              (append
               (org-files-db-dblock--query-includes params)
               (org-files-db--sort-includes
                sort columns context "Dynamic query block"))))
            (response (org-files-db--execute-query
                       query config-file "Dynamic query block"
                       includes))
            (results
             (org-files-db--results-with-config
              (org-files-db--normalize-results response)
              config-file))
            (sorted
             (org-files-db--sort-results
              results columns sort context "Dynamic query block")))
       (org-files-db-dblock--render sorted params)))))

;;;###autoload
(cl-defun org-files-db-dblock-insert-search
    (expression &key (config-file nil config-file-supplied-p)
                (sort nil sort-supplied-p))
  "Insert and update an org-files-db search dynamic block for EXPRESSION.
CONFIG-FILE overrides the global configuration; an explicit nil writes
:config-file none.  SORT optionally controls result ordering."
  (interactive (list (read-string "FTS5 search: ")))
  (unless (derived-mode-p 'org-mode)
    (user-error "Org-files-db dynamic blocks require Org mode"))
  (let ((params (list :name "org-files-db-search"
                      :expression expression
                      :scope 'all
                      :layout 'flat)))
    (when config-file-supplied-p
      (setq params (plist-put params :config-file
                              (or config-file 'none))))
    (when sort-supplied-p
      (setq params (plist-put params :sort (prin1-to-string sort))))
    (org-create-dblock params))
  (org-update-dblock))

(defun org-dblock-write:org-files-db-search (params)
  "Write an org-files-db search dynamic block using PARAMS."
  (org-files-db-dblock--protect
   "org-files-db-search"
   (lambda ()
     (pcase-let* ((`(,expression . ,scope)
                   (org-files-db-dblock--search-definition params))
                  (columns
                   (org-files-db-dblock--columns params 'search 'search))
                  (sort
                   (org-files-db-dblock--sort
                    params 'search 'search columns))
                  (config-file
                   (org-files-db-dblock--config-file params 'search))
                  (response (org-files-db--execute-search
                             expression scope config-file
                             "Dynamic search block"))
                  (results
                   (org-files-db--results-with-config
                    (org-files-db--normalize-results response)
                    config-file))
                  (sorted
                   (org-files-db--sort-results
                    results columns sort 'search "Dynamic search block")))
       (org-files-db-dblock--render sorted params)))))

;;;###autoload
(cl-defun org-files-db-dblock-insert-backlinks
    (&optional scope &key (config-file nil config-file-supplied-p))
  "Insert and update a backlink dynamic block for SCOPE.
CONFIG-FILE overrides the global configuration; an explicit nil writes
:config-file none."
  (interactive
   (list (intern
          (completing-read "Backlink scope: " '("heading" "file")
                           nil t nil nil "heading"))))
  (unless (derived-mode-p 'org-mode)
    (user-error "Org-files-db dynamic blocks require Org mode"))
  (let ((params (list :name "org-files-db-backlinks"
                      :scope (or scope 'heading)
                      :layout 'flat)))
    (when config-file-supplied-p
      (setq params (plist-put params :config-file
                              (or config-file 'none))))
    (org-create-dblock params))
  (org-update-dblock))

(defun org-files-db-dblock--backlink-query (params)
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

(defun org-dblock-write:org-files-db-backlinks (params)
  "Write an org-files-db backlink dynamic block using PARAMS."
  (org-files-db-dblock--protect
   "org-files-db-backlinks"
   (lambda ()
     (let* ((query (org-files-db-dblock--backlink-query params))
            (config-file
             (org-files-db-dblock--config-file params 'backlinks))
            (response (org-files-db--execute-query
                       query config-file "Dynamic backlinks block"
                       (org-files-db-dblock--query-includes params)))
            (results
             (org-files-db--results-with-config
              (org-files-db--normalize-results response)
              config-file)))
       (org-files-db-dblock--render results params)))))

(defun org-files-db-dblock--register ()
  "Register all org-files-db dynamic block types."
  (org-dynamic-block-define
   "org-files-db-query" #'org-files-db-dblock-insert-query)
  (org-dynamic-block-define
   "org-files-db-search" #'org-files-db-dblock-insert-search)
  (org-dynamic-block-define
   "org-files-db-backlinks" #'org-files-db-dblock-insert-backlinks))

(org-files-db-dblock--register)

(provide 'org-files-db-dblock)

;;; org-files-db-dblock.el ends here
