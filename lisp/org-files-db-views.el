;;; org-files-db-views.el --- Predefined views -*- lexical-binding: t; -*-

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

;; Named wrappers around the generic query and search entry points, including
;; optional generation-aware prepared completion caches.

;;; Code:

(require 'org-files-db-cache)
(require 'org-files-db-query)
(require 'org-files-db-search)

(defcustom org-files-db-views nil
  "Named predefined query and search views.
Each entry has the form (NAME :command COMMAND ...), where COMMAND is
`query' or `search'.  An optional :config-file overrides
`org-files-db-config-file'; an explicit nil disables --config for that view.
An optional :sort overrides the configured default sorting; an explicit nil
preserves the order returned by orgfdb.
An optional :pre-cache t opts the view into generation-aware prepared
completion caching while `org-files-db-cache-mode' is enabled."
  :type '(repeat sexp)
  :group 'org-files-db)

(defun org-files-db-views--validate-name (view)
  "Return VIEW's validated name."
  (let ((name (car-safe view)))
    (unless (and (stringp name) (not (string-empty-p name)))
      (user-error "Invalid org-files-db view name in %S" view))
    name))

(defun org-files-db-views--command (view)
  "Return VIEW's validated command."
  (let ((command (plist-get (cdr view) :command)))
    (unless (memq command '(query search))
      (user-error "View `%s' has invalid :command %S"
                  (org-files-db-views--validate-name view) command))
    command))

(defun org-files-db-views--pre-cache-p (view)
  "Return non-nil when VIEW enables prepared pre-caching."
  (let* ((properties (cdr view))
         (present-p (not (null (plist-member properties :pre-cache))))
         (value (plist-get properties :pre-cache)))
    (when (and present-p (not (memq value '(nil t))))
      (user-error "View `%s' has invalid :pre-cache %S" (car view) value))
    (eq value t)))

(defun org-files-db-views--sort (view)
  "Return VIEW's effective validated sort specification."
  (let* ((properties (cdr view))
         (command (org-files-db-views--command view))
         (context
          (if (eq command 'query)
              (org-files-db--query-target (plist-get properties :query))
            'search))
         (columns
          (org-files-db--normalize-columns
           (or (plist-get properties :columns)
               (org-files-db--default-columns context nil))))
         (sort
          (org-files-db--effective-sort
           context
           (plist-get properties :sort)
           (not (null (plist-member properties :sort)))))
         (origin (format "View `%s'" (car view))))
    (org-files-db--normalize-sort sort columns context origin)
    sort))

(defun org-files-db-views--validate ()
  "Validate `org-files-db-views' and return it."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (view org-files-db-views)
      (unless (and (listp view) (cdr view))
        (user-error "Malformed org-files-db view: %S" view))
      (let ((name (org-files-db-views--validate-name view)))
        (when (gethash name seen)
          (user-error "Duplicate org-files-db view name: %s" name))
        (puthash name t seen)
        (org-files-db-views--command view)
        (org-files-db-views--pre-cache-p view)
        (org-files-db-views--sort view)))
    org-files-db-views))

(defun org-files-db-views-get (name)
  "Return a copy of predefined view NAME."
  (unless (stringp name)
    (user-error "View name must be a string"))
  (org-files-db-views--validate)
  (if-let* ((view (assoc name org-files-db-views #'string=)))
      (copy-tree view)
    (user-error "Unknown org-files-db view: %s" name)))

(defun org-files-db-views--names (command &optional pre-cache-only)
  "Return configured view names for COMMAND.
When PRE-CACHE-ONLY is non-nil, include only views with :pre-cache t."
  (org-files-db-views--validate)
  (mapcar #'car
          (seq-filter
           (lambda (view)
             (and (eq (org-files-db-views--command view) command)
                  (or (not pre-cache-only)
                      (org-files-db-views--pre-cache-p view))))
           org-files-db-views)))

(defun org-files-db-views--read-name (command &optional pre-cache-only)
  "Read a configured view name for COMMAND.
When PRE-CACHE-ONLY is non-nil, offer only views with :pre-cache t."
  (let ((names (org-files-db-views--names command pre-cache-only)))
    (unless names
      (user-error "No org-files-db %s views are configured" command))
    (completing-read
     (format "%s view: " (capitalize (symbol-name command)))
     names nil t nil nil (car names))))

(defun org-files-db-views--read-any-pre-cache-name ()
  "Read the name of any predefined view enabling :pre-cache t."
  (org-files-db-views--validate)
  (let ((names
         (mapcar #'car
                 (seq-filter #'org-files-db-views--pre-cache-p
                             org-files-db-views))))
    (unless names
      (user-error "No org-files-db views enable :pre-cache"))
    (completing-read "Cached view: " names nil t nil nil (car names))))

(defun org-files-db-views--action (view)
  "Return VIEW's optional action function."
  (let ((action (plist-get (cdr view) :action)))
    (when (and action (not (functionp action)))
      (user-error "View `%s' has invalid :action %S" (car view) action))
    action))

(defun org-files-db-views--config-file (view)
  "Return VIEW's effective validated configuration file."
  (let ((properties (cdr view)))
    (org-files-db--resolve-config-file
     (plist-get properties :config-file)
     (not (null (plist-member properties :config-file)))
     (format "View `%s'" (car view)))))

(defun org-files-db-views--read-index-state (config-file)
  "Return authoritative index state for effective CONFIG-FILE."
  (org-files-db-cache--read-index-state config-file))

(defun org-files-db-views--read-changes
    (config-file cached-database-id cached-generation)
  "Return CONFIG-FILE changes for CACHED-DATABASE-ID since CACHED-GENERATION."
  (org-files-db-cache--read-changes
   config-file cached-database-id cached-generation))

;;;###autoload
(cl-defun org-files-db-views-query (&optional name &key force-refresh)
  "Execute predefined query view NAME.
Interactively, offer only query views through standard completion.  A prefix
argument or FORCE-REFRESH bypasses and replaces any prepared cache."
  (interactive (list nil :force-refresh (not (null current-prefix-arg))))
  (let* ((name (or name (org-files-db-views--read-name 'query)))
         (view (org-files-db-views-get name)))
    (unless (eq (org-files-db-views--command view) 'query)
      (user-error "View `%s' is not a query view" name))
    (let ((query (plist-get (cdr view) :query))
          (columns (plist-get (cdr view) :columns))
          (action (org-files-db-views--action view))
          (sort (org-files-db-views--sort view))
          (config-file (org-files-db-views--config-file view)))
      (unless query
        (user-error "Query view `%s' has no :query" name))
      (if (and org-files-db-cache-mode
               (org-files-db-views--pre-cache-p view))
          (org-files-db-cache-present-view
           view config-file action "Query result: " force-refresh)
        (org-files-db-query
         query columns action :config-file config-file :sort sort)))))

;;;###autoload
(cl-defun org-files-db-views-search (&optional name &key force-refresh)
  "Execute predefined search view NAME.
Interactively, offer only search views through standard completion.  A prefix
argument or FORCE-REFRESH bypasses and replaces any prepared cache."
  (interactive (list nil :force-refresh (not (null current-prefix-arg))))
  (let* ((name (or name (org-files-db-views--read-name 'search)))
         (view (org-files-db-views-get name)))
    (unless (eq (org-files-db-views--command view) 'search)
      (user-error "View `%s' is not a search view" name))
    (let ((expression (plist-get (cdr view) :expression))
          (columns (plist-get (cdr view) :columns))
          (action (org-files-db-views--action view))
          (sort (org-files-db-views--sort view))
          (scope (or (plist-get (cdr view) :scope) 'all))
          (config-file (org-files-db-views--config-file view)))
      (unless (and (stringp expression)
                   (not (string-empty-p expression)))
        (user-error "Search view `%s' has no valid :expression" name))
      (if (and org-files-db-cache-mode
               (org-files-db-views--pre-cache-p view))
          (org-files-db-cache-present-view
           view config-file action "Search result: " force-refresh)
        (org-files-db-search
         expression columns action
         :scope scope
         :config-file config-file
         :sort sort)))))

;;;###autoload
(defun org-files-db-views-refresh-cache (name &optional synchronous)
  "Refresh prepared cache for predefined view NAME.
By default, refresh asynchronously.  With a prefix argument or SYNCHRONOUS
non-nil, perform a complete refresh immediately."
  (interactive
   (list (org-files-db-views--read-any-pre-cache-name)
         (not (null current-prefix-arg))))
  (let ((view (org-files-db-views-get name)))
    (org-files-db-cache-refresh-view
     view (org-files-db-views--config-file view) synchronous)))

;;;###autoload
(defun org-files-db-views-refresh-all-caches (&optional synchronous)
  "Refresh all configured prepared view caches.
By default, refresh asynchronously.  With a prefix argument or SYNCHRONOUS
non-nil, perform complete refreshes immediately."
  (interactive (list (not (null current-prefix-arg))))
  (org-files-db-cache-refresh-all synchronous))

;;;###autoload
(defun org-files-db-views-clear-cache (name)
  "Clear prepared cache and obsolete workers for predefined view NAME."
  (interactive (list (org-files-db-views--read-any-pre-cache-name)))
  (org-files-db-views-get name)
  (org-files-db-cache-clear-view name))

;;;###autoload
(defun org-files-db-views-clear-all-caches ()
  "Clear every prepared predefined-view cache and pending refresh."
  (interactive)
  (org-files-db-cache-clear-all))

(provide 'org-files-db-views)

;;; org-files-db-views.el ends here
