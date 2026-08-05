;;; org-files-db-actions.el --- Result actions -*- lexical-binding: t; -*-

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

;; Reusable actions for inserting links, following linked headings, showing
;; backlinks, and renaming files while maintaining path-based links.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'org-files-db-core)

(declare-function org-files-db-query
                  "org-files-db-query"
                  (query &optional columns action &rest options))

(autoload 'org-files-db-query "org-files-db-query" nil t)

(defcustom org-files-db-file-link-style 'file
  "Default link style for file query results."
  :type '(choice (const file) (const id) (const custom-id))
  :group 'org-files-db)

(defcustom org-files-db-heading-link-style 'id
  "Default link style for heading query results."
  :type '(choice (const id) (const custom-id) (const heading))
  :group 'org-files-db)

(defun org-files-db-actions-open-result (result)
  "Open RESULT and jump to its stored source location.
RESULT may be an original result object or a propertized completion candidate."
  (interactive
   (list (get-text-property (point) 'org-files-db-result)))
  (when (stringp result)
    (setq result (get-text-property 0 'org-files-db-result result)))
  (org-files-db--visit-result result))

(defun org-files-db-actions--require-result-kind (result kinds)
  "Require RESULT to have one of KINDS and return its kind."
  (let ((kind (org-files-db--kind result)))
    (unless (memq kind kinds)
      (user-error "Expected an org-files-db %s result, got %s"
                  (mapconcat #'symbol-name kinds " or ")
                  (or kind "unknown")))
    kind))

(defun org-files-db-actions--at-result (result function)
  "Call FUNCTION in RESULT's Org buffer at its stored location."
  (let ((file (org-files-db--result-file result)))
    (unless (and file (file-readable-p file))
      (user-error "Result file is missing or unreadable: %s" (or file "<none>")))
    (with-current-buffer (find-file-noselect file)
      (save-restriction
        (widen)
        (org-files-db--goto-result-location result)
        (when (eq (org-files-db--kind result) 'heading)
          (org-back-to-heading t))
        (funcall function)))))

(defun org-files-db-actions--custom-id-base (title)
  "Return a readable CUSTOM_ID base derived from TITLE."
  (let* ((plain (downcase (org-link-display-format (or title "entry"))))
         (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" plain))
         (slug (string-trim slug "-+" "-+")))
    (if (string-empty-p slug)
        (substring (org-id-new) 0 8)
      slug)))

(defun org-files-db-actions--custom-id-used-p (custom-id)
  "Return non-nil when CUSTOM-ID already occurs in the current Org file."
  (let ((found
         (save-excursion
           (goto-char (point-min))
           (equal (org-entry-get nil "CUSTOM_ID") custom-id))))
    (unless found
      (org-map-entries
       (lambda ()
         (when (equal (org-entry-get nil "CUSTOM_ID") custom-id)
           (setq found t)))
       nil 'file))
    found))

(defun org-files-db-actions--new-custom-id (title)
  "Return a CUSTOM_ID unique in the current file for TITLE."
  (let ((base (org-files-db-actions--custom-id-base title))
        (candidate nil)
        (counter 1))
    (setq candidate base)
    (while (org-files-db-actions--custom-id-used-p candidate)
      (setq candidate (format "%s-%d" base counter)
            counter (1+ counter)))
    candidate))

(defun org-files-db-actions--ensure-result-property (result property)
  "Return RESULT's PROPERTY, creating and saving it when absent."
  (org-files-db-actions--at-result
   result
   (lambda ()
     (let ((value (org-entry-get nil property)))
       (unless value
         (setq value
               (pcase property
                 ("ID" (org-id-get-create))
                 ("CUSTOM_ID"
                  (let ((custom-id
                         (org-files-db-actions--new-custom-id
                          (org-files-db--result-title result))))
                    (org-entry-put nil "CUSTOM_ID" custom-id)
                    custom-id))
                 (_ (user-error "Unsupported identifier property: %s"
                                property))))
         (save-buffer))
       value))))

(defun org-files-db-actions--link-file-path (file origin-file)
  "Return FILE in the link style appropriate for ORIGIN-FILE."
  (let ((file (expand-file-name file)))
    (if (and origin-file
             (memq org-link-file-path-type '(relative adaptive)))
        (file-relative-name file (file-name-directory origin-file))
      file)))

(defun org-files-db-actions--make-file-link (file description &optional search)
  "Return an Org file link to FILE using DESCRIPTION and SEARCH."
  (let* ((origin buffer-file-name)
         (path (org-files-db-actions--link-file-path file origin))
         (target (concat "file:" (org-link-escape path)
                         (when search (concat "::" search)))))
    (org-link-make-string target description)))

(defun org-files-db-actions--insert-at-point (text)
  "Insert TEXT at point, replacing the active region when appropriate."
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert text))

(defun org-files-db-actions-insert-file-link (result &optional style)
  "Insert a link to file RESULT using STYLE at point."
  (org-files-db-actions--require-result-kind result '(file root))
  (let* ((style (or style org-files-db-file-link-style))
         (file (org-files-db--result-file result))
         (description (org-files-db--result-title result))
         (link
          (pcase style
            ('file (org-files-db-actions--make-file-link file description))
            ('id
             (org-link-make-string
              (concat "id:" (org-files-db-actions--ensure-result-property result "ID"))
              description))
            ('custom-id
             (org-files-db-actions--make-file-link
              file description
              (concat "#" (org-files-db-actions--ensure-result-property
                           result "CUSTOM_ID"))))
            (_ (user-error "Unsupported file link style: %S" style)))))
    (org-files-db-actions--insert-at-point link)
    link))

(defun org-files-db-actions-insert-heading-link (result &optional style)
  "Insert a link to heading RESULT using STYLE at point."
  (org-files-db-actions--require-result-kind result '(heading))
  (let* ((style (or style org-files-db-heading-link-style))
         (file (org-files-db--result-file result))
         (description (org-files-db--result-title result))
         (link
          (pcase style
            ('id
             (org-link-make-string
              (concat "id:" (org-files-db-actions--ensure-result-property result "ID"))
              description))
            ('custom-id
             (org-files-db-actions--make-file-link
              file description
              (concat "#" (org-files-db-actions--ensure-result-property
                           result "CUSTOM_ID"))))
            ('heading
             (org-files-db-actions--make-file-link
              file description (concat "*" description)))
            (_ (user-error "Unsupported heading link style: %S" style)))))
    (org-files-db-actions--insert-at-point link)
    link))

(defun org-files-db-actions--query-with-insertion-action
    (query columns action config-file)
  "Execute QUERY with COLUMNS and insertion ACTION at the original point.
CONFIG-FILE is the effective orgfdb configuration file."
  (let ((origin (copy-marker (point) t)))
    (unwind-protect
        (org-files-db-query
         query columns
         (lambda (result)
           (unless (marker-buffer origin)
             (user-error "The original insertion buffer no longer exists"))
           (with-current-buffer (marker-buffer origin)
             (goto-char origin)
             (funcall action result)))
         :config-file config-file)
      (set-marker origin nil))))

;;;###autoload
(cl-defun org-files-db-actions-query-insert-file-link
    (query columns &optional style
           &key (config-file nil config-file-supplied-p))
  "Select a file using QUERY and COLUMNS, then insert a link using STYLE.
CONFIG-FILE overrides `org-files-db-config-file'; an explicit nil disables it."
  (interactive
   (list (org-files-db--read-sexp "File query: ")
         org-files-db-file-columns
         org-files-db-file-link-style))
  (org-files-db-actions--query-with-insertion-action
   query columns
   (lambda (result)
     (org-files-db-actions-insert-file-link result style))
   (org-files-db--resolve-config-file
    config-file config-file-supplied-p "File-link query")))

;;;###autoload
(cl-defun org-files-db-actions-query-insert-heading-link
    (query columns &optional style
           &key (config-file nil config-file-supplied-p))
  "Select a heading using QUERY and COLUMNS, then insert a link using STYLE.
CONFIG-FILE overrides `org-files-db-config-file'; an explicit nil disables it."
  (interactive
   (list (org-files-db--read-sexp "Heading query: ")
         org-files-db-heading-columns
         org-files-db-heading-link-style))
  (org-files-db-actions--query-with-insertion-action
   query columns
   (lambda (result)
     (org-files-db-actions-insert-heading-link result style))
   (org-files-db--resolve-config-file
    config-file config-file-supplied-p "Heading-link query")))

(defun org-files-db-actions-follow-heading-link (result)
  "Follow the first supported Org link embedded in heading RESULT."
  (org-files-db-actions--require-result-kind result '(heading))
  (let ((info (org-files-db--heading-link-info result)))
    (unless info
      (user-error "The selected heading contains no supported file or ID link"))
    (org-files-db--goto-linked-target info)))

(defun org-files-db-actions--link-result-info (result)
  "Return parsed source-link information for link RESULT."
  (org-files-db-actions--at-result
   result
   (lambda ()
     (let ((element (org-element-context)))
       (unless (eq (org-element-type element) 'link)
         (user-error "Stored link location is stale; rebuild the index and retry"))
       (let ((type (org-element-property :type element)))
         (unless (equal type "file")
           (user-error "Only path-based file links can be rewritten"))
         (let* ((path (org-element-property :path element))
                (search (org-element-property :search-option element))
                (description
                 (when-let* ((contents (org-element-contents element)))
                   (org-element-interpret-data contents)))
                (format (or (org-files-db--get result 'format) "bracket")))
           (list :buffer (current-buffer)
                 :file buffer-file-name
                 :begin (org-element-property :begin element)
                 :end (- (org-element-property :end element)
                         (or (org-element-property :post-blank element) 0))
                 :path path
                 :search search
                 :description description
                 :format format)))))))

(defun org-files-db-actions--link-target-file (info)
  "Return the absolute linked file represented by INFO."
  (pcase-let* ((`(,path . ,_search)
                (org-files-db--split-file-link-path
                 (plist-get info :path)
                 (plist-get info :search)))
               (path (org-link-unescape path)))
    (expand-file-name path (file-name-directory (plist-get info :file)))))

(defun org-files-db-actions--rewritten-link-path (info new-path &optional old-path)
  "Return NEW-PATH in the original path style of link INFO.
When INFO belongs to OLD-PATH itself, calculate a relative path from the
renamed source location."
  (pcase-let* ((`(,stored-path . ,_search)
                (org-files-db--split-file-link-path
                 (plist-get info :path)
                 (plist-get info :search)))
               (stored-path (org-link-unescape stored-path))
               (source-file (plist-get info :file))
               (source-after-rename
                (if (and old-path
                         (file-equal-p source-file old-path))
                    new-path
                  source-file)))
    (if (file-name-absolute-p stored-path)
        (expand-file-name new-path)
      (file-relative-name (expand-file-name new-path)
                          (file-name-directory source-after-rename)))))

(defun org-files-db-actions--format-rewritten-link (info new-path &optional old-path)
  "Return source link INFO rewritten from OLD-PATH to NEW-PATH."
  (let* ((path (org-link-escape
                (org-files-db-actions--rewritten-link-path info new-path old-path)))
         (search (or (plist-get info :search)
                     (cdr (org-files-db--split-file-link-path
                           (plist-get info :path) nil))))
         (target (concat "file:" path (when search (concat "::" search))))
         (description (plist-get info :description)))
    (pcase (plist-get info :format)
      ("plain" target)
      ("angle" (format "<%s>" target))
      (_ (org-link-make-string target description)))))

(cl-defun org-files-db-actions--incoming-file-link-results
    (old-path &optional (config-file nil config-file-supplied-p))
  "Return resolved path-based links targeting OLD-PATH.
CONFIG-FILE selects the effective orgfdb configuration."
  (let* ((effective-config-file
          (org-files-db--resolve-config-file
           config-file config-file-supplied-p "Rename action"))
         (response
          (org-files-db--execute-query
           `(links
             (and
              (link-type "file")
              (target
               (files (file-path ,(expand-file-name old-path) :exact t)))))
           effective-config-file
           "Rename action")))
    (org-files-db--results-with-config
     (org-files-db--normalize-results response)
     effective-config-file)))

(defun org-files-db-actions--writable-parent-directory-p (path)
  "Return non-nil when PATH has a writable existing parent directory."
  (let ((directory (file-name-directory (expand-file-name path)))
        parent)
    (while (and directory (not (file-exists-p directory)))
      (setq parent (file-name-directory (directory-file-name directory)))
      (setq directory (unless (equal parent directory) parent)))
    (and directory
         (file-directory-p directory)
         (file-writable-p directory))))

(defun org-files-db-actions--validate-rename (old-path new-path link-results)
  "Validate rename from OLD-PATH to NEW-PATH using LINK-RESULTS.
Return link edit records."
  (unless (file-readable-p old-path)
    (user-error "Source file is missing or unreadable: %s" old-path))
  (when (file-exists-p new-path)
    (user-error "Rename destination already exists: %s" new-path))
  (unless (file-writable-p (file-name-directory old-path))
    (user-error "Source directory is not writable: %s"
                (file-name-directory old-path)))
  (unless (org-files-db-actions--writable-parent-directory-p new-path)
    (user-error "Rename destination has no writable parent: %s" new-path))
  (let (edits)
    (dolist (result link-results)
      (let ((source (org-files-db--result-file result)))
        (unless (and source (file-writable-p source))
          (user-error "Link source file is not writable: %s" (or source "<none>")))
        (let ((info (org-files-db-actions--link-result-info result)))
          (unless (file-equal-p (org-files-db-actions--link-target-file info) old-path)
            (user-error "Stored link target is stale in %s; rebuild and retry"
                        source))
          (push (plist-put info :replacement
                           (org-files-db-actions--format-rewritten-link
                            info new-path old-path))
                edits))))
    edits))

(defun org-files-db-actions--modified-rename-buffers (edits old-path)
  "Return modified visiting buffers affected by EDITS or OLD-PATH."
  (let ((buffers (mapcar (lambda (edit) (plist-get edit :buffer)) edits)))
    (when-let* ((target (get-file-buffer old-path)))
      (push target buffers))
    (seq-uniq
     (seq-filter (lambda (buffer)
                   (and (buffer-live-p buffer)
                        (buffer-modified-p buffer)))
                 buffers))))

(defun org-files-db-actions--apply-link-edits (edits)
  "Apply and save validated link EDITS."
  (let ((buffers
         (seq-uniq (mapcar (lambda (edit) (plist-get edit :buffer)) edits))))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (let ((buffer-edits
               (sort
                (seq-filter
                 (lambda (edit) (eq (plist-get edit :buffer) buffer))
                 edits)
                (lambda (left right)
                  (> (plist-get left :begin) (plist-get right :begin))))))
          (save-excursion
            (dolist (edit buffer-edits)
              (goto-char (plist-get edit :begin))
              (delete-region (plist-get edit :begin) (plist-get edit :end))
              (insert (plist-get edit :replacement))))
          (save-buffer))))))

(defun org-files-db-actions--rename-visited-file (old-path new-path)
  "Rename OLD-PATH to NEW-PATH and update its visiting buffer."
  (if-let* ((buffer (get-file-buffer old-path)))
      (with-current-buffer buffer
        (rename-file old-path new-path)
        (set-visited-file-name new-path t))
    (rename-file old-path new-path)))

;;;###autoload
(cl-defun org-files-db-actions-rename-file
    (file new-path &optional confirm
          &key (config-file nil config-file-supplied-p))
  "Rename indexed FILE to NEW-PATH and update resolved file links.
When CONFIRM is non-nil, ask before modifying files.  CONFIG-FILE overrides
`org-files-db-config-file'; an explicit nil disables it."
  (interactive
   (let* ((file (or buffer-file-name
                    (read-file-name "Indexed Org file: ")))
          (new (read-file-name "Rename to: "
                               (file-name-directory file))))
     (list file new)))
  (let* ((effective-config-file
          (org-files-db--resolve-config-file
           config-file config-file-supplied-p "Rename action"))
         (old-path (expand-file-name file))
         (new-path (expand-file-name new-path))
         (link-results
          (org-files-db-actions--incoming-file-link-results
           old-path effective-config-file))
         (edits (org-files-db-actions--validate-rename
                 old-path new-path link-results))
         (modified (org-files-db-actions--modified-rename-buffers edits old-path)))
    (when (and modified
               (not (yes-or-no-p
                     (format (concat "%d affected buffer(s) have unsaved changes; "
                                     "save and continue? ")
                             (length modified)))))
      (user-error "Rename cancelled"))
    (when (and (or confirm (called-interactively-p 'interactive))
               (not (yes-or-no-p
                     (format "Rename %s and update %d link(s) in %d file(s)? "
                             (file-name-nondirectory old-path)
                             (length edits)
                             (length (seq-uniq
                                      (mapcar (lambda (edit)
                                                (plist-get edit :file))
                                              edits)))))))
      (user-error "Rename cancelled"))
    (condition-case err
        (progn
          (dolist (buffer modified)
            (with-current-buffer buffer
              (save-buffer)))
          (make-directory (file-name-directory new-path) t)
          (org-files-db-actions--apply-link-edits edits)
          (org-files-db-actions--rename-visited-file old-path new-path)
          (message "Renamed %s and updated %d link(s)"
                   old-path (length edits))
          new-path)
      (error
       (signal 'org-files-db-error
               (list
                (format
                 (concat "File rename stopped after a partial failure: %s. "
                         "Review %s and affected source files with version control")
                 (error-message-string err) old-path)))))))

(defun org-files-db-actions-rename-file-result (result)
  "Prompt to rename file RESULT and update path-based links."
  (org-files-db-actions--require-result-kind result '(file root))
  (let* ((file (org-files-db--result-file result))
         (new-path (read-file-name "Rename to: "
                                   (file-name-directory file)
                                   nil nil
                                   (file-name-nondirectory file))))
    (org-files-db-actions-rename-file
     file new-path t
     :config-file
     (org-files-db--result-config-file result "Rename result action"))))

;;;###autoload
(cl-defun org-files-db-actions-backlinks
    (&key (config-file nil config-file-supplied-p))
  "Show indexed links pointing to the file or heading at point.
CONFIG-FILE overrides `org-files-db-config-file'; an explicit nil disables it."
  (interactive)
  (let ((effective-config-file
         (org-files-db--resolve-config-file
          config-file config-file-supplied-p "Backlinks")))
    (org-files-db-query
     (org-files-db--backlinks-query-at-point)
     org-files-db-link-columns
     nil
     :config-file effective-config-file)))

(provide 'org-files-db-actions)

;;; org-files-db-actions.el ends here
