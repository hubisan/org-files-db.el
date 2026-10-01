;;; org-files-db-actions.el --- Result action foundation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Hubmann

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Default action selection, dynamic action context and the open actions for
;; headings, files, link sources and link targets.

;;; Code:

(require 'org-files-db-core)
(require 'org-files-db-presentation)
(require 'org-files-db-process)
(require 'ol)
(require 'org-element)
(require 'seq)

(declare-function org-fold-show-context "org-fold" (&optional key))
(declare-function org-files-db-reload-results "org-files-db-query"
                  (records presentation &rest args))
(declare-function org-files-db-query--guarded-json "org-files-db-query"
                  (query-string presentation &rest args))

(defvar org-files-db-actions--current-action-config nil
  "Effective configuration name while an org-files-db action runs.")

(defvar org-files-db-actions--current-presentation nil
  "Presentation whose row is selected while an org-files-db action runs.")

(defun org-files-db-current-presentation ()
  "Return the presentation of the current result action.
Return nil outside an org-files-db result action."
  org-files-db-actions--current-presentation)

(defun org-files-db-current-config ()
  "Return the effective configuration name for the current result action.
Return nil outside an org-files-db result action."
  org-files-db-actions--current-action-config)

(defun org-files-db-actions--move-to (line byte-start)
  "Move point in the current buffer to BYTE-START or, failing that, LINE.
BYTE-START is a 0-based byte offset. When the line at that position differs
from LINE, move to LINE instead. Without either, move to the start of the
buffer. Widen the buffer first."
  (widen)
  (goto-char (point-min))
  (let ((position (and byte-start (byte-to-position (1+ byte-start)))))
    (cond
     ((and position (or (null line)
                        (= (line-number-at-pos position) line)))
      (goto-char position))
     (line
      (goto-char (point-min))
      (forward-line (1- line))))))

(defun org-files-db-actions--goto (file line byte-start)
  "Visit FILE and move point to BYTE-START or, failing that, LINE.
BYTE-START is a 0-based byte offset in FILE. When the line at that position
differs from LINE, move to LINE instead. Without either, stay at the start of
the buffer. Reveal the location when the buffer is in Org mode."
  (find-file file)
  (org-files-db-actions--move-to line byte-start)
  (when (derived-mode-p 'org-mode)
    (org-fold-show-context)))

(defun org-files-db-actions-open-result (record)
  "Open the source of RECORD and return nil.
A heading or link RECORD opens its file at the stored position, a file or root
RECORD opens its file at the start. No orgfdb process is started."
  (let ((kind (org-files-db-record-kind record)))
    (if (memq kind '(heading link))
        (org-files-db-actions--goto (org-files-db-record-file record)
                                    (org-files-db-record-line record)
                                    (org-files-db-record-byte-start record))
      (org-files-db-actions--goto (org-files-db-record-file record) nil nil))
    (message (pcase kind
               ('heading "Heading opened")
               ('link "Link opened")
               (_ "File opened")))
    nil))

(defun org-files-db-actions-open-link-target (record)
  "Open the target of link RECORD and return nil.
A heading target opens at its position, a file target at the start of the
file. Signal a user error for a non-link RECORD or an unresolved target. No
orgfdb process is started."
  (unless (eq (org-files-db-record-kind record) 'link)
    (user-error "Result is not a link"))
  (unless (org-files-db-record-target-resolved-p record)
    (user-error "Link target is not resolved"))
  (org-files-db-actions--goto (org-files-db-record-target-file record)
                              (org-files-db-record-target-line record)
                              (org-files-db-record-target-byte-start record))
  (message "Link target opened")
  nil)

;; The query module requires this one, so reloading is declared, not required.
(defun org-files-db-actions--reload-record (record includes)
  "Reload RECORD with INCLUDES and return its result alist.
Signal a user error outside a result action or when the index changed."
  (unless org-files-db-actions--current-presentation
    (user-error "No query context for this action"))
  (condition-case nil
      (let ((id (org-files-db-record-id record)))
        (or (seq-find (lambda (result) (equal (alist-get 'id result) id))
                      (org-files-db-reload-results
                       (list record) org-files-db-actions--current-presentation
                       :includes includes))
            (user-error "Result no longer exists")))
    (org-files-db-stale-index
     (user-error "Index changed, run the query again"))))

(defun org-files-db-actions--link-path (file)
  "Return FILE relative to the current buffer or abbreviated absolute."
  (if buffer-file-name
      (file-relative-name file (file-name-directory buffer-file-name))
    (abbreviate-file-name file)))

(defun org-files-db-actions-insert-file-link (record)
  "Insert an Org file link to the file of RECORD at point and return nil.
RECORD must be a file or root record. The description is the file title,
or the file name when there is no title."
  (unless (memq (org-files-db-record-kind record) '(file root))
    (user-error "Result is not a file"))
  (let* ((result (org-files-db-actions--reload-record record nil))
         (title (alist-get 'title result))
         (description (if (and (stringp title) (not (string-empty-p title)))
                          title
                        (alist-get 'name result)))
         (path (org-files-db-actions--link-path
                (org-files-db-record-file record))))
    (insert (org-link-make-string (concat "file:" path) description))
    (message "File link inserted")
    nil))

(defun org-files-db-actions--property (result key)
  "Return the non-empty value of property KEY in RESULT, or nil."
  (when-let* ((entry (seq-find (lambda (property)
                                 (equal (alist-get 'key property) key))
                               (alist-get 'properties result)))
              (value (alist-get 'value entry)))
    (and (stringp value) (not (string-empty-p value)) value)))

(defun org-files-db-actions-insert-heading-link (record)
  "Insert an Org link to the heading of RECORD at point and return nil.
Prefer an `id:' link, then a file link with the custom ID, and otherwise a
file link with the heading title as search option."
  (unless (eq (org-files-db-record-kind record) 'heading)
    (user-error "Result is not a heading"))
  (let* ((result (org-files-db-actions--reload-record record '("properties")))
         (title (alist-get 'title result))
         (id (org-files-db-actions--property result "ID"))
         (custom-id (org-files-db-actions--property result "CUSTOM_ID"))
         (path (org-files-db-actions--link-path
                (org-files-db-record-file record)))
         (target (cond
                  (id (concat "id:" id))
                  (custom-id (concat "file:" path "::#" custom-id))
                  (t (concat "file:" path "::*" title)))))
    (insert (org-link-make-string target title))
    (message "Heading link inserted")
    nil))

(defun org-files-db-actions--first-link (line byte-start)
  "Return the first link of the heading at BYTE-START or LINE.
Search the headline first, then the section body, in the current buffer. Point
and restriction are preserved. Return nil when the heading has no link."
  (save-excursion
    (save-restriction
      (org-files-db-actions--move-to line byte-start)
      (goto-char (line-beginning-position))
      (let ((start (point))
            (headline-end (line-end-position))
            (section-end (save-excursion
                           (forward-line 1)
                           (if (re-search-forward org-heading-regexp nil t)
                               (match-beginning 0)
                             (point-max)))))
        (when (or (re-search-forward org-link-any-re headline-end t)
                  (progn (goto-char start)
                         (re-search-forward org-link-any-re section-end t)))
          (goto-char (match-beginning 0))
          (org-element-link-parser))))))

(defun org-files-db-actions-follow-heading-link (record)
  "Open the first link in the heading of RECORD and return nil.
Search the headline first, then the section body. The heading buffer is not
displayed and is killed afterwards when it was not open before and is
unmodified. Relative file links resolve against the directory of the heading
file. No orgfdb process is started. Signal a user error when the heading has
no link."
  (unless (eq (org-files-db-record-kind record) 'heading)
    (user-error "Result is not a heading"))
  (let* ((file (org-files-db-record-file record))
         (existing (find-buffer-visiting file))
         (buffer (or existing (find-file-noselect file))))
    (unwind-protect
        (let ((link (with-current-buffer buffer
                      (org-files-db-actions--first-link
                       (org-files-db-record-line record)
                       (org-files-db-record-byte-start record)))))
          (unless link
            (user-error "Heading has no link"))
          (let ((default-directory (file-name-directory
                                    (expand-file-name file))))
            ;; Links without a file resolve in the current buffer.
            (when (member (org-element-property :type link)
                          '("fuzzy" "custom-id" "coderef"))
              (pop-to-buffer-same-window buffer))
            (org-link-open link))
          (message "Heading link followed")
          nil)
      (when (and (not existing)
                 (buffer-live-p buffer)
                 (not (eq buffer (current-buffer)))
                 (not (get-buffer-window buffer t))
                 (not (buffer-modified-p buffer)))
        (kill-buffer buffer)))))

;; Renaming a file rewrites the indexed incoming links, so the index is
;; queried and guarded before anything on disk changes.
(defun org-files-db-actions--rename-link-path (link-path new-file source)
  "Return the path for NEW-FILE to use in a link of LINK-PATH in SOURCE.
Keep the link relative to the directory of SOURCE when LINK-PATH is relative."
  (cond
   ((string-prefix-p "~" link-path) (abbreviate-file-name new-file))
   ((file-name-absolute-p link-path) new-file)
   (t (file-relative-name new-file (file-name-directory source)))))

(defun org-files-db-actions--rename-link-string (target description format)
  "Return a link to TARGET, a path with optional search option.
DESCRIPTION is the link description or nil. FORMAT is the link format."
  (let ((target (concat "file:" target)))
    (pcase format
      ("angle" (concat "<" target ">"))
      ("plain" target)
      (_ (org-link-make-string target description)))))

(defun org-files-db-actions--rewrite-link (link source new-file)
  "Rewrite incoming LINK in the current buffer for NEW-FILE and return non-nil.
SOURCE is the current path of the file being edited. Return nil without
changes when the buffer text does not match the indexed link."
  (let* ((location (alist-get 'location link))
         (start (byte-to-position (1+ (alist-get 'byte_start location))))
         (end (byte-to-position (1+ (alist-get 'byte_end location))))
         (search-option (alist-get 'search_option link))
         (description (alist-get 'raw_description link)))
    (when (and start end
               (equal (buffer-substring-no-properties start end)
                      (alist-get 'raw link)))
      (let ((path (org-files-db-actions--rename-link-path
                   (alist-get 'link_path link) new-file source)))
        (goto-char start)
        (delete-region start end)
        (insert (org-files-db-actions--rename-link-string
                 (concat path (when search-option (concat "::" search-option)))
                 (and (stringp description)
                      (not (string-empty-p description))
                      description)
                 (alist-get 'format link)))
        t))))

(defun org-files-db-actions--rename-visiting-buffer (old-file new-file)
  "Make a buffer visiting OLD-FILE visit NEW-FILE, keeping its modified state."
  (when-let* ((buffer (find-buffer-visiting old-file)))
    (with-current-buffer buffer
      (let ((modified (buffer-modified-p)))
        (set-visited-file-name new-file t)
        (set-buffer-modified-p modified)
        (unless modified
          (set-visited-file-modtime))))))

(defun org-files-db-actions-rename-file (record &optional new-name)
  "Rename the file of RECORD to NEW-NAME and update incoming file links.
RECORD must be a file or root record. Read NEW-NAME with `read-file-name'
when nil. Reload the indexed incoming links first so a changed index leaves
everything untouched. Links whose text no longer matches the index are
skipped. Return nil."
  (unless (memq (org-files-db-record-kind record) '(file root))
    (user-error "Result is not a file"))
  (unless org-files-db-actions--current-presentation
    (user-error "No query context for this action"))
  (let* ((old-file (expand-file-name (org-files-db-record-file record)))
         (links (condition-case nil
                    (org-files-db-query--guarded-json
                     (format "(links (target (files (id %d))))"
                             (org-files-db-record-id record))
                     org-files-db-actions--current-presentation)
                  (org-files-db-stale-index
                   (user-error "Index changed, run the query again"))))
         (new-file (expand-file-name
                    (or new-name
                        (read-file-name "Rename file to: "
                                        (file-name-directory old-file)
                                        old-file nil
                                        (file-name-nondirectory old-file)))))
         (updated 0)
         (skipped 0)
         (by-source nil))
    (when (directory-name-p new-file)
      (setq new-file (expand-file-name (file-name-nondirectory old-file)
                                       new-file)))
    (when (file-exists-p new-file)
      (user-error "File already exists: %s" new-file))
    (rename-file old-file new-file)
    (org-files-db-actions--rename-visiting-buffer old-file new-file)
    (seq-doseq (link links)
      (when (equal (alist-get 'link_type link) "file")
        (let* ((path (alist-get 'file_path (alist-get 'location link)))
               (source (if (string= (expand-file-name path) old-file) new-file path)))
          (push link (alist-get source by-source nil nil #'equal)))))
    (dolist (entry by-source)
      (let ((source (car entry))
            (buffer (find-file-noselect (car entry))))
        (with-current-buffer buffer
          (save-excursion
            (save-restriction
              (widen)
              (dolist (link (sort (cdr entry)
                                  (lambda (a b)
                                    (> (alist-get 'byte_start (alist-get 'location a))
                                       (alist-get 'byte_start (alist-get 'location b))))))
                (if (org-files-db-actions--rewrite-link link source new-file)
                    (setq updated (1+ updated))
                  (setq skipped (1+ skipped))))))
          (when (buffer-modified-p)
            (save-buffer)))))
    (message "File renamed, %d links updated%s" updated
             (if (> skipped 0) (format ", %d skipped" skipped) ""))
    nil))

(defun org-files-db-actions--default-action (target)
  "Return the configured default action for TARGET."
  (pcase target
    ('headings org-files-db-heading-action)
    ('files org-files-db-file-action)
    ('links org-files-db-link-action)
    (_ (user-error "Unsupported org-files-db query target: %S" target))))

(provide 'org-files-db-actions)

;;; org-files-db-actions.el ends here
