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
(require 'seq)

(declare-function org-fold-show-context "org-fold" (&optional key))
(declare-function org-files-db-reload-results "org-files-db-query"
                  (records presentation &rest args))

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

(defun org-files-db-actions--goto (file line byte-start)
  "Visit FILE and move point to BYTE-START or, failing that, LINE.
BYTE-START is a 0-based byte offset in FILE. When the line at that position
differs from LINE, move to LINE instead. Without either, stay at the start of
the buffer. Reveal the location when the buffer is in Org mode."
  (find-file file)
  (widen)
  (goto-char (point-min))
  (let ((position (and byte-start (byte-to-position (1+ byte-start)))))
    (cond
     ((and position (or (null line)
                        (= (line-number-at-pos position) line)))
      (goto-char position))
     (line
      (goto-char (point-min))
      (forward-line (1- line)))))
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

(defun org-files-db-actions-follow-heading-link (record)
  "Open the first link in the heading of RECORD and return nil.
Search the headline first, then the section body. No orgfdb process is
started. Signal a user error when the heading has no link."
  (unless (eq (org-files-db-record-kind record) 'heading)
    (user-error "Result is not a heading"))
  (org-files-db-actions--goto (org-files-db-record-file record)
                              (org-files-db-record-line record)
                              (org-files-db-record-byte-start record))
  (goto-char (line-beginning-position))
  (let ((start (point))
        (headline-end (line-end-position))
        (section-end (save-excursion
                       (forward-line 1)
                       (if (re-search-forward org-heading-regexp nil t)
                           (match-beginning 0)
                         (point-max)))))
    (unless (or (re-search-forward org-link-any-re headline-end t)
                (progn (goto-char start)
                       (re-search-forward org-link-any-re section-end t)))
      (goto-char start)
      (user-error "Heading has no link"))
    (goto-char (match-beginning 0))
    (org-open-at-point)
    (message "Heading link followed")
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
