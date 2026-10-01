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

(declare-function org-fold-show-context "org-fold" (&optional key))

(defvar org-files-db-actions--current-action-config nil
  "Effective configuration name while an org-files-db action runs.")

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

(defun org-files-db-actions--default-action (target)
  "Return the configured default action for TARGET."
  (pcase target
    ('headings org-files-db-heading-action)
    ('files org-files-db-file-action)
    ('links org-files-db-link-action)
    (_ (user-error "Unsupported org-files-db query target: %S" target))))

(provide 'org-files-db-actions)

;;; org-files-db-actions.el ends here
