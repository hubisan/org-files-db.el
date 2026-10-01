;;; org-files-db-embark.el --- Optional Embark actions for org-files-db -*- lexical-binding: t; -*-

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

;; Author: Daniel Hubmann <hubisan@gmail.com>
;; Maintainer: Daniel Hubmann <hubisan@gmail.com>
;; URL: https://github.com/hubisan/org-files-db.el

;;; Commentary:

;; Result actions as Embark actions for the `org-files-db-result' completion
;; category, and an exporter that lists the currently filtered rows in an
;; `org-files-db-export-mode' buffer. Embark is never required here:
;; everything is registered only once Embark has been loaded.

;;; Code:

(require 'org-files-db-actions)
(require 'org-files-db-presentation)

(defvar embark-keymap-alist)
(defvar embark-exporters-alist)
(defvar embark-target-finders)

(defun org-files-db-embark--candidate-presentation (candidate)
  "Return the presentation for completion CANDIDATE."
  (or (and (stringp candidate)
           (> (length candidate) 0)
           (get-text-property 0 'org-files-db-presentation candidate))
      org-files-db-presentation--current-read-presentation
      (user-error "No presentation available for this result")))

(defun org-files-db-embark--resolve (candidate)
  "Return the cons (RECORD . PRESENTATION) for completion CANDIDATE."
  (let* ((presentation (org-files-db-embark--candidate-presentation candidate))
         (record (or (org-files-db-presentation--candidate-result
                      candidate presentation)
                     (user-error "Selected result is no longer available"))))
    (cons record presentation)))

(defun org-files-db-embark--call-with (action record presentation)
  "Call ACTION with RECORD and bind PRESENTATION and its configuration."
  (let ((org-files-db-actions--current-presentation presentation)
        (org-files-db-actions--current-action-config
         (org-files-db-presentation-config presentation)))
    (funcall action record)))

(defun org-files-db-embark--call (action candidate)
  "Call ACTION with the record of completion CANDIDATE.
Bind the presentation and configuration that reloading actions need."
  (let ((resolved (org-files-db-embark--resolve candidate)))
    (org-files-db-embark--call-with
     action (car resolved) (cdr resolved))))

(defmacro org-files-db-embark--define-action (name action doc)
  "Define Embark command NAME that runs ACTION on a result candidate.
DOC is the docstring."
  (declare (indent 2))
  `(defun ,name (candidate)
     ,doc
     (interactive "s")
     (org-files-db-embark--call #',action candidate)))

(org-files-db-embark--define-action org-files-db-embark-open-result
                                    org-files-db-actions-open-result
                                    "Open the result CANDIDATE.")

(org-files-db-embark--define-action org-files-db-embark-open-link-target
                                    org-files-db-actions-open-link-target
                                    "Open the target of the link result CANDIDATE.")

(org-files-db-embark--define-action org-files-db-embark-insert-file-link
                                    org-files-db-actions-insert-file-link
                                    "Insert a file link to the result CANDIDATE at point.")

(org-files-db-embark--define-action org-files-db-embark-insert-heading-link
                                    org-files-db-actions-insert-heading-link
                                    "Insert a link to the heading result CANDIDATE at point.")

(org-files-db-embark--define-action org-files-db-embark-follow-heading-link
                                    org-files-db-actions-follow-heading-link
                                    "Follow the first link in the heading result CANDIDATE.")

(org-files-db-embark--define-action org-files-db-embark-rename-file
                                    org-files-db-actions-rename-file
                                    "Rename the file of the result CANDIDATE.")

(defvar-keymap org-files-db-embark-result-map
  :doc "Embark actions for org-files-db results."
  "o" #'org-files-db-embark-open-result
  "t" #'org-files-db-embark-open-link-target
  "f" #'org-files-db-embark-insert-file-link
  "h" #'org-files-db-embark-insert-heading-link
  "l" #'org-files-db-embark-follow-heading-link
  "r" #'org-files-db-embark-rename-file)

;;; Export

(defun org-files-db-embark--line-text (candidate)
  "Return the visible row text of completion CANDIDATE."
  (or (and (> (length candidate) 0)
           (get-text-property 0 'display candidate)
           (let ((display (get-text-property 0 'display candidate)))
             (and (stringp display) (not (string-empty-p display)) display)))
      (substring-no-properties
       (replace-regexp-in-string
        "\u2063[\ue000-\uf8ff]*\\'" "" candidate))))

(defun org-files-db-embark--export-target ()
  "Return the Embark target for the export buffer line at point, or nil."
  (when (derived-mode-p 'org-files-db-export-mode)
    (when-let* ((candidate (get-text-property (line-beginning-position)
                                              'org-files-db-candidate)))
      `(org-files-db-result ,candidate
                            ,(line-beginning-position) . ,(line-end-position)))))

(defun org-files-db-embark-export-run-default-action ()
  "Run the default action for the result on the current line."
  (interactive)
  (let* ((record (or (get-text-property (line-beginning-position)
                                        'org-files-db-result)
                     (user-error "No result on this line")))
         (presentation (get-text-property (line-beginning-position)
                                          'org-files-db-presentation))
         (target (pcase (org-files-db-record-kind record)
                   ('heading 'headings)
                   ((or 'file 'root) 'files)
                   ('link 'links)
                   (kind (user-error "Unsupported result kind: %S" kind)))))
    (org-files-db-embark--call-with
     (org-files-db-actions--default-action target) record presentation)))

(defvar-keymap org-files-db-export-mode-map
  :doc "Keymap for `org-files-db-export-mode'."
  "RET" #'org-files-db-embark-export-run-default-action
  "n" #'next-line
  "p" #'previous-line)

(define-derived-mode org-files-db-export-mode special-mode "Org-files-db-export"
  "Major mode listing exported org-files-db result rows.
\\{org-files-db-export-mode-map}"
  (setq truncate-lines t))

(defun org-files-db-embark-export (candidates)
  "Export the filtered completion CANDIDATES to a result buffer.
Insert one line per candidate in the given order in a new buffer in
`org-files-db-export-mode' and switch to it."
  (let ((buffer (generate-new-buffer "*org-files-db export*")))
    (with-current-buffer buffer
      (org-files-db-export-mode)
      (let ((inhibit-read-only t))
        (dolist (candidate candidates)
          (let* ((resolved (org-files-db-embark--resolve candidate))
                 (start (point)))
            (insert (org-files-db-embark--line-text candidate) "\n")
            (add-text-properties
             start (point)
             (list 'org-files-db-result (car resolved)
                   'org-files-db-presentation (cdr resolved)
                   'org-files-db-candidate candidate))))
        (goto-char (point-min))))
    (pop-to-buffer-same-window buffer)))

(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist
               '(org-files-db-result . org-files-db-embark-result-map))
  (add-to-list 'embark-exporters-alist
               '(org-files-db-result . org-files-db-embark-export))
  (add-to-list 'embark-target-finders #'org-files-db-embark--export-target))

(provide 'org-files-db-embark)

;;; org-files-db-embark.el ends here
