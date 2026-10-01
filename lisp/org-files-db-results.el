;;; org-files-db-results.el --- Result buffers for org-files-db -*- lexical-binding: t; -*-

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

;; Flat result buffers in `org-files-db-export-mode': one line per result row
;; with the record, presentation and candidate stored as line properties.
;; Used by the Embark exporter and by `org-files-db-show-query'. Embark is
;; not needed here.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-files-db-actions)
(require 'org-files-db-presentation)
(require 'org-files-db-process)
(require 'org-files-db-query)
(require 'org-files-db-outline)

(defvar-local org-files-db-results--refresh nil
  "Function returning a fresh presentation for this result buffer, or nil.")

(defvar-local org-files-db-results--action nil
  "Effective action for RET in this result buffer, or nil for the default.")

(defun org-files-db-results--candidate-presentation (candidate)
  "Return the presentation for completion CANDIDATE."
  (or (and (stringp candidate)
           (> (length candidate) 0)
           (get-text-property 0 'org-files-db-presentation candidate))
      org-files-db-presentation--current-read-presentation
      (user-error "No presentation available for this result")))

(defun org-files-db-results--resolve (candidate)
  "Return the cons (RECORD . PRESENTATION) for completion CANDIDATE."
  (let* ((presentation (org-files-db-results--candidate-presentation candidate))
         (record (or (org-files-db-presentation--candidate-result
                      candidate presentation)
                     (user-error "Selected result is no longer available"))))
    (cons record presentation)))

(defun org-files-db-results--call-with (action record presentation)
  "Call ACTION with RECORD and bind PRESENTATION and its configuration."
  (let ((org-files-db-actions--current-presentation presentation)
        (org-files-db-actions--current-action-config
         (org-files-db-presentation-config presentation)))
    (funcall action record)))

;;; Buffer

(defun org-files-db-results--line-text (candidate)
  "Return the visible row text of completion CANDIDATE."
  (or (and (> (length candidate) 0)
           (get-text-property 0 'display candidate)
           (let ((display (get-text-property 0 'display candidate)))
             (and (stringp display) (not (string-empty-p display)) display)))
      (substring-no-properties
       (replace-regexp-in-string
        "\u2063[\ue000-\uf8ff]*\\'" "" candidate))))

(defun org-files-db-results--insert (candidates)
  "Insert one line per completion candidate of CANDIDATES at point."
  (let ((inhibit-read-only t))
    (dolist (candidate candidates)
      (let* ((resolved (org-files-db-results--resolve candidate))
             (start (point)))
        (insert (org-files-db-results--line-text candidate) "\n")
        (add-text-properties
         start (point)
         (list 'org-files-db-result (car resolved)
               'org-files-db-presentation (cdr resolved)
               'org-files-db-candidate candidate))))
    (goto-char (point-min))))

(defun org-files-db-embark-export-run-default-action ()
  "Run the default action for the result on the current line.
In a result buffer with its own action, run that action."
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
    (org-files-db-results--call-with
     (or org-files-db-results--action
         (org-files-db-actions--default-action target))
     record presentation)))

(defun org-files-db-results-refresh ()
  "Re-run the query of this result buffer and update its rows.
Keep point on the same line number where possible."
  (interactive)
  (unless org-files-db-results--refresh
    (user-error "This buffer cannot be refreshed"))
  (org-files-db-results--fill (funcall org-files-db-results--refresh)))

(defvar-keymap org-files-db-export-mode-map
  :doc "Keymap for `org-files-db-export-mode'."
  "RET" #'org-files-db-embark-export-run-default-action
  "o" #'org-files-db-embark-export-outline
  "g" #'org-files-db-results-refresh
  "n" #'next-line
  "p" #'previous-line)

(define-derived-mode org-files-db-export-mode special-mode "Org-files-db-export"
  "Major mode listing org-files-db result rows.
\\{org-files-db-export-mode-map}"
  (setq truncate-lines t))

(defun org-files-db-results--fill (presentation)
  "Replace the rows of the current buffer with those of PRESENTATION."
  (let ((line (line-number-at-pos))
        (inhibit-read-only t)
        (candidates (org-files-db-presentation--candidates presentation)))
    (erase-buffer)
    (if candidates
        (org-files-db-results--insert candidates)
      (insert "No results\n"))
    (goto-char (point-min))
    (forward-line (1- line))
    (when (and (eobp) (not (bobp)))
      (forward-line -1))))

(defun org-files-db-results--show (buffer-name refresh action)
  "Show the presentation returned by REFRESH in buffer BUFFER-NAME.
REFRESH is called again to update the buffer. ACTION is the effective
action for RET. Reuse an existing buffer of that name and return it."
  (let ((presentation (funcall refresh))
        (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-files-db-export-mode)
        (org-files-db-export-mode))
      (setq org-files-db-results--refresh refresh
            org-files-db-results--action action)
      (org-files-db-results--fill presentation))
    (pop-to-buffer-same-window buffer)
    buffer))

;;;###autoload
(defun org-files-db-show-query (query &rest keys)
  "Show the rows of structural QUERY in a result buffer and return it.
KEYS are the keywords of `org-files-db-query': :config, :columns, :sort,
:row-source and :action. In the buffer, RET runs the action, o exports an
outline and g runs the query again."
  (interactive
   (cons (org-files-db-query--read-query)
         (list :config (org-files-db-process--interactive-config-name
                        current-prefix-arg))))
  (let* ((target (org-files-db-query--target query))
         (action (org-files-db-query--effective-action
                  target (plist-get keys :action)))
         (arguments (copy-sequence keys)))
    (cl-remf arguments :action)
    (org-files-db-results--show
     "*org-files-db: query*"
     (lambda () (apply #'org-files-db-query-results query arguments))
     action)))

;;; Outline export

(defun org-files-db-results--outline-candidates ()
  "Return the candidates of the rows in the current export buffer."
  (unless (derived-mode-p 'org-files-db-export-mode)
    (user-error "Run this in an org-files-db export buffer"))
  (let (candidates)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((candidate (get-text-property (point) 'org-files-db-candidate)))
          (push candidate candidates))
        (forward-line 1)))
    (nreverse candidates)))

(defun org-files-db-embark-export-outline (candidates)
  "Export the heading result CANDIDATES as an Org outline of links.
Interactively use the rows of the `org-files-db-export-mode' buffer, which
`embark-export' or `org-files-db-show-query' creates. See
`org-files-db-outline-export' for the outline and its options."
  (interactive (list (org-files-db-results--outline-candidates)))
  (let* ((resolved (mapcar #'org-files-db-results--resolve candidates))
         (presentation (cdar resolved))
         (records (mapcar #'car resolved)))
    (unless resolved
      (user-error "No org-files-db results to export"))
    (unless (cl-every (lambda (entry) (eq (cdr entry) presentation)) resolved)
      (user-error "Results come from different queries"))
    (org-files-db-outline-export records presentation)))

(provide 'org-files-db-results)

;;; org-files-db-results.el ends here
