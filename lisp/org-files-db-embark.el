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
;; category. Embark is never required here: the keymap is registered only
;; once Embark has been loaded.

;;; Code:

(require 'org-files-db-actions)
(require 'org-files-db-presentation)

(defvar embark-keymap-alist)

(defun org-files-db-embark--candidate-presentation (candidate)
  "Return the presentation for completion CANDIDATE."
  (or (and (stringp candidate)
           (> (length candidate) 0)
           (get-text-property 0 'org-files-db-presentation candidate))
      org-files-db-presentation--current-read-presentation
      (user-error "No presentation available for this result")))

(defun org-files-db-embark--call (action candidate)
  "Call ACTION with the record of completion CANDIDATE.
Bind the presentation and configuration that reloading actions need."
  (let* ((presentation (org-files-db-embark--candidate-presentation candidate))
         (record (or (org-files-db-presentation--candidate-result
                      candidate presentation)
                     (user-error "Selected result is no longer available")))
         (org-files-db-actions--current-presentation presentation)
         (org-files-db-actions--current-action-config
          (org-files-db-presentation-config presentation)))
    (funcall action record)))

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

(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist
               '(org-files-db-result . org-files-db-embark-result-map)))

(provide 'org-files-db-embark)

;;; org-files-db-embark.el ends here
