;;; org-files-db-presentation.el --- Presentation defaults for org-files-db -*- lexical-binding: t; -*-

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

;; User defaults for Rust-prepared query presentation.

;;; Code:

(require 'org-files-db-process)

(defcustom org-files-db-heading-columns
  '((todo-keyword :width (max 10))
    (priority :width (fixed 3))
    (outline-path :width (max 80))
    (file-name :width (max 30)))
  "Default columns for heading query results."
  :type '(repeat sexp)
  :group 'org-files-db)

(defcustom org-files-db-file-columns
  '((file-title :width (max 50))
    (file-path :width (max 100)))
  "Default columns for file query results."
  :type '(repeat sexp)
  :group 'org-files-db)

(defcustom org-files-db-link-columns
  '((link-type :width (max 12))
    (link-target :width (max 60))
    (source-outline-path :width (max 70))
    (file-name :width (max 30)))
  "Default columns for link query results."
  :type '(repeat sexp)
  :group 'org-files-db)

(defcustom org-files-db-heading-sort nil
  "Default sorting for heading query results."
  :type '(repeat sexp)
  :group 'org-files-db)

(defcustom org-files-db-file-sort nil
  "Default sorting for file query results."
  :type '(repeat sexp)
  :group 'org-files-db)

(defcustom org-files-db-link-sort nil
  "Default sorting for link query results."
  :type '(repeat sexp)
  :group 'org-files-db)

(defun org-files-db--default-columns (target)
  "Return the configured default columns for TARGET."
  (pcase target
    ('headings org-files-db-heading-columns)
    ('files org-files-db-file-columns)
    ('links org-files-db-link-columns)
    (_ (user-error "Unsupported org-files-db query target: %S" target))))

(defun org-files-db--default-sort (target)
  "Return the configured default sorting for TARGET."
  (pcase target
    ('headings org-files-db-heading-sort)
    ('files org-files-db-file-sort)
    ('links org-files-db-link-sort)
    (_ (user-error "Unsupported org-files-db query target: %S" target))))

(provide 'org-files-db-presentation)

;;; org-files-db-presentation.el ends here
