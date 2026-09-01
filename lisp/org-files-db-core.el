;;; org-files-db-core.el --- Shared customization for org-files-db -*- lexical-binding: t; -*-

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

;; Shared user customization, public faces, and package-wide definitions.
;; Specialized modules keep their implementation logic outside Core.

;;; Code:

(require 'org)
(require 'org-faces)

(defgroup org-files-db nil
  "Emacs interface for orgfdb."
  :group 'org
  :prefix "org-files-db-")

(define-error 'org-files-db-error "org-files-db error")

(defcustom org-files-db-executable "orgfdb"
  "Path or command name of the orgfdb executable."
  :type 'string
  :group 'org-files-db)

(defcustom org-files-db-configs nil
  "Named orgfdb configuration files.
Each entry has the form (NAME . FILE).  NAME is a unique non-empty string.
FILE is the configuration file for that name."
  :type '(alist :key-type string :value-type file)
  :group 'org-files-db)

(defcustom org-files-db-default-config nil
  "Name of the default entry in `org-files-db-configs'."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'org-files-db)

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

(defcustom org-files-db-heading-action 'org-files-db-actions-open-result
  "Default action for heading query results."
  :type 'function
  :group 'org-files-db)

(defcustom org-files-db-file-action 'org-files-db-actions-open-result
  "Default action for file query results."
  :type 'function
  :group 'org-files-db)

(defcustom org-files-db-link-action 'org-files-db-actions-open-result
  "Default action for link query results."
  :type 'function
  :group 'org-files-db)

(defcustom org-files-db-views nil
  "Named org-files-db query views.
Each entry starts with a unique view name.  A :config value selects one
entry from `org-files-db-configs'.  A view without :config uses
`org-files-db-default-config'."
  :type '(repeat sexp)
  :group 'org-files-db)

(defface org-files-db-heading-1
  '((t (:inherit org-level-1 :height 1.0)))
  "Face for level 1 headings in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-heading-2
  '((t (:inherit org-level-2 :height 1.0)))
  "Face for level 2 headings in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-heading-3
  '((t (:inherit org-level-3 :height 1.0)))
  "Face for level 3 headings in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-heading-4
  '((t (:inherit org-level-4 :height 1.0)))
  "Face for level 4 headings in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-heading-5
  '((t (:inherit org-level-5 :height 1.0)))
  "Face for level 5 headings in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-heading-6
  '((t (:inherit org-level-6 :height 1.0)))
  "Face for level 6 headings in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-heading-7
  '((t (:inherit org-level-7 :height 1.0)))
  "Face for level 7 headings in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-heading-8
  '((t (:inherit org-level-8 :height 1.0)))
  "Face for level 8 headings in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-title
  '((t (:inherit org-document-title :height 1.0)))
  "Face for title cells in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-todo
  '((t (:inherit org-todo)))
  "Face for open TODO cells in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-done
  '((t (:inherit org-done)))
  "Face for closed TODO cells in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-priority
  '((t (:inherit org-priority)))
  "Face for priority cells in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-tag
  '((t (:inherit org-tag)))
  "Face for tag cells in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-date
  '((t (:inherit org-date)))
  "Face for date cells in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-file-name
  '((t (:inherit org-document-info)))
  "Face for file-name cells in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-file-path
  '((t (:inherit shadow)))
  "Face for file-path cells in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-keyword-name
  '((t (:inherit org-special-keyword)))
  "Face for keyword-name cells in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-keyword-value
  '((t (:inherit org-document-info)))
  "Face for keyword-value cells in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-property-name
  '((t (:inherit org-special-keyword)))
  "Face for property-name cells in org-files-db completion."
  :group 'org-files-db)

(defface org-files-db-property-value
  '((t (:inherit org-property-value)))
  "Face for property-value cells in org-files-db completion."
  :group 'org-files-db)

(provide 'org-files-db-core)

;;; org-files-db-core.el ends here
