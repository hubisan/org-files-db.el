;;; org-files-db-actions.el --- Result action foundation -*- lexical-binding: t; -*-

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

;; Default action configuration and dynamic action context for the rebuilt
;; client.  Result navigation is added in a later rebuild step.

;;; Code:

(require 'org-files-db-process)

(defvar org-files-db--current-action-config nil
  "Effective configuration name while an org-files-db action runs.")

(defun org-files-db-current-config ()
  "Return the effective configuration name for the current result action.
Return nil outside an org-files-db result action."
  org-files-db--current-action-config)

(defun org-files-db-actions-open-result (result)
  "Open RESULT at its stored source location.
The rebuilt navigation implementation is added in a later rebuild step."
  (ignore result)
  (user-error "Org-files-db result navigation is not available yet"))

(defcustom org-files-db-heading-action #'org-files-db-actions-open-result
  "Default action for heading query results."
  :type 'function
  :group 'org-files-db)

(defcustom org-files-db-file-action #'org-files-db-actions-open-result
  "Default action for file query results."
  :type 'function
  :group 'org-files-db)

(defcustom org-files-db-link-action #'org-files-db-actions-open-result
  "Default action for link query results."
  :type 'function
  :group 'org-files-db)

(defun org-files-db--default-action (target)
  "Return the configured default action for TARGET."
  (pcase target
    ('headings org-files-db-heading-action)
    ('files org-files-db-file-action)
    ('links org-files-db-link-action)
    (_ (user-error "Unsupported org-files-db query target: %S" target))))

(provide 'org-files-db-actions)

;;; org-files-db-actions.el ends here
