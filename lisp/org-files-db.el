;;; org-files-db.el --- Emacs interface for orgfdb -*- lexical-binding: t; -*-

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

;; Author: Daniel Hubmann <hubisan@gmail.com>
;; Maintainer: Daniel Hubmann <hubisan@gmail.com>
;; URL: https://github.com/hubisan/org-files-db.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4"))
;; Keywords: outlines, tools, convenience

;;; Commentary:

;; org-files-db.el is an Emacs client for the orgfdb command-line tool.
;; orgfdb owns database access and result presentation.  Emacs owns user
;; configuration, completion, navigation, and user actions.

;;; Code:

(require 'org-files-db-core)
(require 'org-files-db-process)
(require 'org-files-db-presentation)
(require 'org-files-db-actions)
(require 'org-files-db-query)
(require 'org-files-db-views)

(provide 'org-files-db)

;;; org-files-db.el ends here
