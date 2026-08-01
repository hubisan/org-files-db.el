;;; org-files-db.el --- Emacs interface for org-files-db -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Hubmann

;; Author: Daniel Hubmann <hubisan@gmail.com>
;; Maintainer: Daniel Hubmann <hubisan@gmail.com>
;; URL: https://github.com/hubisan/org-files-db.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4"))
;; Keywords: outlines, tools, convenience

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

;; org-files-db.el is a thin Emacs interface to the orgfdb command-line tool.
;; It provides Query Model v0 completion, FTS5 search, named views, result
;; actions, Org dynamic blocks, and optional Embark and Consult integration.
;; Indexing and SQLite access remain in orgfdb.

;;; Code:

(require 'org-files-db-core)
(require 'org-files-db-query)
(require 'org-files-db-search)
(require 'org-files-db-export)
(require 'org-files-db-actions)
(require 'org-files-db-views)
(require 'org-files-db-dblock)

(provide 'org-files-db)

;;; org-files-db.el ends here
