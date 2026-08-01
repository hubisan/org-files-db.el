;;; org-files-db.el --- Interface for the org-files-db command-line tool -*- lexical-binding: t; -*-

;; Copyright (C) 2026  hubisan

;; Author: hubisan
;; Maintainer: hubisan
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/hubisan/org-files-db.el
;; SPDX-License-Identifier: GPL-3.0-only

;;; Commentary:

;; This package provides the Emacs interface for the org-files-db command-line
;; application.  The initial package skeleton intentionally contains only the
;; shared customization group and executable setting.

;;; Code:

(defgroup org-files-db nil
  "Interface for the org-files-db command-line tool."
  :group 'tools)

(defcustom org-files-db-executable "orgfdb"
  "Name or path of the org-files-db executable."
  :type 'string
  :group 'org-files-db)

(provide 'org-files-db)

;;; org-files-db.el ends here
