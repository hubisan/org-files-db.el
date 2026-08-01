;;; org-files-db-test.el --- Tests for org-files-db -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-only

;;; Code:

(require 'buttercup)
(require 'org-files-db)

(describe "org-files-db"
  (it "provides its feature"
    (expect (featurep 'org-files-db) :to-equal t))

  (it "uses orgfdb as the default executable"
    (expect org-files-db-executable :to-equal "orgfdb")))

(provide 'org-files-db-test)

;;; org-files-db-test.el ends here
