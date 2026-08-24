;;; org-files-db-test.el --- Tests for org-files-db -*- lexical-binding: t; -*-

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

;;; Code:

(require 'buttercup)
(require 'org-files-db)

(defvar org-files-db-test--directory nil)

(defun org-files-db-test--config-file (name)
  "Create and return a readable configuration file named NAME."
  (let ((file (expand-file-name name org-files-db-test--directory)))
    (with-temp-file file
      (insert "[database]\n"))
    file))

(describe "clean package foundation"
  (before-each
    (setq org-files-db-test--directory
          (make-temp-file "org-files-db-test-" t)))

  (after-each
    (when (file-directory-p org-files-db-test--directory)
      (delete-directory org-files-db-test--directory t)))

  (it "loads only the rebuilt package foundation"
    (expect (featurep 'org-files-db) :to-equal t)
    (expect (featurep 'org-files-db-process) :to-equal t)
    (expect (featurep 'org-files-db-presentation) :to-equal t)
    (expect (featurep 'org-files-db-query) :to-equal t)
    (expect (featurep 'org-files-db-views) :to-equal t)
    (expect (featurep 'org-files-db-actions) :to-equal t)
    (expect (featurep 'org-files-db-search) :to-equal nil)
    (expect (featurep 'org-files-db-cache) :to-equal nil))

  (it "does not define old configuration or search options"
    (expect (boundp 'org-files-db-config-file) :to-equal nil)
    (expect (boundp 'org-files-db-search-columns) :to-equal nil)
    (expect (boundp 'org-files-db-search-sort) :to-equal nil)
    (expect (boundp 'org-files-db-search-min-input) :to-equal nil))

  (it "defines presentation defaults for all query targets"
    (expect (> (length org-files-db-heading-columns) 0) :to-equal t)
    (expect (> (length org-files-db-file-columns) 0) :to-equal t)
    (expect (> (length org-files-db-link-columns) 0) :to-equal t)
    (expect (org-files-db--default-columns 'headings)
            :to-equal org-files-db-heading-columns)
    (expect (org-files-db--default-columns 'files)
            :to-equal org-files-db-file-columns)
    (expect (org-files-db--default-columns 'links)
            :to-equal org-files-db-link-columns)
    (expect (org-files-db--default-sort 'headings)
            :to-equal org-files-db-heading-sort)
    (expect (org-files-db--default-sort 'files)
            :to-equal org-files-db-file-sort)
    (expect (org-files-db--default-sort 'links)
            :to-equal org-files-db-link-sort))

  (it "defines separate default actions for all query targets"
    (expect (org-files-db--default-action 'headings)
            :to-equal org-files-db-heading-action)
    (expect (org-files-db--default-action 'files)
            :to-equal org-files-db-file-action)
    (expect (org-files-db--default-action 'links)
            :to-equal org-files-db-link-action)
    (expect org-files-db-heading-action
            :to-equal #'org-files-db-actions-open-result)
    (expect org-files-db-file-action
            :to-equal #'org-files-db-actions-open-result)
    (expect org-files-db-link-action
            :to-equal #'org-files-db-actions-open-result))

  (it "resolves named and default configurations"
    (let* ((main (org-files-db-test--config-file "main.toml"))
           (work (org-files-db-test--config-file "work.toml"))
           (org-files-db-configs `(("main" . ,main) ("work" . ,work)))
           (org-files-db-default-config "main"))
      (expect (org-files-db--config-name) :to-equal "main")
      (expect (org-files-db--config-name "work") :to-equal "work")
      (expect (org-files-db--config-file)
              :to-equal (expand-file-name main))
      (expect (org-files-db--config-file "work")
              :to-equal (expand-file-name work))))

  (it "rejects duplicate configuration names"
    (let* ((main (org-files-db-test--config-file "main.toml"))
           (other (org-files-db-test--config-file "other.toml"))
           (org-files-db-configs `(("main" . ,main) ("main" . ,other)))
           (org-files-db-default-config "main"))
      (expect (org-files-db--validated-configs)
              :to-throw 'user-error)))

  (it "rejects an unknown default configuration"
    (let* ((main (org-files-db-test--config-file "main.toml"))
           (org-files-db-configs `(("main" . ,main)))
           (org-files-db-default-config "missing"))
      (expect (org-files-db--validated-configs)
              :to-throw 'user-error)))

  (it "requires a default when configurations exist"
    (let* ((main (org-files-db-test--config-file "main.toml"))
           (org-files-db-configs `(("main" . ,main)))
           (org-files-db-default-config nil))
      (expect (org-files-db--validated-configs)
              :to-throw 'user-error)))

  (it "rejects a default name without configurations"
    (let ((org-files-db-configs nil)
          (org-files-db-default-config "main"))
      (expect (org-files-db--validated-configs)
              :to-throw 'user-error)))

  (it "rejects unknown and unreadable configuration files"
    (let* ((main (org-files-db-test--config-file "main.toml"))
           (missing (expand-file-name "missing.toml"
                                      org-files-db-test--directory))
           (directory (expand-file-name "config-dir"
                                        org-files-db-test--directory))
           (org-files-db-configs
            `(("main" . ,main)
              ("missing" . ,missing)
              ("directory" . ,directory)))
           (org-files-db-default-config "main"))
      (make-directory directory)
      (expect (org-files-db--config-file "unknown")
              :to-throw 'user-error)
      (expect (org-files-db--config-file "missing")
              :to-throw 'user-error)
      (expect (org-files-db--config-file "directory")
              :to-throw 'user-error)))

  (it "validates view configuration names and readable files"
    (let* ((main (org-files-db-test--config-file "main.toml"))
           (work (org-files-db-test--config-file "work.toml"))
           (org-files-db-configs `(("main" . ,main) ("work" . ,work)))
           (org-files-db-default-config "main")
           (org-files-db-views
            '(("default-view" :query (headings))
              ("work-view" :config "work" :query (files)))))
      (expect (org-files-db--validate-views)
              :to-equal org-files-db-views)
      (expect (org-files-db--view-config-name (car org-files-db-views))
              :to-equal "main")
      (expect (org-files-db--view-config-name (cadr org-files-db-views))
              :to-equal "work")))

  (it "rejects unknown and duplicate view configuration"
    (let* ((main (org-files-db-test--config-file "main.toml"))
           (org-files-db-configs `(("main" . ,main)))
           (org-files-db-default-config "main"))
      (let ((org-files-db-views
             '(("bad" :config "missing" :query (headings)))))
        (expect (org-files-db--validate-views)
                :to-throw 'user-error))
      (let ((org-files-db-views
             '(("same" :query (headings))
               ("same" :query (files)))))
        (expect (org-files-db--validate-views)
                :to-throw 'user-error))))

  (it "rejects unsupported query targets for defaults"
    (expect (org-files-db--default-columns 'search)
            :to-throw 'user-error)
    (expect (org-files-db--default-sort 'search)
            :to-throw 'user-error)
    (expect (org-files-db--default-action 'search)
            :to-throw 'user-error)))

(provide 'org-files-db-test)

;;; org-files-db-test.el ends here
