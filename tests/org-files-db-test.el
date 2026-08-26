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
(require 'cl-lib)
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


(describe "shared orgfdb process layer"
  (before-each
    (setq org-files-db-test--directory
          (make-temp-file "org-files-db-process-test-" t)))

  (after-each
    (when (file-directory-p org-files-db-test--directory)
      (delete-directory org-files-db-test--directory t)))

  (it "resolves the configured executable without a shell"
    (let ((org-files-db-executable "orgfdb"))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (name)
                   (and (equal name "orgfdb") "/opt/bin/orgfdb")))
                ((symbol-function 'file-executable-p)
                 (lambda (file) (equal file "/opt/bin/orgfdb"))))
        (expect (org-files-db--resolve-executable)
                :to-equal "/opt/bin/orgfdb"))))

  (it "rejects a missing executable"
    (let ((org-files-db-executable "missing-orgfdb"))
      (cl-letf (((symbol-function 'executable-find) (lambda (_name) nil)))
        (expect (org-files-db--resolve-executable)
                :to-throw 'user-error))))

  (it "passes every process argument separately and captures UTF-8 output"
    (let (command coding)
      (cl-letf (((symbol-function 'org-files-db--resolve-executable)
                 (lambda () "/opt/bin/orgfdb"))
                ((symbol-function 'make-process)
                 (lambda (&rest properties)
                   (setq command (plist-get properties :command)
                         coding (plist-get properties :coding))
                   (with-current-buffer (plist-get properties :buffer)
                     (insert "Grüezi\n"))
                   (with-current-buffer (plist-get properties :stderr)
                     (insert "Fehler ä\n"))
                   'org-files-db-test-process))
                ((symbol-function 'process-live-p) (lambda (_process) nil))
                ((symbol-function 'process-exit-status) (lambda (_process) 0)))
        (let ((result
               (org-files-db--run-process
                '("query" "value with spaces" "$(not-a-shell-command)"))))
          (expect command
                  :to-equal
                  '("/opt/bin/orgfdb"
                    "query"
                    "value with spaces"
                    "$(not-a-shell-command)"))
          (expect coding :to-equal '(utf-8-unix . utf-8-unix))
          (expect (plist-get result :stdout) :to-equal "Grüezi\n")
          (expect (plist-get result :stderr) :to-equal "Fehler ä\n")))))

  (it "reports runtime stderr in CLI errors"
    (cl-letf (((symbol-function 'org-files-db--run-process)
               (lambda (&rest _args)
                 '(:status 1 :stdout "" :stderr "database is stale\n"))))
      (let (message)
        (condition-case err
            (org-files-db--call-raw '("query" "(headings)"))
          (org-files-db-cli-error
           (setq message (error-message-string err))))
        (expect message :to-match "status 1")
        (expect message :to-match "database is stale"))))

  (it "uses a separate error type for CLI usage errors"
    (cl-letf (((symbol-function 'org-files-db--run-process)
               (lambda (&rest _args)
                 '(:status 2 :stdout "" :stderr "unexpected argument\n"))))
      (expect (org-files-db--call-raw '("query" "--bad"))
              :to-throw 'org-files-db-cli-usage-error)))

  (it "parses requested JSON as alists and vectors"
    (cl-letf (((symbol-function 'org-files-db--run-process)
               (lambda (&rest _args)
                 '(:status 0
                   :stdout "{\"name\":\"Grüezi\",\"items\":[1,2],\"flag\":false}"
                   :stderr ""))))
      (let ((value (org-files-db--call-json '("status"))))
        (expect (alist-get 'name value) :to-equal "Grüezi")
        (expect (vectorp (alist-get 'items value)) :to-equal t)
        (expect (alist-get 'flag value) :to-equal :false))))

  (it "reports invalid JSON as an org-files-db error"
    (cl-letf (((symbol-function 'org-files-db--run-process)
               (lambda (&rest _args)
                 '(:status 0 :stdout "not json" :stderr ""))))
      (expect (org-files-db--call-json '("status"))
              :to-throw 'org-files-db-error)))

  (it "uses the default configuration unless a prefix selects another one"
    (let* ((main (org-files-db-test--config-file "main.toml"))
           (work (org-files-db-test--config-file "work.toml"))
           (org-files-db-configs `(("main" . ,main) ("work" . ,work)))
           (org-files-db-default-config "main")
           (read-count 0))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _args)
                   (setq read-count (1+ read-count))
                   "work")))
        (expect (org-files-db--interactive-config-name nil)
                :to-equal "main")
        (expect read-count :to-equal 0)
        (expect (org-files-db--interactive-config-name '(4))
                :to-equal "work")
        (expect read-count :to-equal 1))))

  (it "builds configuration arguments from a configuration name"
    (let* ((main (org-files-db-test--config-file "main.toml"))
           (work (org-files-db-test--config-file "work.toml"))
           (org-files-db-configs `(("main" . ,main) ("work" . ,work)))
           (org-files-db-default-config "main"))
      (expect (org-files-db--config-arguments)
              :to-equal (list "--config" (expand-file-name main)))
      (expect (org-files-db--config-arguments "work")
              :to-equal (list "--config" (expand-file-name work)))))

  (it "checks a selected named configuration with the shared process helpers"
    (let* ((main (org-files-db-test--config-file "main.toml"))
           (work (org-files-db-test--config-file "work.toml"))
           (org-files-db-configs `(("main" . ,main) ("work" . ,work)))
           (org-files-db-default-config "main")
           raw-arguments
           json-arguments)
      (cl-letf (((symbol-function 'org-files-db--resolve-executable)
                 (lambda () "/opt/bin/orgfdb"))
                ((symbol-function 'org-files-db--call-raw)
                 (lambda (arguments &optional _input)
                   (setq raw-arguments arguments)
                   "orgfdb 0.1.0\n"))
                ((symbol-function 'org-files-db--call-json)
                 (lambda (arguments &optional _input)
                   (setq json-arguments arguments)
                   '((generation . 3)))))
        (let ((report (org-files-db-check-setup "work")))
          (expect raw-arguments :to-equal '("--version"))
          (expect json-arguments
                  :to-equal
                  (list "status" "--format" "json"
                        "--config" (expand-file-name work)))
          (expect (alist-get 'executable report) :to-equal "/opt/bin/orgfdb")
          (expect (alist-get 'config report) :to-equal "work")
          (expect (alist-get 'config-file report)
                  :to-equal (expand-file-name work))
          (expect (alist-get 'version report) :to-equal "orgfdb 0.1.0")
          (expect (alist-get 'read-check report) :to-equal "ok")))))

  (it "keeps setup diagnostics when the read-only check fails"
    (let* ((main (org-files-db-test--config-file "main.toml"))
           (org-files-db-configs `(("main" . ,main)))
           (org-files-db-default-config "main"))
      (cl-letf (((symbol-function 'org-files-db--resolve-executable)
                 (lambda () "/opt/bin/orgfdb"))
                ((symbol-function 'org-files-db--call-raw)
                 (lambda (&rest _args) "orgfdb 0.1.0\n"))
                ((symbol-function 'org-files-db--call-json)
                 (lambda (&rest _args)
                   (signal 'org-files-db-cli-error
                           '("orgfdb exited with status 1: missing database")))))
        (let ((report (org-files-db-check-setup)))
          (expect (alist-get 'config report) :to-equal "main")
          (expect (alist-get 'read-check report)
                  :to-match "missing database"))))))

(provide 'org-files-db-test)

;;; org-files-db-test.el ends here
