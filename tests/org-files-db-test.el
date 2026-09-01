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

(defun org-files-db-test--single-result-presentation (result config)
  "Return a one-row presentation for RESULT and configuration CONFIG."
  (org-files-db-presentation--make-presentation
   :version 2
   :database-id "db"
   :generation 1
   :config config
   :results (vector result)
   :schemas nil
   :rows
   (vector
    (org-files-db-presentation--make-presentation-row
     :result-index 0
     :row-context nil
     :cells
     (vector
      (org-files-db-presentation--make-presentation-cell
       :search-text "Selected result"
       :display-text "Selected result"
       :role 'title))))))

(defun org-files-db-test--select-first-candidate (_prompt table &rest _args)
  "Return the first candidate from completion TABLE."
  (car (org-files-db-presentation--completion-candidates table)))

(describe "clean package foundation"
          (before-each
           (setq org-files-db-test--directory
                 (make-temp-file "org-files-db-test-" t)))

          (after-each
           (when (file-directory-p org-files-db-test--directory)
             (delete-directory org-files-db-test--directory t)))

          (it "loads only the rebuilt package foundation"
              (expect (featurep 'org-files-db) :to-equal t)
              (expect (featurep 'org-files-db-core) :to-equal t)
              (expect (featurep 'org-files-db-process) :to-equal t)
              (expect (featurep 'org-files-db-presentation) :to-equal t)
              (expect (featurep 'org-files-db-query) :to-equal t)
              (expect (featurep 'org-files-db-views) :to-equal t)
              (expect (featurep 'org-files-db-actions) :to-equal t)
              (expect (featurep 'org-files-db-search) :to-equal nil)
              (expect (featurep 'org-files-db-cache) :to-equal nil))

          (it "loads Core without specialized org-files-db modules"
              (let* ((emacs (expand-file-name invocation-name invocation-directory))
                     (library-directory
                      (file-name-directory (locate-library "org-files-db-core")))
                     (form
                      "(progn (require 'org-files-db-core) (when (or (featurep 'org-files-db-process) (featurep 'org-files-db-presentation) (featurep 'org-files-db-query) (featurep 'org-files-db-views) (featurep 'org-files-db-actions) (featurep 'org-files-db)) (kill-emacs 7)))"))
                (expect
                 (call-process emacs nil nil nil
                               "--batch" "-Q" "-L" library-directory
                               "--eval" form)
                 :to-equal 0)))

          (it "keeps public customization and faces available"
              (dolist (variable '(org-files-db-executable
                                  org-files-db-configs
                                  org-files-db-default-config
                                  org-files-db-heading-columns
                                  org-files-db-file-columns
                                  org-files-db-link-columns
                                  org-files-db-heading-sort
                                  org-files-db-file-sort
                                  org-files-db-link-sort
                                  org-files-db-heading-action
                                  org-files-db-file-action
                                  org-files-db-link-action
                                  org-files-db-views))
                (expect (not (null (custom-variable-p variable))) :to-equal t))
              (dolist (face '(org-files-db-heading-1
                              org-files-db-heading-2
                              org-files-db-heading-3
                              org-files-db-heading-4
                              org-files-db-heading-5
                              org-files-db-heading-6
                              org-files-db-heading-7
                              org-files-db-heading-8
                              org-files-db-title
                              org-files-db-todo
                              org-files-db-done
                              org-files-db-priority
                              org-files-db-tag
                              org-files-db-date
                              org-files-db-file-name
                              org-files-db-file-path
                              org-files-db-keyword-name
                              org-files-db-keyword-value
                              org-files-db-property-name
                              org-files-db-property-value))
                (expect (not (null (facep face))) :to-equal t)))

          (it "keeps current public entry points unchanged"
              (dolist (function '(org-files-db-query
                                  org-files-db-query-results
                                  org-files-db-check-setup
                                  org-files-db-current-config
                                  org-files-db-actions-open-result))
                (expect (not (null (fboundp function))) :to-equal t)))

          (it "does not define old configuration or search options"
              (expect (boundp 'org-files-db-config-file) :to-equal nil)
              (expect (boundp 'org-files-db-search-columns) :to-equal nil)
              (expect (boundp 'org-files-db-search-sort) :to-equal nil)
              (expect (boundp 'org-files-db-search-min-input) :to-equal nil))

          (it "defines presentation defaults for all query targets"
              (expect (> (length org-files-db-heading-columns) 0) :to-equal t)
              (expect (> (length org-files-db-file-columns) 0) :to-equal t)
              (expect (> (length org-files-db-link-columns) 0) :to-equal t)
              (expect (org-files-db-presentation--default-columns 'headings)
                      :to-equal org-files-db-heading-columns)
              (expect (org-files-db-presentation--default-columns 'files)
                      :to-equal org-files-db-file-columns)
              (expect (org-files-db-presentation--default-columns 'links)
                      :to-equal org-files-db-link-columns)
              (expect (org-files-db-presentation--default-sort 'headings)
                      :to-equal org-files-db-heading-sort)
              (expect (org-files-db-presentation--default-sort 'files)
                      :to-equal org-files-db-file-sort)
              (expect (org-files-db-presentation--default-sort 'links)
                      :to-equal org-files-db-link-sort))

          (it "defines separate default actions for all query targets"
              (expect (org-files-db-actions--default-action 'headings)
                      :to-equal org-files-db-heading-action)
              (expect (org-files-db-actions--default-action 'files)
                      :to-equal org-files-db-file-action)
              (expect (org-files-db-actions--default-action 'links)
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
                (expect (org-files-db-process--config-name) :to-equal "main")
                (expect (org-files-db-process--config-name "work") :to-equal "work")
                (expect (org-files-db-process--config-file)
                        :to-equal (expand-file-name main))
                (expect (org-files-db-process--config-file "work")
                        :to-equal (expand-file-name work))))

          (it "rejects duplicate configuration names"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (other (org-files-db-test--config-file "other.toml"))
                     (org-files-db-configs `(("main" . ,main) ("main" . ,other)))
                     (org-files-db-default-config "main"))
                (expect (org-files-db-process--validated-configs)
                        :to-throw 'user-error)))

          (it "rejects an unknown default configuration"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config "missing"))
                (expect (org-files-db-process--validated-configs)
                        :to-throw 'user-error)))

          (it "requires a default when configurations exist"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config nil))
                (expect (org-files-db-process--validated-configs)
                        :to-throw 'user-error)))

          (it "rejects a default name without configurations"
              (let ((org-files-db-configs nil)
                    (org-files-db-default-config "main"))
                (expect (org-files-db-process--validated-configs)
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
                (expect (org-files-db-process--config-file "unknown")
                        :to-throw 'user-error)
                (expect (org-files-db-process--config-file "missing")
                        :to-throw 'user-error)
                (expect (org-files-db-process--config-file "directory")
                        :to-throw 'user-error)))

          (it "validates view configuration names and readable files"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (work (org-files-db-test--config-file "work.toml"))
                     (org-files-db-configs `(("main" . ,main) ("work" . ,work)))
                     (org-files-db-default-config "main")
                     (org-files-db-views
                      '(("default-view" :query (headings))
                        ("work-view" :config "work" :query (files)))))
                (expect (org-files-db-views--validate-views)
                        :to-equal org-files-db-views)
                (expect (org-files-db-views--config-name (car org-files-db-views))
                        :to-equal "main")
                (expect (org-files-db-views--config-name (cadr org-files-db-views))
                        :to-equal "work")))

          (it "rejects unknown and duplicate view configuration"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config "main"))
                (let ((org-files-db-views
                       '(("bad" :config "missing" :query (headings)))))
                  (expect (org-files-db-views--validate-views)
                          :to-throw 'user-error))
                (let ((org-files-db-views
                       '(("same" :query (headings))
                         ("same" :query (files)))))
                  (expect (org-files-db-views--validate-views)
                          :to-throw 'user-error))))

          (it "rejects unsupported query targets for defaults"
              (expect (org-files-db-presentation--default-columns 'search)
                      :to-throw 'user-error)
              (expect (org-files-db-presentation--default-sort 'search)
                      :to-throw 'user-error)
              (expect (org-files-db-actions--default-action 'search)
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
                  (expect (org-files-db-process--resolve-executable)
                          :to-equal "/opt/bin/orgfdb"))))

          (it "rejects a missing executable"
              (let ((org-files-db-executable "missing-orgfdb"))
                (cl-letf (((symbol-function 'executable-find) (lambda (_name) nil)))
                  (expect (org-files-db-process--resolve-executable)
                          :to-throw 'user-error))))

          (it "passes every process argument separately and captures UTF-8 output"
              (let (command coding)
                (cl-letf (((symbol-function 'org-files-db-process--resolve-executable)
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
                         (org-files-db-process--run-process
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
              (cl-letf (((symbol-function 'org-files-db-process--run-process)
                         (lambda (&rest _args)
                           '(:status 1 :stdout "" :stderr "database is stale\n"))))
                (let (message)
                  (condition-case err
                      (org-files-db-process--call-raw '("query" "(headings)"))
                    (org-files-db-cli-error
                     (setq message (error-message-string err))))
                  (expect message :to-match "status 1")
                  (expect message :to-match "database is stale"))))

          (it "uses a separate error type for CLI usage errors"
              (cl-letf (((symbol-function 'org-files-db-process--run-process)
                         (lambda (&rest _args)
                           '(:status 2 :stdout "" :stderr "unexpected argument\n"))))
                (expect (org-files-db-process--call-raw '("query" "--bad"))
                        :to-throw 'org-files-db-cli-usage-error)))

          (it "parses requested JSON as alists and vectors"
              (cl-letf (((symbol-function 'org-files-db-process--run-process)
                         (lambda (&rest _args)
                           '(:status 0
                                     :stdout "{\"name\":\"Grüezi\",\"items\":[1,2],\"flag\":false}"
                                     :stderr ""))))
                (let ((value (org-files-db-process--call-json '("status"))))
                  (expect (alist-get 'name value) :to-equal "Grüezi")
                  (expect (vectorp (alist-get 'items value)) :to-equal t)
                  (expect (alist-get 'flag value) :to-equal :false))))

          (it "reports invalid JSON as an org-files-db error"
              (cl-letf (((symbol-function 'org-files-db-process--run-process)
                         (lambda (&rest _args)
                           '(:status 0 :stdout "not json" :stderr ""))))
                (expect (org-files-db-process--call-json '("status"))
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
                  (expect (org-files-db-process--interactive-config-name nil)
                          :to-equal "main")
                  (expect read-count :to-equal 0)
                  (expect (org-files-db-process--interactive-config-name '(4))
                          :to-equal "work")
                  (expect read-count :to-equal 1))))

          (it "builds configuration arguments from a configuration name"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (work (org-files-db-test--config-file "work.toml"))
                     (org-files-db-configs `(("main" . ,main) ("work" . ,work)))
                     (org-files-db-default-config "main"))
                (expect (org-files-db-process--config-arguments)
                        :to-equal (list "--config" (expand-file-name main)))
                (expect (org-files-db-process--config-arguments "work")
                        :to-equal (list "--config" (expand-file-name work)))))

          (it "checks a selected named configuration with the shared process helpers"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (work (org-files-db-test--config-file "work.toml"))
                     (org-files-db-configs `(("main" . ,main) ("work" . ,work)))
                     (org-files-db-default-config "main")
                     raw-arguments
                     json-arguments)
                (cl-letf (((symbol-function 'org-files-db-process--resolve-executable)
                           (lambda () "/opt/bin/orgfdb"))
                          ((symbol-function 'org-files-db-process--call-raw)
                           (lambda (arguments &optional _input)
                             (setq raw-arguments arguments)
                             "orgfdb 0.1.0\n"))
                          ((symbol-function 'org-files-db-process--call-json)
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
                (cl-letf (((symbol-function 'org-files-db-process--resolve-executable)
                           (lambda () "/opt/bin/orgfdb"))
                          ((symbol-function 'org-files-db-process--call-raw)
                           (lambda (&rest _args) "orgfdb 0.1.0\n"))
                          ((symbol-function 'org-files-db-process--call-json)
                           (lambda (&rest _args)
                             (signal 'org-files-db-cli-error
                                     '("orgfdb exited with status 1: missing database")))))
                  (let ((report (org-files-db-check-setup)))
                    (expect (alist-get 'config report) :to-equal "main")
                    (expect (alist-get 'read-check report)
                            :to-match "missing database"))))))


(describe "presentation-json version 2"
          (before-each
           (setq org-files-db-test--directory
                 (make-temp-file "org-files-db-presentation-test-" t)))

          (after-each
           (when (file-directory-p org-files-db-test--directory)
             (delete-directory org-files-db-test--directory t)))

          (it "serializes flat Emacs presentation configuration to PresentationSpec JSON"
              (let* ((json
                      (org-files-db-presentation--spec-json
                       '((outline-path
                          :width (max 80)
                          :truncate (:position right :marker "…")
                          :separator " / "
                          :include-root t
                          :include-match nil)
                         (file-name :width auto))
                       '((priority :direction desc))
                       'tags))
                     (spec (org-files-db-process--parse-json json))
                     (columns (alist-get 'columns spec))
                     (first (aref columns 0))
                     (second (aref columns 1))
                     (sort (aref (alist-get 'sort spec) 0))
                     (row-source (alist-get 'row_source spec)))
                (expect (length columns) :to-equal 2)
                (expect (alist-get 'name first) :to-equal "outline-path")
                (expect (alist-get 'mode (alist-get 'width first)) :to-equal "max")
                (expect (alist-get 'value (alist-get 'width first)) :to-equal 80)
                (expect (alist-get 'position (alist-get 'truncate first)) :to-equal "right")
                (expect (alist-get 'marker (alist-get 'truncate first)) :to-equal "…")
                (expect (alist-get 'separator (alist-get 'outline_path first)) :to-equal " / ")
                (expect (alist-get 'include_root (alist-get 'outline_path first)) :to-equal t)
                (expect (alist-get 'include_match (alist-get 'outline_path first)) :to-equal :false)
                (expect (alist-get 'name second) :to-equal "file-name")
                (expect (alist-get 'mode (alist-get 'width second)) :to-equal "auto")
                (expect (alist-get 'column sort) :to-equal "priority")
                (expect (alist-get 'direction sort) :to-equal "desc")
                (expect (alist-get 'kind row-source) :to-equal "tags")))

          (it "serializes Rust defaults without duplicating presentation semantics"
              (let* ((json (org-files-db-presentation--spec-json '((title)) nil nil))
                     (spec (org-files-db-process--parse-json json))
                     (column (aref (alist-get 'columns spec) 0)))
                (expect (alist-get 'name column) :to-equal "title")
                (expect (alist-get 'mode (alist-get 'width column)) :to-equal "auto")
                (expect (alist-get 'truncate column) :to-equal nil)
                (expect (alist-get 'outline_path column) :to-equal nil)
                (expect (alist-get 'sort spec) :to-equal [])
                (expect (alist-get 'row_source spec) :to-equal nil)))

          (it "decodes rows, cells, roles, and row context from version 2 schemas"
              (let* ((result '((kind . "heading") (level . 2) (title . "Task")))
                     (wire
                      `((presentation_version . 2)
                        (database_id . "db-1")
                        (generation . 7)
                        (results . [,result])
                        (schemas
                         . ((row_fields . ["result_index" "row_context" "cells"])
                            (cell_fields . ["search_text" "display_text" "role"])
                            (row_context_shapes
                             . ((tag . ["kind" "value"])
                                (effective-property . ["kind" "name" "value"])
                                (keyword . ["kind" "name" "value"])))
                            (display_text_null . "same-as-search_text")
                            (role_encoding . "null-or-index-into-role_values")
                            (role_values . ["heading" "title" "todo" "done" "priority" "tag"])))
                        (rows
                         . [[0 nil [["Task" nil 1]]]
                            [0 ["tag" "project"] [["project" "project " 5]]]])))
                     (presentation (org-files-db-presentation--decode wire))
                     (rows (org-files-db-presentation-rows presentation))
                     (first-row (aref rows 0))
                     (second-row (aref rows 1))
                     (first-cell (aref (org-files-db-presentation-row-cells first-row) 0))
                     (second-cell (aref (org-files-db-presentation-row-cells second-row) 0)))
                (expect (org-files-db-presentation-version presentation) :to-equal 2)
                (expect (org-files-db-presentation-database-id presentation) :to-equal "db-1")
                (expect (org-files-db-presentation-generation presentation) :to-equal 7)
                (expect (length rows) :to-equal 2)
                (expect (org-files-db-presentation-row-result-index first-row) :to-equal 0)
                (expect (org-files-db-presentation-row-row-context first-row) :to-equal nil)
                (expect (org-files-db-presentation-cell-search-text first-cell) :to-equal "Task")
                (expect (org-files-db-presentation-cell-display-text first-cell) :to-equal "Task")
                (expect (org-files-db-presentation-cell-role first-cell) :to-equal 'title)
                (expect (org-files-db-presentation-row-row-context second-row)
                        :to-equal '((kind . "tag") (value . "project")))
                (expect (org-files-db-presentation-cell-display-text second-cell)
                        :to-equal "project ")
                (expect (org-files-db-presentation-cell-role second-cell) :to-equal 'tag)
                (expect (eq (org-files-db-presentation--row-result presentation first-row)
                            result)
                        :to-equal t)
                (expect (eq (org-files-db-presentation--row-result presentation second-row)
                            result)
                        :to-equal t)))

          (it "uses emitted schema field positions instead of fixed row positions"
              (let* ((result '((kind . "file") (path . "/tmp/a.org")))
                     (wire
                      `((presentation_version . 2)
                        (database_id . "db-2")
                        (generation . 9)
                        (results . [,result])
                        (schemas
                         . ((row_fields . ["cells" "result_index" "row_context"])
                            (cell_fields . ["role" "display_text" "search_text"])
                            (row_context_shapes . ((tag . ["value" "kind"])))
                            (display_text_null . "same-as-search_text")
                            (role_encoding . "null-or-index-into-role_values")
                            (role_values . ["file-name"])))
                        (rows . [[[[0 nil "a.org"]] 0 ["project" "tag"]]])))
                     (presentation (org-files-db-presentation--decode wire))
                     (row (aref (org-files-db-presentation-rows presentation) 0))
                     (cell (aref (org-files-db-presentation-row-cells row) 0)))
                (expect (org-files-db-presentation-row-result-index row) :to-equal 0)
                (expect (org-files-db-presentation-row-row-context row)
                        :to-equal '((value . "project") (kind . "tag")))
                (expect (org-files-db-presentation-cell-search-text cell) :to-equal "a.org")
                (expect (org-files-db-presentation-cell-display-text cell) :to-equal "a.org")
                (expect (org-files-db-presentation-cell-role cell) :to-equal 'file-name)))

          (it "rejects unsupported presentation versions"
              (let (message)
                (condition-case err
                    (org-files-db-presentation--decode
                     '((presentation_version . 3)
                       (database_id . "db")
                       (generation . 1)
                       (results . [])
                       (schemas . nil)
                       (rows . [])))
                  (org-files-db-error
                   (setq message (error-message-string err))))
                (expect message :to-match "Unsupported presentation version")
                (expect message :to-match "expected 2")))

          (it "rejects invalid result and role indexes"
              (let ((base
                     '((presentation_version . 2)
                       (database_id . "db")
                       (generation . 1)
                       (results . [])
                       (schemas
                        . ((row_fields . ["result_index" "row_context" "cells"])
                           (cell_fields . ["search_text" "display_text" "role"])
                           (row_context_shapes . nil)
                           (display_text_null . "same-as-search_text")
                           (role_encoding . "null-or-index-into-role_values")
                           (role_values . ["title"])))
                       (rows . [[0 nil []]]))))
                (expect (org-files-db-presentation--decode base)
                        :to-throw 'org-files-db-error))
              (let ((wire
                     '((presentation_version . 2)
                       (database_id . "db")
                       (generation . 1)
                       (results . [((kind . "heading"))])
                       (schemas
                        . ((row_fields . ["result_index" "row_context" "cells"])
                           (cell_fields . ["search_text" "display_text" "role"])
                           (row_context_shapes . nil)
                           (display_text_null . "same-as-search_text")
                           (role_encoding . "null-or-index-into-role_values")
                           (role_values . ["title"])))
                       (rows . [[0 nil [["Task" nil 5]]]]))))
                (expect (org-files-db-presentation--decode wire)
                        :to-throw 'org-files-db-error)))

          (it "parses structural query strings without evaluation state"
              (expect (org-files-db-query--form "(headings (not (done)))")
                      :to-equal '(headings (not (done)))))

          (it "runs the data-only query path with defaults and no completion or actions"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config "main")
                     (org-files-db-heading-columns '((title :width (max 40))))
                     (org-files-db-heading-sort '((priority :direction asc)))
                     called-arguments)
                (cl-letf (((symbol-function 'org-files-db-process--call-json)
                           (lambda (arguments &optional _input)
                             (setq called-arguments arguments)
                             '((presentation_version . 2)
                               (database_id . "db")
                               (generation . 1)
                               (results . [])
                               (schemas
                                . ((row_fields . ["result_index" "row_context" "cells"])
                                   (cell_fields . ["search_text" "display_text" "role"])
                                   (row_context_shapes . nil)
                                   (display_text_null . "same-as-search_text")
                                   (role_encoding . "null-or-index-into-role_values")
                                   (role_values . [])))
                               (rows . []))))
                          ((symbol-function 'completing-read)
                           (lambda (&rest _args)
                             (error "completion must not run")))
                          ((symbol-function 'org-files-db-actions-open-result)
                           (lambda (&rest _args)
                             (error "actions must not run"))))
                  (let* ((presentation
                          (org-files-db-query-results '(headings (not (done)))))
                         (spec-index (cl-position "--presentation-spec-json"
                                                  called-arguments :test #'equal))
                         (spec-json (nth (1+ spec-index) called-arguments))
                         (spec (org-files-db-process--parse-json spec-json)))
                    (expect (org-files-db-presentation-p presentation) :to-equal t)
                    (expect called-arguments
                            :to-equal
                            (list "query"
                                  "--format" "presentation-json"
                                  "--presentation-spec-json" spec-json
                                  "--config" (expand-file-name main)
                                  "(headings (not (done)))"))
                    (expect (alist-get 'name (aref (alist-get 'columns spec) 0))
                            :to-equal "title")
                    (expect (alist-get 'column (aref (alist-get 'sort spec) 0))
                            :to-equal "priority")))))

          (it "lets data-only callers override config, sorting, and row source"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (work (org-files-db-test--config-file "work.toml"))
                     (org-files-db-configs `(("main" . ,main) ("work" . ,work)))
                     (org-files-db-default-config "main")
                     (org-files-db-heading-sort '((priority :direction asc)))
                     called-arguments)
                (cl-letf (((symbol-function 'org-files-db-process--call-json)
                           (lambda (arguments &optional _input)
                             (setq called-arguments arguments)
                             '((presentation_version . 2)
                               (database_id . "db")
                               (generation . 1)
                               (results . [])
                               (schemas
                                . ((row_fields . ["result_index" "row_context" "cells"])
                                   (cell_fields . ["search_text" "display_text" "role"])
                                   (row_context_shapes . nil)
                                   (display_text_null . "same-as-search_text")
                                   (role_encoding . "null-or-index-into-role_values")
                                   (role_values . [])))
                               (rows . [])))))
                  (org-files-db-query-results
                   "(headings)"
                   :config "work"
                   :columns '((tag :width (fixed 12)))
                   :sort nil
                   :row-source 'tags)
                  (let* ((spec-index (cl-position "--presentation-spec-json"
                                                  called-arguments :test #'equal))
                         (spec (org-files-db-process--parse-json
                                (nth (1+ spec-index) called-arguments))))
                    (expect (not (null (member (expand-file-name work) called-arguments)))
                            :to-equal t)
                    (expect (alist-get 'sort spec) :to-equal [])
                    (expect (alist-get 'kind (alist-get 'row_source spec))
                            :to-equal "tags"))))))


(describe "lightweight completion and semantic faces"
          (before-each
           (setq org-files-db-test--directory
                 (make-temp-file "org-files-db-completion-test-" t)))

          (after-each
           (when (file-directory-p org-files-db-test--directory)
             (delete-directory org-files-db-test--directory t)))

          (it "keeps full search text and shows Rust-prepared display text"
              (let* ((result '((kind . "heading") (level . 3) (title . "Long heading")))
                     (row
                      (org-files-db-presentation--make-presentation-row
                       :result-index 0
                       :row-context nil
                       :cells
                       (vector
                        (org-files-db-presentation--make-presentation-cell
                         :search-text "A complete heading value"
                         :display-text "A complete…"
                         :role 'heading)
                        (org-files-db-presentation--make-presentation-cell
                         :search-text "notes.org"
                         :display-text "notes.org  "
                         :role 'file-name))))
                     (presentation
                      (org-files-db-presentation--make-presentation
                       :version 2
                       :database-id "db"
                       :generation 1
                       :config "main"
                       :results (vector result)
                       :schemas nil
                       :rows (vector row)))
                     (candidate (car (org-files-db-presentation--candidates presentation)))
                     (searchable (substring-no-properties candidate))
                     (visible (get-text-property 0 'display candidate)))
                (expect searchable :to-match "A complete heading value")
                (expect searchable :to-match "notes\\.org")
                (expect (substring-no-properties visible)
                        :to-equal "A complete…  notes.org  ")
                (expect (get-text-property 0 'face visible)
                        :to-equal 'org-files-db-heading-3)
                (expect (get-text-property (length "A complete…  ") 'face visible)
                        :to-equal 'org-files-db-file-name)))

          (it "maps all supported semantic roles and ignores unknown roles"
              (let ((result '((kind . "heading") (level . 2))))
                (expect (org-files-db-presentation--role-face 'heading result)
                        :to-equal 'org-files-db-heading-2)
                (expect (org-files-db-presentation--role-face 'title result)
                        :to-equal 'org-files-db-title)
                (expect (org-files-db-presentation--role-face 'todo result)
                        :to-equal 'org-files-db-todo)
                (expect (org-files-db-presentation--role-face 'done result)
                        :to-equal 'org-files-db-done)
                (expect (org-files-db-presentation--role-face 'priority result)
                        :to-equal 'org-files-db-priority)
                (expect (org-files-db-presentation--role-face 'tag result)
                        :to-equal 'org-files-db-tag)
                (expect (org-files-db-presentation--role-face 'date result)
                        :to-equal 'org-files-db-date)
                (expect (org-files-db-presentation--role-face 'file-name result)
                        :to-equal 'org-files-db-file-name)
                (expect (org-files-db-presentation--role-face 'file-path result)
                        :to-equal 'org-files-db-file-path)
                (expect (org-files-db-presentation--role-face 'keyword-name result)
                        :to-equal 'org-files-db-keyword-name)
                (expect (org-files-db-presentation--role-face 'keyword-value result)
                        :to-equal 'org-files-db-keyword-value)
                (expect (org-files-db-presentation--role-face 'property-name result)
                        :to-equal 'org-files-db-property-name)
                (expect (org-files-db-presentation--role-face 'property-value result)
                        :to-equal 'org-files-db-property-value)
                (expect (org-files-db-presentation--role-face 'future-role result)
                        :to-equal nil)))

          (it "uses Org TODO keyword faces before semantic fallback faces"
              (let ((org-todo-keyword-faces
                     '(("REVIEW" . org-warning)
                       ("DONE" . "green")
                       ("CANCEL" . (:foreground "blue" :weight bold))
                       ("WAIT" . "orange")))
                    (result '((kind . "heading") (level . 2))))
                (expect
                 (org-files-db-presentation--role-face 'todo result "REVIEW")
                 :to-equal 'org-warning)
                (expect
                 (org-files-db-presentation--role-face 'done result "DONE")
                 :to-equal
                 (org-face-from-face-or-color 'todo 'org-todo "green"))
                (expect
                 (org-files-db-presentation--role-face 'done result "CANCEL")
                 :to-equal '(:foreground "blue" :weight bold))
                (expect
                 (org-files-db-presentation--role-face 'todo result "WAIT")
                 :to-equal
                 (org-face-from-face-or-color 'todo 'org-todo "orange"))
                (expect
                 (org-files-db-presentation--role-face 'todo result "NEXT")
                 :to-equal 'org-files-db-todo)
                (expect
                 (org-files-db-presentation--role-face 'done result "CLOSED")
                 :to-equal 'org-files-db-done)))

          (it "uses the Rust TODO role for fallback state"
              (let ((org-todo-keyword-faces nil)
                    (result '((kind . "heading") (level . 1))))
                (expect
                 (org-files-db-presentation--role-face 'todo result "DONE")
                 :to-equal 'org-files-db-todo)
                (expect
                 (org-files-db-presentation--role-face 'done result "TODO")
                 :to-equal 'org-files-db-done)))

          (it "uses full TODO search text when display text is formatted"
              (let* ((org-todo-keyword-faces '(("REVIEW" . org-warning)))
                     (result '((kind . "heading") (level . 1)))
                     (row
                      (org-files-db-presentation--make-presentation-row
                       :result-index 0
                       :row-context nil
                       :cells
                       (vector
                        (org-files-db-presentation--make-presentation-cell
                         :search-text "REVIEW"
                         :display-text "REVI…     "
                         :role 'todo))))
                     (visible
                      (org-files-db-presentation--visible-row row result)))
                (expect (get-text-property 0 'face visible)
                        :to-equal 'org-warning)))

          (it "defines heading faces with normal completion text height"
              (dolist (face '(org-files-db-heading-1
                              org-files-db-heading-2
                              org-files-db-heading-3
                              org-files-db-heading-4
                              org-files-db-heading-5
                              org-files-db-heading-6
                              org-files-db-heading-7
                              org-files-db-heading-8))
                (expect (not (null (facep face))) :to-equal t)
                (expect (= (face-attribute face :height nil 'default) 1.0)
                        :to-equal t)))

          (it "keeps row result context and configuration metadata on candidates"
              (let* ((result '((kind . "heading") (level . 1) (title . "Task")))
                     (context '((kind . "tag") (value . "project")))
                     (row
                      (org-files-db-presentation--make-presentation-row
                       :result-index 0
                       :row-context context
                       :cells
                       (vector
                        (org-files-db-presentation--make-presentation-cell
                         :search-text "project"
                         :display-text "project"
                         :role 'tag))))
                     (presentation
                      (org-files-db-presentation--make-presentation
                       :version 2
                       :database-id "db"
                       :generation 1
                       :config "work"
                       :results (vector result)
                       :schemas nil
                       :rows (vector row)))
                     (candidate (car (org-files-db-presentation--candidates presentation))))
                (expect (eq (get-text-property 0 'org-files-db-presentation-row candidate)
                            row)
                        :to-equal t)
                (expect (eq (get-text-property 0 'org-files-db-result candidate)
                            result)
                        :to-equal t)
                (expect (get-text-property 0 'org-files-db-row-context candidate)
                        :to-equal context)
                (expect (get-text-property 0 'org-files-db-config candidate)
                        :to-equal "work")))

          (it "resolves duplicate completion strings to the correct original result"
              (let* ((first '((kind . "heading") (level . 1) (title . "Same")))
                     (second '((kind . "heading") (level . 1) (title . "Same")))
                     (cell-1
                      (org-files-db-presentation--make-presentation-cell
                       :search-text "Same" :display-text "Same" :role 'title))
                     (cell-2
                      (org-files-db-presentation--make-presentation-cell
                       :search-text "Same" :display-text "Same" :role 'title))
                     (presentation
                      (org-files-db-presentation--make-presentation
                       :version 2
                       :database-id "db"
                       :generation 1
                       :config "main"
                       :results (vector first second)
                       :schemas nil
                       :rows
                       (vector
                        (org-files-db-presentation--make-presentation-row
                         :result-index 0 :row-context nil :cells (vector cell-1))
                        (org-files-db-presentation--make-presentation-row
                         :result-index 1 :row-context nil :cells (vector cell-2)))))
                     captured-candidates)
                (cl-letf (((symbol-function 'completing-read)
                           (lambda (_prompt collection &rest _args)
                             (setq captured-candidates
                                   (org-files-db-presentation--completion-candidates collection))
                             (substring-no-properties (cadr captured-candidates)))))
                  (expect (eq (org-files-db-presentation--read presentation "Result: ")
                              second)
                          :to-equal t))
                (expect (length captured-candidates) :to-equal 2)
                (expect (equal (car captured-candidates) (cadr captured-candidates))
                        :to-equal nil)
                (expect (substring-no-properties
                         (get-text-property 0 'display (car captured-candidates)))
                        :to-equal "Same")
                (expect (substring-no-properties
                         (get-text-property 0 'display (cadr captured-candidates)))
                        :to-equal "Same")))

          (it "preserves Rust row order in completion metadata"
              (let* ((candidates '("b" "a"))
                     (table (org-files-db-presentation--completion-table candidates))
                     (metadata (funcall table "" nil 'metadata)))
                (expect (cdr (assq 'display-sort-function (cdr metadata)))
                        :to-equal #'identity)
                (expect (cdr (assq 'cycle-sort-function (cdr metadata)))
                        :to-equal #'identity)
                (expect (org-files-db-presentation--completion-candidates table)
                        :to-equal candidates)))

          (it "does not recalculate Rust presentation data while building candidates"
              (let* ((result '((kind . "file") (title . "File")))
                     (row
                      (org-files-db-presentation--make-presentation-row
                       :result-index 0
                       :row-context nil
                       :cells
                       (vector
                        (org-files-db-presentation--make-presentation-cell
                         :search-text "File"
                         :display-text "File   "
                         :role 'title))))
                     (presentation
                      (org-files-db-presentation--make-presentation
                       :version 2
                       :database-id "db"
                       :generation 1
                       :config "main"
                       :results (vector result)
                       :schemas nil
                       :rows (vector row))))
                (cl-letf (((symbol-function 'string-width)
                           (lambda (&rest _args) (error "width calculation must not run")))
                          ((symbol-function 'truncate-string-to-width)
                           (lambda (&rest _args) (error "truncation must not run"))))
                  (expect (length (org-files-db-presentation--candidates presentation))
                          :to-equal 1))))

          (it "stores the effective configuration on one-shot presentation results"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (work (org-files-db-test--config-file "work.toml"))
                     (org-files-db-configs `(("main" . ,main) ("work" . ,work)))
                     (org-files-db-default-config "main"))
                (cl-letf (((symbol-function 'org-files-db-process--call-json)
                           (lambda (&rest _args)
                             '((presentation_version . 2)
                               (database_id . "db")
                               (generation . 1)
                               (results . [])
                               (schemas
                                . ((row_fields . ["result_index" "row_context" "cells"])
                                   (cell_fields . ["search_text" "display_text" "role"])
                                   (row_context_shapes . nil)
                                   (display_text_null . "same-as-search_text")
                                   (role_encoding . "null-or-index-into-role_values")
                                   (role_values . [])))
                               (rows . [])))))
                  (expect
                   (org-files-db-presentation-config
                    (org-files-db-query-results '(files) :config "work"))
                   :to-equal "work")))))


(describe "public structural query command"
          (it "uses the target-specific default action for headings files and links"
              (dolist (case '(((headings) . headings)
                              ((files) . files)
                              ((links) . links)))
                (let* ((query (car case))
                       (target (cdr case))
                       (result `((kind . ,(symbol-name target))))
                       (presentation
                        (org-files-db-test--single-result-presentation result "main"))
                       (called nil)
                       (org-files-db-heading-action
                        (lambda (value) (setq called (list 'headings value))))
                       (org-files-db-file-action
                        (lambda (value) (setq called (list 'files value))))
                       (org-files-db-link-action
                        (lambda (value) (setq called (list 'links value)))))
                  (cl-letf (((symbol-function 'org-files-db-query-results)
                             (lambda (&rest _args) presentation))
                            ((symbol-function 'completing-read)
                             #'org-files-db-test--select-first-candidate))
                    (expect (org-files-db-query query) :to-equal result)
                    (expect (car called) :to-equal target)
                    (expect (cadr called) :to-be result)))))

          (it "lets a per-call action override the target default"
              (let* ((result '((kind . "heading") (title . "Task")))
                     (presentation
                      (org-files-db-test--single-result-presentation result "main"))
                     (default-called nil)
                     (override-called nil)
                     (org-files-db-heading-action
                      (lambda (_value) (setq default-called t))))
                (cl-letf (((symbol-function 'org-files-db-query-results)
                           (lambda (&rest _args) presentation))
                          ((symbol-function 'completing-read)
                           #'org-files-db-test--select-first-candidate))
                  (expect
                   (org-files-db-query
                    '(headings)
                    :action (lambda (value) (setq override-called value)))
                   :to-equal result)
                  (expect default-called :to-equal nil)
                  (expect override-called :to-be result))))

          (it "passes per-call presentation and configuration overrides to the data path"
              (let* ((result '((kind . "heading")))
                     (presentation
                      (org-files-db-test--single-result-presentation result "work"))
                     (called-arguments nil)
                     (org-files-db-heading-action #'ignore))
                (cl-letf (((symbol-function 'org-files-db-query-results)
                           (lambda (query &rest arguments)
                             (setq called-arguments (cons query arguments))
                             presentation))
                          ((symbol-function 'completing-read)
                           #'org-files-db-test--select-first-candidate))
                  (org-files-db-query
                   '(headings (not (done)))
                   :config "work"
                   :columns '((title :width (max 40)))
                   :sort '((priority :direction asc))
                   :row-source 'tags)
                  (expect (car called-arguments)
                          :to-equal '(headings (not (done))))
                  (expect (plist-get (cdr called-arguments) :config)
                          :to-equal "work")
                  (expect (plist-get (cdr called-arguments) :columns)
                          :to-equal '((title :width (max 40))))
                  (expect (not (null (plist-member (cdr called-arguments) :sort)))
                          :to-equal t)
                  (expect (plist-get (cdr called-arguments) :sort)
                          :to-equal '((priority :direction asc)))
                  (expect (plist-get (cdr called-arguments) :row-source)
                          :to-equal 'tags))))

          (it "provides the effective configuration only while the action runs"
              (let* ((result '((kind . "file") (path . "/tmp/example.org")))
                     (presentation
                      (org-files-db-test--single-result-presentation result "work"))
                     (seen-config nil)
                     (org-files-db-file-action
                      (lambda (_value)
                        (setq seen-config (org-files-db-current-config)))))
                (expect (org-files-db-current-config) :to-equal nil)
                (cl-letf (((symbol-function 'org-files-db-query-results)
                           (lambda (&rest _args) presentation))
                          ((symbol-function 'completing-read)
                           #'org-files-db-test--select-first-candidate))
                  (expect (org-files-db-query '(files)) :to-equal result))
                (expect seen-config :to-equal "work")
                (expect (org-files-db-current-config) :to-equal nil)))

          (it "uses the default configuration for an interactive query without a prefix"
              (let* ((result '((kind . "heading")))
                     (presentation
                      (org-files-db-test--single-result-presentation result "main"))
                     (seen-prefix 'unset)
                     (seen-config nil)
                     (org-files-db-heading-action #'ignore))
                (cl-letf (((symbol-function 'org-files-db-query--read-query)
                           (lambda () '(headings)))
                          ((symbol-function 'org-files-db-process--interactive-config-name)
                           (lambda (&optional prefix)
                             (setq seen-prefix prefix)
                             "main"))
                          ((symbol-function 'org-files-db-query-results)
                           (lambda (_query &rest arguments)
                             (setq seen-config (plist-get arguments :config))
                             presentation))
                          ((symbol-function 'completing-read)
                           #'org-files-db-test--select-first-candidate))
                  (let ((current-prefix-arg nil))
                    (call-interactively #'org-files-db-query)))
                (expect seen-prefix :to-equal nil)
                (expect seen-config :to-equal "main")))

          (it "lets an interactive prefix select another configuration"
              (let* ((result '((kind . "heading")))
                     (presentation
                      (org-files-db-test--single-result-presentation result "work"))
                     (seen-prefix nil)
                     (seen-config nil)
                     (org-files-db-heading-action #'ignore))
                (cl-letf (((symbol-function 'org-files-db-query--read-query)
                           (lambda () '(headings)))
                          ((symbol-function 'org-files-db-process--interactive-config-name)
                           (lambda (&optional prefix)
                             (setq seen-prefix prefix)
                             "work"))
                          ((symbol-function 'org-files-db-query-results)
                           (lambda (_query &rest arguments)
                             (setq seen-config (plist-get arguments :config))
                             presentation))
                          ((symbol-function 'completing-read)
                           #'org-files-db-test--select-first-candidate))
                  (let ((current-prefix-arg '(4)))
                    (call-interactively #'org-files-db-query)))
                (expect seen-prefix :to-equal '(4))
                (expect seen-config :to-equal "work")))

          (it "does not print the selected result from the query command"
              (let* ((result '((kind . "heading") (title . "Private data")))
                     (presentation
                      (org-files-db-test--single-result-presentation result "main"))
                     (messages nil)
                     (org-files-db-heading-action #'ignore))
                (cl-letf (((symbol-function 'org-files-db-query-results)
                           (lambda (&rest _args) presentation))
                          ((symbol-function 'completing-read)
                           #'org-files-db-test--select-first-candidate)
                          ((symbol-function 'message)
                           (lambda (&rest args) (push args messages))))
                  (expect (org-files-db-query '(headings)) :to-equal result))
                (expect messages :to-equal nil)))

          (it "rejects a non-callable action before execution"
              (cl-letf (((symbol-function 'org-files-db-query-results)
                         (lambda (&rest _args)
                           (error "Query must not run for an invalid action")))
                        ((symbol-function 'completing-read)
                         (lambda (&rest _args)
                           (error "Completion must not run for an invalid action"))))
                (expect (org-files-db-query '(headings) :action 'not-a-function)
                        :to-throw 'user-error))))

(provide 'org-files-db-test)

;;; org-files-db-test.el ends here
