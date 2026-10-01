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
   :version 3
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

(defconst org-files-db-test--fixture-directory
  (expand-file-name
   "fixtures"
   (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Directory with recorded orgfdb payloads.")

(defun org-files-db-test--fixture (name)
  "Return the recorded presentation-json fixture NAME parsed like process output."
  (org-files-db-process--parse-json
   (with-temp-buffer
     (insert-file-contents
      (expand-file-name (format "presentation-v3-%s.json" name)
                        org-files-db-test--fixture-directory))
     (buffer-string))))

(defun org-files-db-test--fixture-text (file-name)
  "Return the raw text of the recorded fixture FILE-NAME."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name file-name org-files-db-test--fixture-directory))
    (buffer-string)))

(defun org-files-db-test--schemas (&optional role-values)
  "Return a version 3 schemas alist with ROLE-VALUES."
  `((result_kinds . ["root" "heading" "file" "link"])
    (result_shapes
     . ((root . ["kind" "id" "file" "line" "byte_start"])
        (heading . ["kind" "id" "file" "line" "byte_start"])
        (file . ["kind" "id" "file" "line" "byte_start"])
        (link . ["kind" "id" "file" "line" "byte_start"
                 "target_file" "target_line" "target_byte_start"])))
    (result_file_encoding . "index-into-files")
    (row_fields . ["result_index" "row_context" "cells"])
    (cell_fields . ["search_text" "display_text" "role"])
    (row_context_shapes
     . ((tag . ["kind" "value"])
        (effective-property . ["kind" "name" "value"])
        (keyword . ["kind" "name" "value"])))
    (display_text_null . "same-as-search_text")
    (role_encoding . "null-or-index-into-role_values")
    (role_values . ,(or role-values []))))

(defun org-files-db-test--wire (&rest overrides)
  "Return a minimal valid version 3 wire alist with OVERRIDES applied.
OVERRIDES is a plist keyed by keywords such as `:files' or `:results'."
  (let ((wire (copy-tree `((presentation_version . 3)
                           (database_id . "db")
                           (generation . 1)
                           (files . ["/notes/a.org"])
                           (results . [])
                           (schemas . ,(org-files-db-test--schemas))
                           (rows . [])) t)))
    (while overrides
      (setf (alist-get (intern (substring (symbol-name (pop overrides)) 1)) wire)
            (pop overrides)))
    wire))

(defun org-files-db-test--empty-wire ()
  "Return a valid empty version 3 wire alist."
  (org-files-db-test--wire :files []))

(defun org-files-db-test--empty-presentation-json ()
  "Return a valid empty presentation-json version 3 payload."
  (json-serialize
   (org-files-db-test--json-object (org-files-db-test--empty-wire))
   :null-object nil))

(defun org-files-db-test--json-object (value)
  "Convert parsed-JSON style alist VALUE into a hash table for serialization."
  (cond
   ((and (consp value) (consp (car value)) (symbolp (caar value)))
    (let ((object (make-hash-table :test #'equal)))
      (dolist (entry value object)
        (puthash (symbol-name (car entry))
                 (org-files-db-test--json-object (cdr entry))
                 object))))
   (t value)))

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
              (expect (featurep 'org-files-db-watch) :to-equal t)
              (expect (featurep 'org-files-db-search) :to-equal nil)
              (expect (featurep 'org-files-db-cache) :to-equal t))

          (it "loads Core without specialized org-files-db modules"
              (let* ((emacs (expand-file-name invocation-name invocation-directory))
                     (library-directory
                      (file-name-directory (locate-library "org-files-db-core")))
                     (form
                      "(progn (require 'org-files-db-core) (when (or (featurep 'org-files-db-process) (featurep 'org-files-db-presentation) (featurep 'org-files-db-query) (featurep 'org-files-db-views) (featurep 'org-files-db-actions) (featurep 'org-files-db-cache) (featurep 'org-files-db)) (kill-emacs 7)))"))
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
                                  org-files-db-views
                                  org-files-db-watch-startup-timeout))
                (expect (not (null (custom-variable-p variable))) :to-equal t))
              (dolist (face '(org-files-db-heading
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
                                  org-files-db-view
                                  org-files-db-check-setup
                                  org-files-db-current-config
                                  org-files-db-actions-open-result
                                  org-files-db-watch-mode
                                  org-files-db-watch-start
                                  org-files-db-watch-stop
                                  org-files-db-cache-mode
                                  org-files-db-cache-start
                                  org-files-db-cache-stop))
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

          (it "requests structured JSON errors for every call"
              (let (called)
                (cl-letf (((symbol-function 'org-files-db-process--run-process)
                           (lambda (arguments &optional _input)
                             (setq called arguments)
                             '(:status 0 :stdout "{}" :stderr ""))))
                  (org-files-db-process--call-json '("status" "--format" "json"))
                  (expect called
                          :to-equal
                          '("--error-format" "json" "status" "--format" "json")))))

          (it "decodes structured stderr into error data"
              (dolist (case '(("{\"error\":{\"kind\":\"usage\",\"message\":\"bad option\"}}" 2
                               org-files-db-cli-usage-error "bad option" "usage")
                              ("{\"error\":{\"kind\":\"io\",\"message\":\"cannot read\",\"path\":\"/x\"}}" 1
                               org-files-db-cli-error "cannot read (/x)" "io")
                              ("{\"error\":{\"kind\":\"stale-index\",\"message\":\"stale\"}}" 1
                               org-files-db-stale-index "stale" "stale-index")
                              ("{not json\n" 1 org-files-db-cli-error "{not json" nil)
                              ("{\"other\":1}" 1 org-files-db-cli-error "{\"other\":1}" nil)))
                (pcase-let ((`(,stderr ,status ,symbol ,text ,kind) case))
                  (cl-letf (((symbol-function 'org-files-db-process--run-process)
                             (lambda (&rest _args)
                               (list :status status :stdout "" :stderr stderr))))
                    (condition-case err
                        (org-files-db-process--call-raw '("query"))
                      (error
                       (expect (car err) :to-be symbol)
                       (expect (cadr err) :to-match (regexp-quote text))
                       (expect (cadr err) :to-match (format "status %d" status))
                       (expect (nth 2 err) :to-equal status)
                       (expect (nth 3 err) :to-equal (string-trim stderr))
                       (expect (nth 4 err) :to-equal kind)))))))

          (it "signals a stale index from the recorded CLI error"
              (cl-letf (((symbol-function 'org-files-db-process--run-process)
                         (lambda (&rest _args)
                           (list :status 1 :stdout ""
                                 :stderr (org-files-db-test--fixture-text
                                          "error-stale-index.stderr")))))
                (expect (org-files-db-process--call-raw '("query"))
                        :to-throw 'org-files-db-stale-index)
                (expect (get 'org-files-db-stale-index 'error-conditions)
                        :to-contain 'org-files-db-cli-error)))

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


(describe "reloading results by id"
          (let (called)
            (before-each
             (setq called nil)
             (setq org-files-db-test--directory
                   (make-temp-file "org-files-db-reload-test-" t)))
            (after-each
             (when (file-directory-p org-files-db-test--directory)
               (delete-directory org-files-db-test--directory t)))

            (cl-flet ((record (kind id)
                        (org-files-db-presentation--make-record
                         :kind kind :id id :file "/f.org" :line 1))
                      (presentation ()
                        (org-files-db-presentation--make-presentation
                         :database-id "db-1" :generation 4)))
              (it "builds the id query for each record kind"
                  (let* ((config (org-files-db-test--config-file "main.toml"))
                         (org-files-db-configs `(("main" . ,config)))
                         (org-files-db-default-config "main"))
                    (dolist (case '((((heading . 2) (heading . 3)) "headings" "2 3")
                                    (((link . 5)) "links" "5")
                                    (((file . 1)) "files" "1")
                                    (((root . 1)) "files" "1")
                                    (((file . 1) (root . 2)) "files" "1 2")))
                      (pcase-let ((`(,specs ,target ,ids) case))
                        (cl-letf (((symbol-function 'org-files-db-process--call-json)
                                   (lambda (arguments &optional _input)
                                     (setq called arguments)
                                     '((results . [a b])))))
                          (expect (org-files-db-reload-results
                                   (mapcar (lambda (spec) (record (car spec) (cdr spec)))
                                           specs)
                                   (presentation))
                                  :to-equal [a b])
                          (expect called
                                  :to-equal
                                  (list "query" "--format" "json"
                                        "--expect-database-id" "db-1"
                                        "--expect-generation" "4"
                                        "--config" (expand-file-name config)
                                        (format "(%s (id %s))" target ids))))))))

              (it "passes includes and an explicit configuration"
                  (let* ((main (org-files-db-test--config-file "main.toml"))
                         (work (org-files-db-test--config-file "work.toml"))
                         (org-files-db-configs `(("main" . ,main) ("work" . ,work)))
                         (org-files-db-default-config "main"))
                    (cl-letf (((symbol-function 'org-files-db-process--call-json)
                               (lambda (arguments &optional _input)
                                 (setq called arguments)
                                 '((results . [])))))
                      (org-files-db-reload-results
                       (list (record 'heading 2)) (presentation)
                       :includes '("properties" "path") :config "work")
                      (expect called
                              :to-equal
                              (list "query" "--format" "json"
                                    "--include" "properties" "--include" "path"
                                    "--expect-database-id" "db-1"
                                    "--expect-generation" "4"
                                    "--config" (expand-file-name work)
                                    "(headings (id 2))")))))

              (it "rejects empty and mixed-kind record lists"
                  (expect (org-files-db-reload-results nil (presentation))
                          :to-throw 'user-error)
                  (expect (org-files-db-reload-results
                           (list (record 'heading 2) (record 'link 3))
                           (presentation))
                          :to-throw 'user-error))

              (it "parses a recorded heading reload and propagates stale index"
                  (let* ((config (org-files-db-test--config-file "main.toml"))
                         (org-files-db-configs `(("main" . ,config)))
                         (org-files-db-default-config "main"))
                    (cl-letf (((symbol-function 'org-files-db-process--run-process)
                               (lambda (&rest _args)
                                 (list :status 0
                                       :stdout (org-files-db-test--fixture-text
                                                "query-json-headings-by-ids.json")
                                       :stderr ""))))
                      (expect (length (org-files-db-reload-results
                                       (list (record 'heading 2)) (presentation)))
                              :to-be-greater-than 0))
                    (cl-letf (((symbol-function 'org-files-db-process--run-process)
                               (lambda (&rest _args)
                                 (list :status 1 :stdout ""
                                       :stderr (org-files-db-test--fixture-text
                                                "error-stale-index.stderr")))))
                      (expect (org-files-db-reload-results
                               (list (record 'heading 2)) (presentation))
                              :to-throw 'org-files-db-stale-index)))))))

(describe "presentation-json version 3"
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

          (it "decodes rows, cells, roles, and row context from version 3 schemas"
              (let* ((wire
                      (org-files-db-test--wire
                       :database_id "db-1"
                       :generation 7
                       :results [[1 12 0 6 126]]
                       :schemas
                       (org-files-db-test--schemas
                        ["heading" "title" "todo" "done" "priority" "tag"])
                       :rows
                       [[0 nil [["Task" nil 1]]]
                        [0 ["tag" "project"] [["project" "project " 5]]]]))
                     (presentation (org-files-db-presentation--decode wire))
                     (rows (org-files-db-presentation-rows presentation))
                     (first-row (aref rows 0))
                     (second-row (aref rows 1))
                     (first-cell (aref (org-files-db-presentation-row-cells first-row) 0))
                     (second-cell (aref (org-files-db-presentation-row-cells second-row) 0))
                     (record (aref (org-files-db-presentation-results presentation) 0)))
                (expect (org-files-db-presentation-version presentation) :to-equal 3)
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
                            record)
                        :to-equal t)
                (expect (eq (org-files-db-presentation--row-result presentation second-row)
                            record)
                        :to-equal t)))

          (it "uses emitted schema field positions instead of fixed row positions"
              (let* ((schemas (copy-alist (org-files-db-test--schemas ["file-name"])))
                     (wire
                      (progn
                        (setf (alist-get 'row_fields schemas)
                              ["cells" "result_index" "row_context"])
                        (setf (alist-get 'cell_fields schemas)
                              ["role" "display_text" "search_text"])
                        (setf (alist-get 'row_context_shapes schemas)
                              '((tag . ["value" "kind"])))
                        (org-files-db-test--wire
                         :results [[2 3 0 1 nil]]
                         :schemas schemas
                         :rows [[[[0 nil "a.org"]] 0 ["project" "tag"]]])))
                     (presentation (org-files-db-presentation--decode wire))
                     (row (aref (org-files-db-presentation-rows presentation) 0))
                     (cell (aref (org-files-db-presentation-row-cells row) 0)))
                (expect (org-files-db-presentation-row-result-index row) :to-equal 0)
                (expect (org-files-db-presentation-row-row-context row)
                        :to-equal '((value . "project") (kind . "tag")))
                (expect (org-files-db-presentation-cell-search-text cell) :to-equal "a.org")
                (expect (org-files-db-presentation-cell-display-text cell) :to-equal "a.org")
                (expect (org-files-db-presentation-cell-role cell) :to-equal 'file-name)))

          (it "indexes the files table and keeps the reload identity"
              (let ((presentation
                     (org-files-db-presentation--decode
                      (org-files-db-test--fixture "links"))))
                (expect (org-files-db-presentation-files presentation)
                        :to-equal ["/notes/projects.org" "/notes/other.org"])
                (expect (org-files-db-presentation-database-id presentation)
                        :to-equal "e5410fc4-257f-49ab-84b1-e05803872aa0")
                (expect (org-files-db-presentation-generation presentation) :to-equal 1)))

          (it "decodes root, heading, and file records from recorded payloads"
              (let* ((headings
                      (org-files-db-presentation-results
                       (org-files-db-presentation--decode
                        (org-files-db-test--fixture "headings"))))
                     (files
                      (org-files-db-presentation-results
                       (org-files-db-presentation--decode
                        (org-files-db-test--fixture "files"))))
                     (root (aref headings 0))
                     (heading (aref headings 1))
                     (file (aref files 1)))
                (expect (length headings) :to-equal 5)
                (expect (org-files-db-record-kind root) :to-equal 'root)
                (expect (org-files-db-record-id root) :to-equal 1)
                (expect (org-files-db-record-file root) :to-equal "/notes/other.org")
                (expect (org-files-db-record-line root) :to-equal 1)
                (expect (org-files-db-record-byte-start root) :to-equal nil)
                (expect (org-files-db-record-kind heading) :to-equal 'heading)
                (expect (org-files-db-record-id heading) :to-equal 2)
                (expect (org-files-db-record-file heading) :to-equal "/notes/other.org")
                (expect (org-files-db-record-line heading) :to-equal 3)
                (expect (org-files-db-record-byte-start heading) :to-equal 16)
                (expect (org-files-db-record-target-file heading) :to-equal nil)
                (expect (org-files-db-record-kind file) :to-equal 'file)
                (expect (org-files-db-record-id file) :to-equal 2)
                (expect (org-files-db-record-file file) :to-equal "/notes/projects.org")
                (expect (org-files-db-record-byte-start file) :to-equal nil)))

          (it "decodes link records with resolved and unresolved targets"
              (let* ((results
                      (org-files-db-presentation-results
                       (org-files-db-presentation--decode
                        (org-files-db-test--fixture "links"))))
                     (file-target (aref results 0))
                     (unresolved (aref results 1))
                     (heading-target (aref results 2)))
                (expect (org-files-db-record-kind file-target) :to-equal 'link)
                (expect (org-files-db-record-id file-target) :to-equal 1)
                (expect (org-files-db-record-file file-target) :to-equal "/notes/projects.org")
                (expect (org-files-db-record-line file-target) :to-equal 5)
                (expect (org-files-db-record-byte-start file-target) :to-equal 58)
                (expect (org-files-db-record-target-file file-target) :to-equal "/notes/other.org")
                (expect (org-files-db-record-target-line file-target) :to-equal 1)
                (expect (org-files-db-record-target-byte-start file-target) :to-equal nil)
                (expect (org-files-db-record-target-resolved-p file-target) :to-equal t)
                (expect (org-files-db-record-target-file unresolved) :to-equal nil)
                (expect (org-files-db-record-target-line unresolved) :to-equal nil)
                (expect (org-files-db-record-target-byte-start unresolved) :to-equal nil)
                (expect (org-files-db-record-target-resolved-p unresolved) :to-equal nil)
                (expect (org-files-db-record-target-file heading-target) :to-equal "/notes/other.org")
                (expect (org-files-db-record-target-line heading-target) :to-equal 3)
                (expect (org-files-db-record-target-byte-start heading-target) :to-equal 16)))

          (it "rejects other presentation versions with a clear error"
              (dolist (version '(2 4))
                (let (message)
                  (condition-case err
                      (org-files-db-presentation--decode
                       (org-files-db-test--wire :presentation_version version))
                    (org-files-db-error
                     (setq message (error-message-string err))))
                  (expect message :to-match "Unsupported presentation version")
                  (expect message :to-match "expected 3"))))

          (it "rejects invalid payloads"
              (let ((schemas-with
                     (lambda (key value)
                       (let ((schemas (copy-alist (org-files-db-test--schemas))))
                         (setf (alist-get key schemas) value)
                         schemas))))
                (dolist (case
                         `(("missing files"
                            ,(assq-delete-all 'files (org-files-db-test--wire)))
                           ("files not array" ,(org-files-db-test--wire :files "/a.org"))
                           ("files entry not string" ,(org-files-db-test--wire :files [1]))
                           ("result_index out of range"
                            ,(org-files-db-test--wire :rows [[0 nil []]]))
                           ("role index out of range"
                            ,(org-files-db-test--wire
                              :results [[1 1 0 1 1]]
                              :rows [[0 nil [["Task" nil 5]]]]))
                           ("non-vector record"
                            ,(org-files-db-test--wire :results [((kind . "heading"))]))
                           ("empty record" ,(org-files-db-test--wire :results [[]]))
                           ("kind index out of range"
                            ,(org-files-db-test--wire :results [[4 1 0 1 1]]))
                           ("heading record too long"
                            ,(org-files-db-test--wire :results [[1 1 0 1 1 nil nil nil]]))
                           ("link record too short"
                            ,(org-files-db-test--wire :results [[3 1 0 1 1]]))
                           ("file index out of range"
                            ,(org-files-db-test--wire :results [[1 1 1 1 1]]))
                           ("negative file index"
                            ,(org-files-db-test--wire :results [[1 1 -1 1 1]]))
                           ("file index not integer"
                            ,(org-files-db-test--wire :results [[1 1 "a" 1 1]]))
                           ("id not integer"
                            ,(org-files-db-test--wire :results [[1 "x" 0 1 1]]))
                           ("line not integer"
                            ,(org-files-db-test--wire :results [[1 1 0 "x" 1]]))
                           ("target file out of range"
                            ,(org-files-db-test--wire :results [[3 1 0 1 1 5 1 nil]]))
                           ("target line not integer"
                            ,(org-files-db-test--wire :results [[3 1 0 1 1 0 "x" nil]]))
                           ("unsupported kind in schema"
                            ,(org-files-db-test--wire
                              :schemas (funcall schemas-with 'result_kinds ["root" "bogus"])))
                           ("changed shape"
                            ,(org-files-db-test--wire
                              :schemas (funcall schemas-with 'result_shapes
                                                '((root . ["kind" "id"])))))
                           ("unsupported file encoding"
                            ,(org-files-db-test--wire
                              :schemas (funcall schemas-with 'result_file_encoding "paths")))))
                  (expect (org-files-db-presentation--decode (cadr case))
                          :to-throw 'org-files-db-error))))

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
                             (org-files-db-test--empty-wire)))
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
                             (org-files-db-test--empty-wire))))
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
                       :version 3
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
                        :to-equal 'org-files-db-heading)
                (expect (get-text-property (length "A complete…  ") 'face visible)
                        :to-equal 'org-files-db-file-name)))

          (it "maps all supported semantic roles and ignores unknown roles"
              (expect (org-files-db-presentation--role-face 'heading)
                      :to-equal 'org-files-db-heading)
              (expect (org-files-db-presentation--role-face 'title)
                      :to-equal 'org-files-db-title)
              (expect (org-files-db-presentation--role-face 'todo)
                      :to-equal 'org-files-db-todo)
              (expect (org-files-db-presentation--role-face 'done)
                      :to-equal 'org-files-db-done)
              (expect (org-files-db-presentation--role-face 'priority)
                      :to-equal 'org-files-db-priority)
              (expect (org-files-db-presentation--role-face 'tag)
                      :to-equal 'org-files-db-tag)
              (expect (org-files-db-presentation--role-face 'date)
                      :to-equal 'org-files-db-date)
              (expect (org-files-db-presentation--role-face 'file-name)
                      :to-equal 'org-files-db-file-name)
              (expect (org-files-db-presentation--role-face 'file-path)
                      :to-equal 'org-files-db-file-path)
              (expect (org-files-db-presentation--role-face 'keyword-name)
                      :to-equal 'org-files-db-keyword-name)
              (expect (org-files-db-presentation--role-face 'keyword-value)
                      :to-equal 'org-files-db-keyword-value)
              (expect (org-files-db-presentation--role-face 'property-name)
                      :to-equal 'org-files-db-property-name)
              (expect (org-files-db-presentation--role-face 'property-value)
                      :to-equal 'org-files-db-property-value)
              (expect (org-files-db-presentation--role-face 'future-role)
                      :to-equal nil))

          (it "uses Org TODO keyword faces before semantic fallback faces"
              (let ((org-todo-keyword-faces
                     '(("REVIEW" . org-warning)
                       ("DONE" . "green")
                       ("CANCEL" . (:foreground "blue" :weight bold))
                       ("WAIT" . "orange"))))
                (expect
                 (org-files-db-presentation--role-face 'todo "REVIEW")
                 :to-equal 'org-warning)
                (expect
                 (org-files-db-presentation--role-face 'done "DONE")
                 :to-equal
                 (org-face-from-face-or-color 'todo 'org-todo "green"))
                (expect
                 (org-files-db-presentation--role-face 'done "CANCEL")
                 :to-equal '(:foreground "blue" :weight bold))
                (expect
                 (org-files-db-presentation--role-face 'todo "WAIT")
                 :to-equal
                 (org-face-from-face-or-color 'todo 'org-todo "orange"))
                (expect
                 (org-files-db-presentation--role-face 'todo "NEXT")
                 :to-equal 'org-files-db-todo)
                (expect
                 (org-files-db-presentation--role-face 'done "CLOSED")
                 :to-equal 'org-files-db-done)))

          (it "uses the Rust TODO role for fallback state"
              (let ((org-todo-keyword-faces nil))
                (expect
                 (org-files-db-presentation--role-face 'todo "DONE")
                 :to-equal 'org-files-db-todo)
                (expect
                 (org-files-db-presentation--role-face 'done "TODO")
                 :to-equal 'org-files-db-done)))

          (it "uses full TODO search text when display text is formatted"
              (let* ((org-todo-keyword-faces '(("REVIEW" . org-warning)))
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
                      (org-files-db-presentation--visible-row row)))
                (expect (get-text-property 0 'face visible)
                        :to-equal 'org-warning)))

          (it "faces only non-empty cells and leaves the cell texts untouched"
              (let* ((text (copy-sequence "Task"))
                     (row
                      (org-files-db-presentation--make-presentation-row
                       :result-index 0
                       :row-context nil
                       :cells
                       (vector
                        (org-files-db-presentation--make-presentation-cell
                         :search-text "Task" :display-text text :role 'title)
                        (org-files-db-presentation--make-presentation-cell
                         :search-text "" :display-text "" :role 'tag)
                        (org-files-db-presentation--make-presentation-cell
                         :search-text "a" :display-text "a" :role 'tag))))
                     (visible (org-files-db-presentation--visible-row row)))
                (expect (substring-no-properties visible) :to-equal "Task    a")
                (expect (get-text-property 0 'face visible)
                        :to-equal 'org-files-db-title)
                (expect (get-text-property 4 'face visible) :to-equal nil)
                (expect (get-text-property 8 'face visible)
                        :to-equal 'org-files-db-tag)
                (expect (text-properties-at 0 text) :to-equal nil)))

          (it "maps vectors without changing the input"
              (let ((input (vector 1 2 3)))
                (expect (org-files-db-presentation--map-vector #'1+ input)
                        :to-equal (vector 2 3 4))
                (expect input :to-equal (vector 1 2 3))))

          (it "defines one heading face with normal completion text height"
              (expect (not (null (facep 'org-files-db-heading))) :to-equal t)
              (expect (= (face-attribute 'org-files-db-heading :height nil 'default) 1.0)
                      :to-equal t))

          (it "uses the same heading face for all heading result levels"
              (let ((row
                     (org-files-db-presentation--make-presentation-row
                      :result-index 0
                      :row-context nil
                      :cells
                      (vector
                       (org-files-db-presentation--make-presentation-cell
                        :search-text "Heading"
                        :display-text "Heading"
                        :role 'heading)))))
                (dolist (result '(((kind . "heading") (level . 1))
                                  ((kind . "heading") (level . 7))
                                  ((kind . "link") (heading_level . 4))))
                  (let* ((presentation
                          (org-files-db-presentation--make-presentation
                           :version 3
                           :database-id "db"
                           :generation 1
                           :config "main"
                           :results (vector result)
                           :schemas nil
                           :rows (vector row)))
                         (candidate
                          (car (org-files-db-presentation--candidates presentation)))
                         (visible (get-text-property 0 'display candidate)))
                    (expect (get-text-property 0 'face visible)
                            :to-equal 'org-files-db-heading)))))

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
                       :version 3
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
                       :version 3
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
                       :version 3
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
                             (org-files-db-test--empty-wire))))
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


(describe "watcher lifecycle management"
          (before-each
           (setq org-files-db-watch-mode nil
                 org-files-db-watch--activation nil))

          (after-each
           (setq org-files-db-watch-mode nil
                 org-files-db-watch--activation nil))

          (it "detects an external watcher through the read-only view probe"
              (let (arguments)
                (cl-letf (((symbol-function 'org-files-db-process--run-process)
                           (lambda (value)
                             (setq arguments value)
                             '(:status 1
                                       :stdout ""
                                       :stderr "presentation view request failed (view_not_found): presentation view `__org-files-db-emacs-watcher-probe__` is not registered\n"))))
                  (expect (org-files-db-watch--probe-active-p "/tmp/config.toml")
                          :to-equal t))
                (expect arguments
                        :to-equal
                        '("view" "show" "--config" "/tmp/config.toml"
                          "__org-files-db-emacs-watcher-probe__"))))

          (it "treats a watcher control connection failure as no active watcher"
              (cl-letf (((symbol-function 'org-files-db-process--run-process)
                         (lambda (_arguments)
                           '(:status 1
                                     :stdout ""
                                     :stderr "failed to connect to the active watcher presentation view registry at /tmp/view.sock: No such file or directory\n"))))
                (expect (org-files-db-watch--probe-active-p "/tmp/config.toml")
                        :to-equal nil)))

          (it "rejects watcher probe errors that do not mean absence"
              (cl-letf (((symbol-function 'org-files-db-process--run-process)
                         (lambda (_arguments)
                           '(:status 1
                                     :stdout ""
                                     :stderr "invalid configuration\n"))))
                (expect (org-files-db-watch--probe-active-p "/tmp/config.toml")
                        :to-throw 'org-files-db-cli-error)))

          (it "starts missing watchers and keeps external watchers"
              (let (started)
                (cl-letf (((symbol-function 'org-files-db-watch--configured-targets)
                           (lambda ()
                             '(("main" . "/tmp/main.toml")
                               ("work" . "/tmp/work.toml"))))
                          ((symbol-function 'org-files-db-watch--probe-active-p)
                           (lambda (file) (equal file "/tmp/main.toml")))
                          ((symbol-function 'org-files-db-watch--start-owned-watcher)
                           (lambda (name file)
                             (push name started)
                             (org-files-db-watch--entry-create
                              :config name
                              :config-file file
                              :ownership 'owned
                              :state 'ready
                              :process 'work-process))))
                  (let ((snapshot (org-files-db-watch--activate)))
                    (expect started :to-equal '("work"))
                    (expect (length snapshot) :to-equal 2)
                    (expect (org-files-db-watch--entry-ownership (car snapshot))
                            :to-equal 'external)
                    (expect (org-files-db-watch--entry-ownership (cadr snapshot))
                            :to-equal 'owned)))))

          (it "rolls back owned watchers after partial activation failure"
              (let (stopped caught)
                (cl-letf (((symbol-function 'org-files-db-watch--configured-targets)
                           (lambda ()
                             '(("main" . "/tmp/main.toml")
                               ("work" . "/tmp/work.toml"))))
                          ((symbol-function 'org-files-db-watch--probe-active-p)
                           (lambda (_file) nil))
                          ((symbol-function 'org-files-db-watch--start-owned-watcher)
                           (lambda (name file)
                             (if (equal name "work")
                                 (signal 'org-files-db-error
                                         '("Cannot start work watcher"))
                               (org-files-db-watch--entry-create
                                :config name
                                :config-file file
                                :ownership 'owned
                                :state 'ready
                                :process 'main-process))))
                          ((symbol-function 'org-files-db-watch--stop-owned-entry)
                           (lambda (entry)
                             (push (org-files-db-watch--entry-config entry) stopped))))
                  (condition-case err
                      (org-files-db-watch--activate)
                    (org-files-db-error
                     (setq caught err)))
                  (expect (car caught) :to-equal 'org-files-db-error)
                  (expect (cadr caught) :to-equal "Cannot start work watcher")
                  (expect stopped :to-equal '("main"))
                  (expect org-files-db-watch--activation :to-equal nil))))

          (it "stops only Emacs-owned watcher entries"
              (let ((owned
                     (org-files-db-watch--entry-create
                      :config "main"
                      :config-file "/tmp/main.toml"
                      :ownership 'owned
                      :state 'ready
                      :process 'main-process))
                    (external
                     (org-files-db-watch--entry-create
                      :config "work"
                      :config-file "/tmp/work.toml"
                      :ownership 'external
                      :state 'ready
                      :process nil))
                    stopped)
                (cl-letf (((symbol-function 'org-files-db-watch--stop-owned-entry)
                           (lambda (entry)
                             (push (org-files-db-watch--entry-config entry) stopped))))
                  (org-files-db-watch--stop-owned-entries (list owned external)))
                (expect stopped :to-equal '("main"))))

          (it "uses the activation snapshot when stopping"
              (let* ((entry
                      (org-files-db-watch--entry-create
                       :config "main"
                       :config-file "/tmp/old.toml"
                       :ownership 'owned
                       :state 'ready
                       :process 'old-process))
                     (org-files-db-watch--activation (list entry))
                     (org-files-db-configs '(("new" . "/tmp/new.toml")))
                     stopped)
                (cl-letf (((symbol-function 'org-files-db-watch--stop-owned-entry)
                           (lambda (value)
                             (push (org-files-db-watch--entry-config-file value)
                                   stopped))))
                  (org-files-db-watch--deactivate))
                (expect stopped :to-equal '("/tmp/old.toml"))
                (expect org-files-db-watch--activation :to-equal nil)))

          (it "keeps startup diagnostics until activation reports the failure"
              (let* ((stderr (generate-new-buffer " *org-files-db-watch-startup-error*"))
                     (entry
                      (org-files-db-watch--entry-create
                       :config "main"
                       :config-file "/tmp/main.toml"
                       :ownership 'owned
                       :state 'starting
                       :process 'watcher))
                     cleaned)
                (with-current-buffer stderr
                  (insert "Cannot open configured source\n"))
                (cl-letf (((symbol-function 'process-status)
                           (lambda (_process) 'exit))
                          ((symbol-function 'process-get)
                           (lambda (_process key)
                             (pcase key
                               ('org-files-db-watch--entry entry)
                               ('org-files-db-watch--expected-stop nil)
                               ('org-files-db-watch--starting t)
                               ('org-files-db-watch--stderr-buffer stderr))))
                          ((symbol-function 'org-files-db-watch--cleanup-process-buffers)
                           (lambda (_process) (setq cleaned t))))
                  (org-files-db-watch--sentinel
                   'watcher "exited abnormally with code 1\n"))
                (expect cleaned :to-equal nil)
                (expect (buffer-live-p stderr) :to-equal t)
                (expect (org-files-db-watch--entry-state entry) :to-equal 'failed)
                (expect (org-files-db-watch--entry-failure entry)
                        :to-match "Cannot open configured source")
                (kill-buffer stderr)))

          (it "records unexpected watcher exits without restarting"
              (let* ((entry
                      (org-files-db-watch--entry-create
                       :config "main"
                       :config-file "/tmp/main.toml"
                       :ownership 'owned
                       :state 'ready
                       :process 'watcher))
                     (org-files-db-watch-mode t)
                     warning)
                (cl-letf (((symbol-function 'display-warning)
                           (lambda (_type message &rest _args)
                             (setq warning message)))
                          ((symbol-function 'org-files-db-watch--start-owned-watcher)
                           (lambda (&rest _args)
                             (error "Unexpected restart"))))
                  (org-files-db-watch--record-unexpected-exit
                   entry "exited abnormally with code 1" "watcher failed"))
                (expect (org-files-db-watch--entry-state entry) :to-equal 'failed)
                (expect (org-files-db-watch--entry-failure entry)
                        :to-match "watcher failed")
                (expect warning :to-match "Watcher for configuration `main' exited unexpectedly")))

          (it "runs exit cleanup hooks before stopping owned watchers"
              (let* ((order nil)
                     (entry
                      (org-files-db-watch--entry-create
                       :config "main"
                       :config-file "/tmp/main.toml"
                       :ownership 'owned
                       :state 'ready
                       :process 'watcher))
                     (org-files-db-watch--activation (list entry))
                     (org-files-db-watch--before-exit-hook
                      (list (lambda () (push 'views order)))))
                (cl-letf (((symbol-function 'org-files-db-watch--stop-owned-entry)
                           (lambda (_entry) (push 'watcher order))))
                  (org-files-db-watch--cleanup-at-exit))
                (expect (nreverse order) :to-equal '(views watcher))
                (expect org-files-db-watch--activation :to-equal nil)))

          (it "starts and stops through the public mode commands"
              (let (events)
                (cl-letf (((symbol-function 'org-files-db-watch--activate)
                           (lambda () (push 'start events)))
                          ((symbol-function 'org-files-db-watch--deactivate)
                           (lambda () (push 'stop events))))
                  (org-files-db-watch-start)
                  (expect org-files-db-watch-mode :to-equal t)
                  (org-files-db-watch-stop)
                  (expect org-files-db-watch-mode :to-equal nil))
                (expect (nreverse events) :to-equal '(start stop))))

          (it "keeps watch mode disabled when activation fails"
              (let ((org-files-db-watch-mode nil)
                    (org-files-db-watch--activation nil)
                    caught)
                (cl-letf (((symbol-function 'org-files-db-watch--activate)
                           (lambda ()
                             (signal 'org-files-db-error
                                     '("Watcher startup failed")))))
                  (condition-case err
                      (org-files-db-watch-mode 1)
                    (org-files-db-error
                     (setq caught err))))
                (expect (car caught) :to-equal 'org-files-db-error)
                (expect org-files-db-watch-mode :to-equal nil)
                (expect org-files-db-watch--activation :to-equal nil)))

          (it "does not reactivate an already active watch mode"
              (let ((org-files-db-watch-mode nil)
                    (org-files-db-watch--activation nil)
                    activations)
                (cl-letf (((symbol-function 'org-files-db-watch--activate)
                           (lambda ()
                             (push 'activate activations)
                             (setq org-files-db-watch--activation '(snapshot))
                             org-files-db-watch--activation))
                          ((symbol-function 'org-files-db-watch--deactivate)
                           #'ignore))
                  (org-files-db-watch-mode 1)
                  (org-files-db-watch-mode 1))
                (expect activations :to-equal '(activate))))

          (it "recognizes the exact Rust watcher readiness line"
              (let ((buffer (generate-new-buffer " *org-files-db-watch-ready*")))
                (unwind-protect
                    (progn
                      (with-current-buffer buffer
                        (insert "watcher ready\n"))
                      (expect (not (null (org-files-db-watch--ready-p buffer)))
                              :to-equal t))
                  (when (buffer-live-p buffer)
                    (kill-buffer buffer)))))

          (it "accepts process output until the ready marker arrives"
              (let ((live t)
                    command
                    stdout-buffer
                    stderr-buffer
                    accepted)
                (cl-letf (((symbol-function 'org-files-db-process--resolve-executable)
                           (lambda () "/opt/bin/orgfdb"))
                          ((symbol-function 'make-process)
                           (lambda (&rest properties)
                             (setq command (plist-get properties :command)
                                   stdout-buffer (plist-get properties :buffer)
                                   stderr-buffer (plist-get properties :stderr))
                             'watcher-process))
                          ((symbol-function 'process-live-p)
                           (lambda (_process) live))
                          ((symbol-function 'process-put)
                           (lambda (&rest _args) nil))
                          ((symbol-function 'process-exit-status)
                           (lambda (_process) 1))
                          ((symbol-function 'accept-process-output)
                           (lambda (process &rest _args)
                             (push process accepted)
                             (with-current-buffer stderr-buffer
                               (insert "watcher ready\n"))
                             t)))
                  (let ((entry
                         (org-files-db-watch--start-owned-watcher
                          "main" "/tmp/main.toml")))
                    (expect command
                            :to-equal
                            '("/opt/bin/orgfdb" "watch" "--config" "/tmp/main.toml"))
                    (expect (org-files-db-watch--entry-state entry) :to-equal 'ready)
                    (expect (org-files-db-watch--entry-process entry)
                            :to-equal 'watcher-process)
                    (expect accepted :to-contain nil)))
                (when (buffer-live-p stdout-buffer)
                  (kill-buffer stdout-buffer))
                (when (buffer-live-p stderr-buffer)
                  (kill-buffer stderr-buffer))))

          (it "times out watcher startup instead of waiting forever"
              (let ((org-files-db-watch-startup-timeout 0)
                    (live t)
                    stdout-buffer
                    stderr-buffer)
                (cl-letf (((symbol-function 'org-files-db-process--resolve-executable)
                           (lambda () "/opt/bin/orgfdb"))
                          ((symbol-function 'make-process)
                           (lambda (&rest properties)
                             (setq stdout-buffer (plist-get properties :buffer)
                                   stderr-buffer (plist-get properties :stderr))
                             'watcher-process))
                          ((symbol-function 'process-live-p)
                           (lambda (_process) live))
                          ((symbol-function 'process-put)
                           (lambda (&rest _args) nil))
                          ((symbol-function 'org-files-db-watch--cleanup-process-buffers)
                           #'ignore)
                          ((symbol-function 'accept-process-output)
                           (lambda (&rest _args) nil))
                          ((symbol-function 'interrupt-process)
                           (lambda (_process) (setq live nil))))
                  (expect
                   (org-files-db-watch--start-owned-watcher
                    "main" "/tmp/main.toml")
                   :to-throw 'org-files-db-error))
                (when (buffer-live-p stdout-buffer)
                  (kill-buffer stdout-buffer))
                (when (buffer-live-p stderr-buffer)
                  (kill-buffer stderr-buffer)))))



(describe "predefined views and Rust cache integration"
          (before-each
           (setq org-files-db-test--directory
                 (make-temp-file "org-files-db-views-test-" t)
                 org-files-db-cache-mode nil
                 org-files-db-cache--activation nil
                 org-files-db-watch-mode nil
                 org-files-db-watch--activation nil))

          (after-each
           (setq org-files-db-cache-mode nil
                 org-files-db-cache--activation nil
                 org-files-db-watch-mode nil
                 org-files-db-watch--activation nil)
           (when (file-directory-p org-files-db-test--directory)
             (delete-directory org-files-db-test--directory t)))

          (it "resolves flat views with inherited defaults and cache disabled by default"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config "main")
                     (org-files-db-heading-columns '((title)))
                     (org-files-db-heading-sort '((title :direction asc)))
                     (org-files-db-heading-action #'ignore)
                     (view '("tasks" :query (headings)))
                     (resolved (org-files-db-views--resolve view)))
                (expect (org-files-db-views--resolved-name resolved)
                        :to-equal "tasks")
                (expect (org-files-db-views--resolved-config resolved)
                        :to-equal "main")
                (expect (org-files-db-views--resolved-config-file resolved)
                        :to-equal (expand-file-name main))
                (expect (org-files-db-views--resolved-query resolved)
                        :to-equal '(headings))
                (expect (org-files-db-views--resolved-columns resolved)
                        :to-equal '((title)))
                (expect (org-files-db-views--resolved-sort resolved)
                        :to-equal '((title :direction asc)))
                (expect (org-files-db-views--resolved-cache resolved)
                        :to-equal nil)
                (expect (org-files-db-views--resolved-action resolved)
                        :to-equal #'ignore)
                (expect (org-files-db-views--resolved-p resolved) :to-equal t)))

          (it "rejects missing queries, unsupported keys, duplicate keys, and invalid cache values"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config "main"))
                (dolist (views
                         '((("missing-query" :cache t))
                           (("unknown-key" :query (headings) :future t))
                           (("duplicate-key" :query (headings) :cache t :cache nil))
                           (("bad-cache" :query (headings) :cache yes))))
                  (let ((org-files-db-views views))
                    (expect (org-files-db-views--validate-views)
                            :to-throw 'user-error)))))

          (it "runs cache-enabled views through the normal query path when cache mode is disabled"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config "main")
                     (org-files-db-heading-columns '((title)))
                     (org-files-db-heading-sort nil)
                     (org-files-db-views
                      '(("tasks" :query (headings) :cache t :action ignore)))
                     seen)
                (cl-letf (((symbol-function 'org-files-db-query)
                           (lambda (query &rest arguments)
                             (setq seen (cons query arguments))
                             'selected)))
                  (expect (org-files-db-view "tasks") :to-equal 'selected))
                (expect (car seen) :to-equal '(headings))
                (expect (plist-get (cdr seen) :config) :to-equal "main")
                (expect (plist-get (cdr seen) :columns) :to-equal '((title)))
                (expect (plist-get (cdr seen) :sort) :to-equal nil)
                (expect (plist-get (cdr seen) :action) :to-equal #'ignore)))

          (it "keeps non-cached views on the one-shot path while cache mode is active"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config "main")
                     (org-files-db-heading-action #'ignore)
                     (org-files-db-views
                      '(("fresh" :query (headings) :cache nil :action ignore)))
                     (org-files-db-cache-mode t)
                     (org-files-db-cache--activation
                      (org-files-db-cache--activation-create :entries nil))
                     seen)
                (cl-letf (((symbol-function 'org-files-db-query)
                           (lambda (query &rest arguments)
                             (setq seen (cons query arguments))
                             'selected))
                          ((symbol-function 'org-files-db-cache--read-entry)
                           (lambda (&rest _args)
                             (error "Cached read must not run"))))
                  (expect (org-files-db-view "fresh") :to-equal 'selected))
                (expect (car seen) :to-equal '(headings))
                (expect (plist-get (cdr seen) :config) :to-equal "main")))

          (it "asks for a view name during interactive use"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config "main")
                     (org-files-db-heading-action #'ignore)
                     (org-files-db-views '(("tasks" :query (headings))))
                     prompt
                     collection
                     selected)
                (cl-letf (((symbol-function 'completing-read)
                           (lambda (value choices &rest _args)
                             (setq prompt value
                                   collection choices)
                             "tasks"))
                          ((symbol-function 'org-files-db-query)
                           (lambda (&rest _args)
                             (setq selected t)
                             'result)))
                  (expect (call-interactively #'org-files-db-view)
                          :to-equal 'result))
                (expect prompt :to-equal "org-files-db view: ")
                (expect collection :to-equal '("tasks"))
                (expect selected :to-equal t)))

          (it "creates stable private Rust names inside one Emacs session"
              (let ((org-files-db-cache--session-id "session-a"))
                (expect (org-files-db-cache--rust-name "open tasks")
                        :to-equal
                        (org-files-db-cache--rust-name "open tasks"))
                (expect (org-files-db-cache--rust-name "open tasks")
                        :to-match "\\`__org-files-db-emacs-session-a-")
                (expect
                 (equal (org-files-db-cache--rust-name "open tasks")
                        (org-files-db-cache--rust-name "closed tasks"))
                 :to-equal nil)))

          (it "registers only cache-enabled views and stores resolved action requirements"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config "main")
                     (org-files-db-heading-columns '((title)))
                     (org-files-db-heading-sort '((title :direction asc)))
                     (org-files-db-heading-action #'ignore)
                     (org-files-db-views
                      '(("cached" :query (headings) :cache t)
                        ("fresh" :query (headings))))
                     (org-files-db-cache--session-id "test-session")
                     registered)
                (cl-letf (((symbol-function 'org-files-db-watch-start)
                           (lambda () (setq org-files-db-watch-mode t)))
                          ((symbol-function 'org-files-db-watch--probe-active-p)
                           (lambda (_file) t))
                          ((symbol-function 'org-files-db-process--call-json)
                           (lambda (arguments)
                             (push arguments registered)
                             '((status . "registered")))))
                  (org-files-db-cache-mode 1))
                (let* ((activation org-files-db-cache--activation)
                       (entries
                        (org-files-db-cache--activation-entries activation))
                       (entry (car entries))
                       (resolved (org-files-db-cache--entry-resolved entry))
                       (arguments (car registered)))
                  (expect org-files-db-cache-mode :to-equal t)
                  (expect org-files-db-watch-mode :to-equal t)
                  (expect (length entries) :to-equal 1)
                  (expect (org-files-db-views--resolved-name resolved)
                          :to-equal "cached")
                  (expect (org-files-db-views--resolved-columns resolved)
                          :to-equal '((title)))
                  (expect (org-files-db-views--resolved-sort resolved)
                          :to-equal '((title :direction asc)))
                  (expect (org-files-db-cache--entry-rust-name entry)
                          :to-match "test-session")
                  (expect arguments :to-contain "view")
                  (expect arguments :to-contain "register")
                  (expect arguments :not :to-contain "--include"))))

          (it "rolls back registrations and watch mode after cache activation failure"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (work (org-files-db-test--config-file "work.toml"))
                     (org-files-db-configs `(("main" . ,main) ("work" . ,work)))
                     (org-files-db-default-config "main")
                     (org-files-db-heading-action #'ignore)
                     (org-files-db-views
                      '(("one" :query (headings) :cache t)
                        ("two" :config "work" :query (headings) :cache t)))
                     registered
                     removed
                     stopped
                     caught)
                (cl-letf (((symbol-function 'org-files-db-watch-start)
                           (lambda () (setq org-files-db-watch-mode t)))
                          ((symbol-function 'org-files-db-watch-stop)
                           (lambda ()
                             (setq stopped t
                                   org-files-db-watch-mode nil)))
                          ((symbol-function 'org-files-db-watch--probe-active-p)
                           (lambda (_file) t))
                          ((symbol-function 'org-files-db-cache--register-entry)
                           (lambda (entry)
                             (let ((name
                                    (org-files-db-views--resolved-name
                                     (org-files-db-cache--entry-resolved entry))))
                               (if registered
                                   (signal 'org-files-db-error '("Registration failed"))
                                 (push name registered)))))
                          ((symbol-function 'org-files-db-cache--remove-entry)
                           (lambda (entry)
                             (push
                              (org-files-db-views--resolved-name
                               (org-files-db-cache--entry-resolved entry))
                              removed))))
                  (condition-case err
                      (org-files-db-cache-mode 1)
                    (org-files-db-error (setq caught err))))
                (expect (car caught) :to-equal 'org-files-db-error)
                (expect registered :to-equal '("one"))
                (expect removed :to-equal '("one" "two"))
                (expect stopped :to-equal t)
                (expect org-files-db-cache-mode :to-equal nil)
                (expect org-files-db-watch-mode :to-equal nil)
                (expect org-files-db-cache--activation :to-equal nil)))

          (it "does not stop a pre-existing watch mode after cache activation failure"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config "main")
                     (org-files-db-heading-action #'ignore)
                     (org-files-db-views
                      '(("cached" :query (headings) :cache t)))
                     (org-files-db-watch-mode t)
                     stopped
                     caught)
                (cl-letf (((symbol-function 'org-files-db-watch--probe-active-p)
                           (lambda (_file) t))
                          ((symbol-function 'org-files-db-watch-stop)
                           (lambda () (setq stopped t)))
                          ((symbol-function 'org-files-db-cache--register-entry)
                           (lambda (_entry)
                             (signal 'org-files-db-error '("Registration failed"))))
                          ((symbol-function 'org-files-db-cache--remove-entry)
                           #'ignore))
                  (condition-case err
                      (org-files-db-cache-mode 1)
                    (org-files-db-error (setq caught err))))
                (expect (car caught) :to-equal 'org-files-db-error)
                (expect stopped :to-equal nil)
                (expect org-files-db-watch-mode :to-equal t)))

          (it "removes registrations from the activation snapshot instead of current views"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config "main")
                     (org-files-db-heading-action #'ignore)
                     (org-files-db-views
                      '(("cached" :query (headings) :cache t)))
                     (resolved (org-files-db-views--resolve (car org-files-db-views)))
                     (entry
                      (org-files-db-cache--entry-create
                       :resolved resolved :rust-name "private-old"))
                     (org-files-db-cache-mode t)
                     (org-files-db-cache--activation
                      (org-files-db-cache--activation-create
                       :entries (list entry)))
                     removed)
                (setq org-files-db-views
                      '(("cached" :query (files) :cache t)))
                (cl-letf (((symbol-function 'org-files-db-cache--remove-entry)
                           (lambda (value)
                             (push (org-files-db-cache--entry-rust-name value)
                                   removed))))
                  (org-files-db-cache-mode -1))
                (expect removed :to-equal '("private-old"))
                (expect org-files-db-cache--activation :to-equal nil)))

          (it "uses the cached presentation path for an active cached view"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config "main")
                     (org-files-db-heading-action #'ignore)
                     (org-files-db-views
                      '(("cached" :query (headings) :cache t :action ignore)))
                     (resolved (org-files-db-views--resolve (car org-files-db-views)))
                     (entry
                      (org-files-db-cache--entry-create
                       :resolved resolved :rust-name "private"))
                     (org-files-db-cache-mode t)
                     (org-files-db-cache--activation
                      (org-files-db-cache--activation-create
                       :entries (list entry)))
                     (result '((kind . "heading") (title . "Task")))
                     (presentation
                      (org-files-db-test--single-result-presentation result "main")))
                (cl-letf (((symbol-function 'org-files-db-cache--read-entry)
                           (lambda (_entry) presentation))
                          ((symbol-function 'org-files-db-query)
                           (lambda (&rest _args)
                             (error "One-shot fallback must not run")))
                          ((symbol-function 'completing-read)
                           #'org-files-db-test--select-first-candidate))
                  (expect (org-files-db-view "cached") :to-equal result))))

          (it "requires a cache-mode restart after a cached definition changes"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config "main")
                     (org-files-db-heading-columns '((title)))
                     (org-files-db-heading-action #'ignore)
                     (org-files-db-views
                      '(("cached" :query (headings) :cache t :action ignore)))
                     (resolved (org-files-db-views--resolve (car org-files-db-views)))
                     (entry
                      (org-files-db-cache--entry-create
                       :resolved resolved :rust-name "private"))
                     (org-files-db-cache-mode t)
                     (org-files-db-cache--activation
                      (org-files-db-cache--activation-create
                       :entries (list entry)))
                     caught)
                (setq org-files-db-heading-columns '((outline-path)))
                (condition-case err
                    (org-files-db-view "cached")
                  (user-error (setq caught err)))
                (expect (error-message-string caught)
                        :to-match "restart org-files-db-cache-mode")))

          (it "re-registers the stored snapshot once after view_not_found"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config "main")
                     (org-files-db-heading-action #'ignore)
                     (org-files-db-views
                      '(("cached" :query (headings) :cache t)))
                     (resolved (org-files-db-views--resolve (car org-files-db-views)))
                     (entry
                      (org-files-db-cache--entry-create
                       :resolved resolved :rust-name "private"))
                     (reads 0)
                     (registrations 0))
                (cl-letf (((symbol-function 'org-files-db-cache--view-read-result)
                           (lambda (_entry)
                             (setq reads (1+ reads))
                             (if (= reads 1)
                                 '(:status 1 :stdout ""
                                           :stderr "presentation view request failed (view_not_found): presentation view `private' is not registered")
                               (list :status 0
                                     :stdout (org-files-db-test--empty-presentation-json)
                                     :stderr ""))))
                          ((symbol-function 'org-files-db-cache--register-entry)
                           (lambda (value)
                             (expect value :to-be entry)
                             (setq registrations (1+ registrations)))))
                  (let ((presentation
                         (org-files-db-cache--read-entry entry)))
                    (expect (org-files-db-presentation-config presentation)
                            :to-equal "main")))
                (expect reads :to-equal 2)
                (expect registrations :to-equal 1)))

          (it "does not fall back to a one-shot query on cached read errors"
              (let* ((main (org-files-db-test--config-file "main.toml"))
                     (org-files-db-configs `(("main" . ,main)))
                     (org-files-db-default-config "main")
                     (org-files-db-heading-action #'ignore)
                     (org-files-db-views
                      '(("cached" :query (headings) :cache t :action ignore)))
                     (resolved (org-files-db-views--resolve (car org-files-db-views)))
                     (entry
                      (org-files-db-cache--entry-create
                       :resolved resolved :rust-name "private"))
                     (org-files-db-cache-mode t)
                     (org-files-db-cache--activation
                      (org-files-db-cache--activation-create
                       :entries (list entry)))
                     fallback
                     caught)
                (cl-letf (((symbol-function 'org-files-db-cache--view-read-result)
                           (lambda (_entry)
                             '(:status 1 :stdout "" :stderr "cache rebuild failed")))
                          ((symbol-function 'org-files-db-query)
                           (lambda (&rest _args) (setq fallback t))))
                  (condition-case err
                      (org-files-db-view "cached")
                    (org-files-db-cli-error (setq caught err))))
                (expect (car caught) :to-equal 'org-files-db-cli-error)
                (expect fallback :to-equal nil)))

          (it "prevents watch-mode shutdown while cache mode is active"
              (let ((org-files-db-watch-mode t)
                    (org-files-db-cache-mode t)
                    (org-files-db-watch--activation '(snapshot))
                    caught)
                (condition-case err
                    (org-files-db-watch-mode -1)
                  (user-error (setq caught err)))
                (expect (error-message-string caught)
                        :to-match "Disable org-files-db-cache-mode")
                (expect org-files-db-watch-mode :to-equal t)))

          (it "cache-stop with an argument disables cache before watch mode"
              (let ((org-files-db-cache-mode t)
                    order)
                (cl-letf (((symbol-function 'org-files-db-cache-mode)
                           (lambda (value)
                             (push (list 'cache value) order)
                             (setq org-files-db-cache-mode nil)))
                          ((symbol-function 'org-files-db-watch-stop)
                           (lambda () (push 'watch order))))
                  (org-files-db-cache-stop t))
                (expect (nreverse order)
                        :to-equal '((cache -1) watch))))

          (it "removes cached registrations before watcher shutdown at normal exit"
              (let* ((resolved
                      (org-files-db-views--resolved-create
                       :name "cached"
                       :config "main"
                       :config-file "/tmp/main.toml"
                       :query '(headings)
                       :query-string "(headings)"
                       :target 'headings
                       :columns '((title))
                       :sort nil
                       :row-source nil
                       :cache t
                       :action #'ignore))
                     (entry
                      (org-files-db-cache--entry-create
                       :resolved resolved :rust-name "private"))
                     (org-files-db-cache-mode t)
                     (org-files-db-cache--activation
                      (org-files-db-cache--activation-create
                       :entries (list entry)))
                     (org-files-db-watch--activation '(watcher))
                     order)
                (cl-letf (((symbol-function 'org-files-db-cache--remove-entry)
                           (lambda (_entry) (push 'view order)))
                          ((symbol-function 'org-files-db-watch--deactivate)
                           (lambda () (push 'watcher order))))
                  (org-files-db-watch--cleanup-at-exit))
                (expect (nreverse order) :to-equal '(view watcher))
                (expect org-files-db-cache-mode :to-equal nil)
                (expect org-files-db-cache--activation :to-equal nil)))
          )

(defvar org-files-db-test--actions-directory nil)

(defconst org-files-db-test--actions-prefix "#+title: Notes\n\n"
  "Text before the first heading in the action test file.")

(defconst org-files-db-test--actions-first "* TODO Grüße aus Zürich\n"
  "First heading of the action test file.")

(defun org-files-db-test--actions-file (name)
  "Create the multibyte action test file NAME and return its absolute path."
  (let ((file (expand-file-name name org-files-db-test--actions-directory)))
    (with-temp-file file
      (let ((coding-system-for-write 'utf-8-unix))
        (insert org-files-db-test--actions-prefix
                org-files-db-test--actions-first
                "body\n** Zweiter Üß\n")))
    file))

(defun org-files-db-test--actions-configs ()
  "Return `org-files-db-configs' with a main configuration file."
  (let ((config (expand-file-name "main.toml" org-files-db-test--actions-directory)))
    (with-temp-file config (insert "[database]\n"))
    `(("main" . ,config))))

(defun org-files-db-test--actions-record (kind file &rest slots)
  "Return a record of KIND for FILE with SLOTS as keyword arguments."
  (apply #'org-files-db-presentation--make-record
         :kind kind :id 1 :file file slots))

(defun org-files-db-test--corpus-fixture (name corpus)
  "Return fixture NAME with @CORPUS@ replaced by directory CORPUS."
  (string-replace "@CORPUS@" (directory-file-name corpus)
                  (org-files-db-test--fixture-text name)))

(describe "result actions"
          (let (file other messages)
            (before-each
             (setq org-files-db-test--actions-directory
                   (make-temp-file "org-files-db-actions-" t)
                   file (org-files-db-test--actions-file "notes.org")
                   other (org-files-db-test--actions-file "other.org")
                   messages nil)
             (spy-on 'message :and-call-fake
                     (lambda (format &rest args)
                       (push (apply #'format format args) messages)))
             (spy-on 'org-files-db-process--run-process :and-throw-error 'error))

            (after-each
             (dolist (buffer (buffer-list))
               (when-let* ((name (buffer-file-name buffer)))
                 (when (string-prefix-p org-files-db-test--actions-directory name)
                   (with-current-buffer buffer (set-buffer-modified-p nil))
                   (kill-buffer buffer))))
             (delete-directory org-files-db-test--actions-directory t))

            (it "opens headings, files, roots and links at their source location"
                (let* ((first-byte (string-bytes org-files-db-test--actions-prefix))
                       (second-byte (+ first-byte
                                       (string-bytes org-files-db-test--actions-first)
                                       (string-bytes "body\n"))))
                  (dolist (case `((heading :byte-start ,first-byte :line 3)
                                  (heading :byte-start ,second-byte :line 5)
                                  (heading :byte-start 0 :line 5)
                                  (heading :line 4)
                                  (heading)
                                  (link :byte-start ,second-byte :line 5)
                                  (file)
                                  (root)))
                    (let* ((kind (car case))
                           (slots (cdr case))
                           (line (plist-get slots :line))
                           (byte-start (plist-get slots :byte-start))
                           (record (apply #'org-files-db-test--actions-record
                                          kind file slots)))
                      (setq messages nil)
                      (expect (org-files-db-actions-open-result record) :to-be nil)
                      (expect (buffer-file-name) :to-equal file)
                      (cond
                       ((or line byte-start)
                        (expect (line-number-at-pos) :to-equal line)
                        (when (and byte-start (> byte-start 0))
                          (expect (position-bytes (point)) :to-equal (1+ byte-start))))
                       (t (expect (point) :to-equal (point-min))))
                      (expect messages :to-equal
                              (list (pcase kind
                                      ('heading "Heading opened")
                                      ('link "Link opened")
                                      (_ "File opened"))))))))

            (it "opens a resolved link target at a heading or at the start of a file"
                (let ((byte (string-bytes org-files-db-test--actions-prefix)))
                  (dolist (case `((:target-line 3 :target-byte-start ,byte :line 3)
                                  (:target-line 1 :target-byte-start nil :line 1)))
                    (let ((record (org-files-db-test--actions-record
                                   'link file
                                   :line 9 :byte-start 0
                                   :target-file other
                                   :target-line (plist-get case :target-line)
                                   :target-byte-start (plist-get case :target-byte-start))))
                      (setq messages nil)
                      (expect (org-files-db-actions-open-link-target record) :to-be nil)
                      (expect (buffer-file-name) :to-equal other)
                      (expect (line-number-at-pos) :to-equal (plist-get case :line))
                      (expect messages :to-equal '("Link target opened"))))))

            (it "rejects link targets that cannot be opened"
                (dolist (case `((,(org-files-db-test--actions-record 'heading file :line 3)
                                 . "Result is not a link")
                                (,(org-files-db-test--actions-record 'link file :line 3)
                                 . "Link target is not resolved")))
                  (expect (org-files-db-actions-open-link-target (car case))
                          :to-throw 'user-error (list (cdr case))))
                (expect messages :to-equal nil))

            (it "inserts file and heading links built from a reloaded result"
                (let* ((org-files-db-configs (org-files-db-test--actions-configs))
                       (org-files-db-default-config "main")
                       (corpus (file-name-as-directory
                                org-files-db-test--actions-directory)))
                  (dolist (case `((org-files-db-actions-insert-file-link file
                                                                         "query-json-file-by-id.json" 2 "target.org"
                                                                         "[[file:target.org][Target]]" "File link inserted")
                                  (org-files-db-actions-insert-heading-link heading
                                                                            "query-json-headings-by-ids.json" 2 "notes.org"
                                                                            "[[id:a1b2c3d4-0000-4000-8000-000000000001][Grüße aus Zürich]]"
                                                                            "Heading link inserted")
                                  (org-files-db-actions-insert-heading-link heading
                                                                            "query-json-headings-by-ids.json" 3 "notes.org"
                                                                            "[[file:notes.org::#plain][Plain heading]]"
                                                                            "Heading link inserted")
                                  (org-files-db-actions-insert-heading-link heading
                                                                            "query-json-headings-by-ids.json" 4 "notes.org"
                                                                            "[[file:notes.org::*No ids][No ids]]"
                                                                            "Heading link inserted")))
                    (pcase-let* ((`(,action ,kind ,fixture ,id ,name ,expected ,text) case)
                                 (stdout (org-files-db-test--corpus-fixture fixture corpus))
                                 (record (org-files-db-presentation--make-record
                                          :kind kind :id id
                                          :file (expand-file-name name corpus)))
                                 (presentation (org-files-db-test--single-result-presentation
                                                record "main"))
                                 (org-files-db-actions--current-presentation presentation))
                      (setq messages nil)
                      (spy-on 'org-files-db-process--run-process :and-return-value
                              (list :status 0 :stdout stdout))
                      (with-temp-buffer
                        (setq default-directory corpus)
                        (set-visited-file-name (expand-file-name "here.org" corpus) t)
                        (expect (funcall action record) :to-be nil)
                        (expect (buffer-string) :to-equal expected)
                        (set-buffer-modified-p nil))
                      (expect messages :to-equal (list text))
                      (expect (member "--expect-generation"
                                      (car (spy-calls-args-for
                                            'org-files-db-process--run-process 0)))
                              :to-be-truthy)))))

            (it "inserts an absolute file link outside a file buffer"
                (spy-on 'org-files-db-process--run-process :and-return-value
                        (list :status 0
                              :stdout (org-files-db-test--corpus-fixture
                                       "query-json-file-by-id.json"
                                       (file-name-as-directory
                                        org-files-db-test--actions-directory))))
                (let* ((org-files-db-configs (org-files-db-test--actions-configs))
                       (org-files-db-default-config "main")
                       (record (org-files-db-presentation--make-record
                                :kind 'file :id 2 :file file))
                       (org-files-db-actions--current-presentation
                        (org-files-db-test--single-result-presentation record "main")))
                  (with-temp-buffer
                    (org-files-db-actions-insert-file-link record)
                    (expect (buffer-string) :to-equal
                            (format "[[file:%s][Target]]" (abbreviate-file-name file))))))

            (it "rejects link insertion for the wrong kind, without context and on stale index"
                (let* ((org-files-db-configs (org-files-db-test--actions-configs))
                       (org-files-db-default-config "main")
                       (heading (org-files-db-test--actions-record 'heading file))
                       (file-record (org-files-db-test--actions-record 'file file))
                       (org-files-db-actions--current-presentation nil))
                  (expect (org-files-db-actions-insert-file-link heading)
                          :to-throw 'user-error '("Result is not a file"))
                  (expect (org-files-db-actions-insert-heading-link file-record)
                          :to-throw 'user-error '("Result is not a heading"))
                  (expect (org-files-db-actions-insert-file-link file-record)
                          :to-throw 'user-error '("No query context for this action"))
                  (let ((org-files-db-actions--current-presentation
                         (org-files-db-test--single-result-presentation heading "main")))
                    (spy-on 'org-files-db-process--run-process :and-return-value
                            (list :status 1 :stdout ""
                                  :stderr (org-files-db-test--fixture-text
                                           "error-stale-index.stderr")))
                    (with-temp-buffer
                      (insert "x")
                      (dolist (case (list (cons #'org-files-db-actions-insert-file-link
                                                file-record)
                                          (cons #'org-files-db-actions-insert-heading-link
                                                heading)))
                        (expect (funcall (car case) (cdr case))
                                :to-throw 'user-error '("Index changed, run the query again")))
                      (expect (buffer-string) :to-equal "x")))))

            (it "binds the presentation around the query action"
                (let* ((record (org-files-db-test--actions-record 'heading file :line 3))
                       (presentation
                        (org-files-db-test--single-result-presentation record "main"))
                       seen)
                  (expect (org-files-db-current-presentation) :to-be nil)
                  (cl-letf (((symbol-function 'completing-read)
                             #'org-files-db-test--select-first-candidate))
                    (org-files-db-query--run-presentation-action
                     presentation 'headings
                     (lambda (_record) (setq seen (org-files-db-current-presentation)))))
                  (expect seen :to-be presentation)))

            (it "follows the first link of a heading without starting a process"
                (with-temp-file (expand-file-name "target.org"
                                                  org-files-db-test--actions-directory)
                  (insert "* Target\n"))
                (dolist (case '(("one.org" . "* [[file:target.org][Target]]\n")
                                ("two.org" . "* Plain\nsome text [[file:target.org][T]]\n** Next [[file:other.org]]\n")))
                  (let ((source (expand-file-name (car case)
                                                  org-files-db-test--actions-directory))
                        (body (cdr case))
                        (org-link-frame-setup '((file . find-file)))
                        (record nil))
                    (with-temp-file source (insert "intro\n" body))
                    (setq record (org-files-db-test--actions-record
                                  'heading source :line 2)
                          messages nil)
                    (expect (org-files-db-actions-follow-heading-link record) :to-be nil)
                    (expect (file-name-nondirectory (buffer-file-name))
                            :to-equal "target.org")
                    (expect 'org-files-db-process--run-process :not :to-have-been-called))))

            (it "signals when the heading has no link or is not a heading"
                (let ((source (expand-file-name "source.org"
                                                org-files-db-test--actions-directory)))
                  (with-temp-file source
                    (insert "* Plain\ntext\n** Next [[file:other.org]]\n"))
                  (expect (org-files-db-actions-follow-heading-link
                           (org-files-db-test--actions-record 'heading source :line 1))
                          :to-throw 'user-error '("Heading has no link"))
                  (expect (org-files-db-actions-follow-heading-link
                           (org-files-db-test--actions-record 'file source))
                          :to-throw 'user-error '("Result is not a heading"))))

            (it "runs the default open action from a query"
                (let* ((record (org-files-db-test--actions-record
                                'heading file
                                :line 3
                                :byte-start (string-bytes org-files-db-test--actions-prefix)))
                       (presentation
                        (org-files-db-test--single-result-presentation record "main")))
                  (cl-letf (((symbol-function 'org-files-db-query-results)
                             (lambda (&rest _args) presentation))
                            ((symbol-function 'completing-read)
                             #'org-files-db-test--select-first-candidate))
                    (expect (org-files-db-query '(headings)) :to-be record)
                    (expect (buffer-file-name) :to-equal file)
                    (expect (line-number-at-pos) :to-equal 3)
                    (expect messages :to-equal '("Heading opened")))))

            (describe "renaming a file"
                      (let (corpus notes target renamed json record)
                        (before-each
                         (setq corpus (expand-file-name
                                       "corpus" org-files-db-test--actions-directory)
                               notes (expand-file-name "notes.org" corpus)
                               target (expand-file-name "target.org" corpus)
                               renamed (expand-file-name "renamed.org" corpus)
                               json (org-files-db-test--corpus-fixture
                                     "query-json-incoming-links.json" corpus)
                               record (org-files-db-presentation--make-record
                                       :kind 'file :id 2 :file target))
                         (copy-directory (expand-file-name "corpus"
                                                           org-files-db-test--fixture-directory)
                                         corpus)
                         (spy-on 'org-files-db-process--run-process :and-return-value
                                 (list :status 0 :stdout json)))

                        (cl-flet ((rename (&optional (rec record))
                                    (let* ((org-files-db-configs
                                            (org-files-db-test--actions-configs))
                                           (org-files-db-default-config "main")
                                           (org-files-db-actions--current-presentation
                                            (org-files-db-test--single-result-presentation
                                             rec "main")))
                                      (org-files-db-actions-rename-file rec renamed)))
                                  (text (file)
                                    (with-temp-buffer
                                      (insert-file-contents file)
                                      (buffer-string))))

                          (it "renames the file and rewrites relative incoming links"
                              (expect (rename) :to-be nil)
                              (expect (file-exists-p target) :to-be nil)
                              (expect (file-exists-p renamed) :to-be t)
                              (expect (text notes) :to-match
                                      (regexp-quote "[[file:renamed.org][the target file]]"))
                              (expect (text notes) :to-match
                                      (regexp-quote "[[file:renamed.org::*Target heading][target heading]]"))
                              (expect (text notes) :not :to-match "target\\.org")
                              (expect messages :to-equal
                                      '("File renamed, 2 links updated"))
                              (let ((call (car (spy-calls-args-for
                                                'org-files-db-process--run-process 0))))
                                (expect (member "(links (target (files (id 2))))" call)
                                        :to-be-truthy)
                                (expect (member "--expect-generation" call)
                                        :to-be-truthy)))

                          (it "retargets a visiting buffer without marking it modified"
                              (let ((buffer (find-file-noselect target)))
                                (rename)
                                (expect (buffer-file-name buffer) :to-equal renamed)
                                (expect (buffer-modified-p buffer) :to-be nil)))

                          (it "keeps absolute links absolute"
                              (let* ((relative "[[file:target.org][the target file]]")
                                     (absolute (format "[[file:%s][the target file]]" target)))
                                (with-temp-file notes
                                  (insert (string-replace relative absolute
                                                          (text notes))))
                                (spy-on 'org-files-db-process--run-process
                                        :and-return-value
                                        (list :status 0
                                              :stdout
                                              (string-replace
                                               "\"[[file:target.org][the target file]]\",\n      \"raw_target\": \"file:target.org\",\n      \"raw_description\": \"the target file\",\n      \"link_path\": \"target.org\""
                                               (format "\"%s\",\n      \"raw_target\": \"file:target.org\",\n      \"raw_description\": \"the target file\",\n      \"link_path\": \"%s\""
                                                       (substring (json-serialize absolute) 1 -1)
                                                       (substring (json-serialize target) 1 -1))
                                               (let ((delta (- (string-bytes absolute)
                                                               (string-bytes relative))))
                                                 (dolist (offset '(165 225 277) json)
                                                   (setq json
                                                         (string-replace
                                                          (format ": %d" offset)
                                                          (format ": %d" (+ offset delta))
                                                          json)))))))
                                (rename)
                                (expect (text notes) :to-match
                                        (regexp-quote
                                         (format "[[file:%s][the target file]]" renamed)))
                                (expect (text notes) :to-match
                                        (regexp-quote "[[file:renamed.org::*Target heading]"))))

                          (it "skips links whose text no longer matches the index"
                              (let ((edited (string-replace "the target file" "the TARGET file"
                                                            (text notes))))
                                (with-temp-file notes (insert edited))
                                (rename)
                                (expect (text notes) :to-match
                                        (regexp-quote "[[file:target.org][the TARGET file]]"))
                                (expect (text notes) :to-match
                                        (regexp-quote "[[file:renamed.org::*Target heading]"))
                                (expect messages :to-equal
                                        '("File renamed, 1 links updated, 1 skipped"))))

                          (it "refuses an existing target and changes nothing"
                              (with-temp-file renamed (insert "x"))
                              (let ((before (text notes)))
                                (expect (rename) :to-throw 'user-error)
                                (expect (file-exists-p target) :to-be t)
                                (expect (text renamed) :to-equal "x")
                                (expect (text notes) :to-equal before)))

                          (it "changes nothing when the index is stale"
                              (spy-on 'org-files-db-process--run-process
                                      :and-return-value
                                      (list :status 1 :stdout ""
                                            :stderr (org-files-db-test--fixture-text
                                                     "error-stale-index.stderr")))
                              (let ((before (text notes)))
                                (expect (rename) :to-throw 'user-error
                                        '("Index changed, run the query again"))
                                (expect (file-exists-p target) :to-be t)
                                (expect (file-exists-p renamed) :to-be nil)
                                (expect (text notes) :to-equal before)))

                          (it "rejects records that are not files"
                              (expect (rename (org-files-db-presentation--make-record
                                               :kind 'heading :id 2 :file target))
                                      :to-throw 'user-error '("Result is not a file"))
                              (expect (file-exists-p target) :to-be t)))))))

(describe "Embark integration"
          (let ((cases
                 '((org-files-db-embark-open-result org-files-db-actions-open-result "o")
                   (org-files-db-embark-open-link-target org-files-db-actions-open-link-target "t")
                   (org-files-db-embark-insert-file-link org-files-db-actions-insert-file-link "f")
                   (org-files-db-embark-insert-heading-link org-files-db-actions-insert-heading-link "h")
                   (org-files-db-embark-follow-heading-link org-files-db-actions-follow-heading-link "l")
                   (org-files-db-embark-rename-file org-files-db-actions-rename-file "r"))))

            (it "loads the package without loading embark"
                (let ((emacs (expand-file-name invocation-name invocation-directory))
                      (lisp (file-name-directory (locate-library "org-files-db"))))
                  (unless (file-executable-p emacs)
                    (buttercup-skip "No Emacs executable available"))
                  (expect (call-process
                           emacs nil nil nil "-Q" "--batch" "-L" lisp
                           "-l" "org-files-db"
                           "--eval" "(when (featurep 'embark) (kill-emacs 1))")
                          :to-equal 0)))

            (it "registers the keymap in embark-keymap-alist"
                (unless (require 'embark nil t)
                  (buttercup-skip "Embark is not available"))
                (expect (cdr (assq 'org-files-db-result embark-keymap-alist))
                        :to-be 'org-files-db-embark-result-map))

            (it "binds each key to its Embark command"
                (dolist (case cases)
                  (expect (lookup-key org-files-db-embark-result-map (nth 2 case))
                          :to-be (nth 0 case))))

            (it "dispatches the candidate record with the presentation bound"
                (dolist (case cases)
                  (let* ((result (list (cons 'kind "heading") (cons 'id (nth 2 case))))
                         (presentation
                          (org-files-db-test--single-result-presentation result "work"))
                         (candidate
                          (car (org-files-db-presentation--candidates presentation)))
                         seen)
                    (spy-on (nth 1 case) :and-call-fake
                            (lambda (record &rest _)
                              (setq seen (list record
                                               org-files-db-actions--current-presentation
                                               org-files-db-actions--current-action-config))))
                    (funcall (nth 0 case) candidate)
                    (expect seen :to-equal (list result presentation "work")))))

            (it "resolves a candidate without properties by identity"
                (let* ((result '((kind . "heading") (id . 1)))
                       (presentation
                        (org-files-db-test--single-result-presentation result "work"))
                       (candidate (substring-no-properties
                                   (car (org-files-db-presentation--candidates presentation))))
                       seen)
                  (spy-on 'org-files-db-actions-open-result :and-call-fake
                          (lambda (record) (setq seen (list record
                                                            org-files-db-actions--current-presentation))))
                  (let ((org-files-db-presentation--current-read-presentation presentation))
                    (org-files-db-embark-open-result candidate))
                  (expect seen :to-equal (list result presentation))))

            (it "signals a user error without any presentation"
                (expect (org-files-db-embark-open-result "plain") :to-throw 'user-error))

            (it "sets the result category in completion metadata"
                (let* ((table (org-files-db-presentation--completion-table nil))
                       (metadata (funcall table "" nil 'metadata)))
                  (expect (cdr (assq 'category (cdr metadata)))
                          :to-be 'org-files-db-result)))

            (describe "export"
                      (let (presentation candidates)
                        (before-each
                         (let ((row
                                (lambda (index text)
                                  (org-files-db-presentation--make-presentation-row
                                   :result-index index
                                   :row-context nil
                                   :cells
                                   (vector
                                    (org-files-db-presentation--make-presentation-cell
                                     :search-text text
                                     :display-text text
                                     :role 'title))))))
                           (setq presentation
                                 (org-files-db-presentation--make-presentation
                                  :version 3 :database-id "db" :generation 1
                                  :config "work" :schemas nil
                                  :results
                                  (vector
                                   (org-files-db-presentation--make-record
                                    :kind 'heading :id 1 :file "/a.org" :line 1)
                                   (org-files-db-presentation--make-record
                                    :kind 'file :id 2 :file "/b.org" :line 1))
                                  :rows
                                  (vector (funcall row 0 "First tag a")
                                          (funcall row 0 "First tag b")
                                          (funcall row 1 "Second"))))
                           (setq candidates
                                 (org-files-db-presentation--candidates presentation))))

                        (after-each
                         (when (get-buffer "*org-files-db export*")
                           (kill-buffer "*org-files-db export*")))

                        (it "lists one line per filtered candidate in order"
                            (with-current-buffer
                                (org-files-db-embark-export
                                 (list (nth 2 candidates) (nth 1 candidates)
                                       (nth 0 candidates)))
                              (expect major-mode :to-be 'org-files-db-export-mode)
                              (expect buffer-read-only :to-be t)
                              (expect (split-string (buffer-string) "\n" t)
                                      :to-equal '("Second" "First tag b" "First tag a"))))

                        (it "keeps rows of one result as separate lines with the same record"
                            (with-current-buffer
                                (org-files-db-embark-export (list (nth 0 candidates)
                                                                  (nth 1 candidates)))
                              (let ((first (get-text-property 1 'org-files-db-result))
                                    (second (progn (forward-line 1)
                                                   (get-text-property
                                                    (point) 'org-files-db-result))))
                                (expect (count-lines (point-min) (point-max)) :to-be 2)
                                (expect first :to-be second)
                                (expect (get-text-property (point) 'org-files-db-candidate)
                                        :to-equal (nth 1 candidates))
                                (expect (get-text-property (point) 'org-files-db-presentation)
                                        :to-be presentation))))

                        (it "runs the default action for the record kind on RET"
                            (let (seen)
                              (spy-on 'org-files-db-actions-open-result
                                      :and-call-fake
                                      (lambda (record)
                                        (push (list record
                                                    org-files-db-actions--current-presentation
                                                    org-files-db-actions--current-action-config)
                                              seen)))
                              (let ((org-files-db-heading-action #'org-files-db-actions-open-result)
                                    (org-files-db-file-action #'org-files-db-actions-open-result))
                                (with-current-buffer
                                    (org-files-db-embark-export (list (nth 1 candidates)
                                                                      (nth 2 candidates)))
                                  (execute-kbd-macro (kbd "RET"))
                                  (execute-kbd-macro (kbd "n"))
                                  (execute-kbd-macro (kbd "RET"))))
                              (expect (mapcar #'car (reverse seen))
                                      :to-equal
                                      (list (aref (org-files-db-presentation-results presentation) 0)
                                            (aref (org-files-db-presentation-results presentation) 1)))
                              (expect (nth 1 (car seen)) :to-be presentation)
                              (expect (nth 2 (car seen)) :to-equal "work")))

                        (it "finds the candidate at point as Embark target"
                            (with-current-buffer
                                (org-files-db-embark-export (list (nth 0 candidates)
                                                                  (nth 2 candidates)))
                              (forward-line 1)
                              (forward-char 2)
                              (let ((target (org-files-db-embark--export-target)))
                                (expect (car target) :to-be 'org-files-db-result)
                                (expect (cadr target) :to-equal (nth 2 candidates))
                                (expect (caddr target) :to-be (line-beginning-position))
                                (expect (cdddr target) :to-be (line-end-position)))))

                        (it "finds no target outside the export mode"
                            (with-temp-buffer
                              (expect (org-files-db-embark--export-target) :to-be nil)))

                        (it "registers the exporter and target finder"
                            (unless (require 'embark nil t)
                              (buttercup-skip "Embark is not available"))
                            (expect (cdr (assq 'org-files-db-result embark-exporters-alist))
                                    :to-be 'org-files-db-embark-export)
                            (expect (memq #'org-files-db-embark--export-target
                                          embark-target-finders)
                                    :to-be-truthy))))))

(defun org-files-db-test--outline-trim (node matched)
  "Return outline NODE with MATCHED flags set and unmatched leaves removed.
Return nil when nothing below NODE is matched and NODE is not a root."
  (let* ((children (delq nil
                         (mapcar (lambda (child)
                                   (org-files-db-test--outline-trim child matched))
                                 (append (alist-get 'children node) nil))))
         (is-matched (and (memq (alist-get 'id node) matched) t)))
    (when (or children is-matched (eq (alist-get 'kind node) 'root))
      (setf (alist-get 'children node) (vconcat children))
      (setf (alist-get 'matched node) (if is-matched t :false))
      node)))

(defun org-files-db-test--outline-json (corpus matched)
  "Return the outline fixture for CORPUS with only the ids MATCHED matched."
  (let ((data (json-parse-string
               (org-files-db-test--corpus-fixture
                "query-json-outline-by-ids.json" corpus)
               :object-type 'alist)))
    (setf (alist-get 'results data)
          (vconcat (mapcar (lambda (root)
                             (org-files-db-test--outline-trim root matched))
                           (alist-get 'results data))))
    (json-serialize data)))

(defmacro org-files-db-test--outline-options (options &rest body)
  "Run BODY with the outline export OPTIONS plist bound."
  (declare (indent 1))
  `(let ((org-files-db-outline-export-ancestors
          (if (plist-member ,options :ancestors) (plist-get ,options :ancestors) t))
         (org-files-db-outline-export-children (plist-get ,options :children))
         (org-files-db-outline-export-planning
          (if (plist-member ,options :planning) (plist-get ,options :planning) t))
         (org-files-db-outline-export-properties (plist-get ,options :properties))
         (org-files-db-outline-export-body (plist-get ,options :body)))
     ,@body))

(describe "outline export"
          (let (corpus file)
            (before-each
             (setq org-files-db-test--actions-directory
                   (make-temp-file "org-files-db-outline-" t)
                   corpus (file-name-as-directory org-files-db-test--actions-directory)
                   file (expand-file-name "outline.org" corpus))
             (copy-file (expand-file-name "corpus/outline.org"
                                          org-files-db-test--fixture-directory)
                        file)
             (spy-on 'org-files-db-process--run-process :and-throw-error 'error))

            (after-each
             (dolist (buffer (buffer-list))
               (when-let* ((name (buffer-file-name buffer)))
                 (when (string-prefix-p org-files-db-test--actions-directory name)
                   (with-current-buffer buffer (set-buffer-modified-p nil))
                   (kill-buffer buffer))))
             (when (get-buffer "*org-files-db outline*")
               (kill-buffer "*org-files-db outline*"))
             (delete-directory org-files-db-test--actions-directory t))

            (cl-flet*
                ((record (id &optional (kind 'heading))
                   (org-files-db-presentation--make-record
                    :kind kind :id id :file file))
                 (export (matched options &optional records)
                   (spy-on 'org-files-db-process--run-process :and-return-value
                           (list :status 0
                                 :stdout (org-files-db-test--outline-json corpus matched)))
                   (let* ((org-files-db-configs (org-files-db-test--actions-configs))
                          (org-files-db-default-config "main")
                          (records (or records (mapcar #'record matched)))
                          (presentation (org-files-db-test--single-result-presentation
                                         (car records) "main")))
                     (org-files-db-test--outline-options options
                                                         (with-current-buffer
                                                             (org-files-db-outline-export records presentation)
                                                           (buffer-substring-no-properties (point-min) (point-max))))))
                 (expected (text)
                   (string-replace "@P@" (abbreviate-file-name file) text)))

              (it "exports the matched headings under their shared ancestors"
                  (expect (export '(8 9 11) nil)
                          :to-equal
                          (expected "* [[file:@P@][Outline]]
** [[file:@P@::#parent][Parent]] :proj:
*** TODO [[id:b1b2c3d4-0000-4000-8000-000000000002][Child one]] :work:
SCHEDULED: <2026-10-02 Fri>
**** [[file:@P@::14][Grandchild]]
*** [[file:@P@::16][Child two]]
** [[file:@P@::17][Other]]
*** [[file:@P@::18][Match in other]]
"))
                  (let ((arguments (car (spy-calls-args-for
                                         'org-files-db-process--run-process 0))))
                    (expect arguments :to-contain "--output")
                    (expect (cadr (member "--output" arguments)) :to-equal "outline")
                    (expect arguments :to-contain "--expect-generation")
                    (expect (car (last arguments)) :to-equal "(headings (id 8 9 11))")))

              (it "lists only the matched headings flat without ancestors"
                  (expect (export '(8 9 11) '(:ancestors nil))
                          :to-equal
                          (expected "* [[file:@P@][Outline]]
** [[file:@P@::14][Grandchild]]
** [[file:@P@::16][Child two]]
** [[file:@P@::18][Match in other]]
")))

              (it "adds children or the subtree of matched headings as links"
                  (dolist (case
                           `((nil . "* [[file:@P@][Outline]]
** [[file:@P@::#parent][Parent]] :proj:
")
                             (children . "* [[file:@P@][Outline]]
** [[file:@P@::#parent][Parent]] :proj:
*** TODO [[id:b1b2c3d4-0000-4000-8000-000000000002][Child one]] :work:
*** [[file:@P@::16][Child two]]
")
                             (subtree . "* [[file:@P@][Outline]]
** [[file:@P@::#parent][Parent]] :proj:
*** TODO [[id:b1b2c3d4-0000-4000-8000-000000000002][Child one]] :work:
**** [[file:@P@::14][Grandchild]]
*** [[file:@P@::16][Child two]]
")))
                    (expect (export '(6) (list :children (car case)))
                            :to-equal (expected (cdr case)))))

              (it "adds children below flat matched headings and skips other matches"
                  (expect (export '(6 7) '(:ancestors nil :children subtree))
                          :to-equal
                          (expected "* [[file:@P@][Outline]]
** [[file:@P@::#parent][Parent]] :proj:
*** [[file:@P@::16][Child two]]
** TODO [[id:b1b2c3d4-0000-4000-8000-000000000002][Child one]] :work:
SCHEDULED: <2026-10-02 Fri>
*** [[file:@P@::14][Grandchild]]
")))

              (it "copies planning, properties and body of matched headings only"
                  (let ((child "*** TODO [[id:b1b2c3d4-0000-4000-8000-000000000002][Child one]] :work:\n")
                        (planning "SCHEDULED: <2026-10-02 Fri>\n")
                        (properties ":PROPERTIES:\n:ID:       b1b2c3d4-0000-4000-8000-000000000002\n:END:\n")
                        (body "Child body.\n"))
                    (dolist (case `((nil ,(concat child planning))
                                    ((:planning nil) ,child)
                                    ((:properties t) ,(concat child planning properties))
                                    ((:body t) ,(concat child planning body))
                                    ((:planning nil :properties t :body t)
                                     ,(concat child properties body))))
                      (expect (export '(7) (car case))
                              :to-equal
                              (expected (concat "* [[file:@P@][Outline]]\n"
                                                "** [[file:@P@::#parent][Parent]] :proj:\n"
                                                (cadr case)))))))

              (it "marks matched headline lines with the match face only"
                  (export '(8 9 11) nil)
                  (with-current-buffer "*org-files-db outline*"
                    (let (marked plain)
                      (goto-char (point-min))
                      (while (not (eobp))
                        (let ((faces (delq nil
                                           (mapcar (lambda (overlay)
                                                     (overlay-get overlay 'face))
                                                   (overlays-at (point)))))
                              (line (buffer-substring-no-properties
                                     (line-beginning-position) (line-end-position))))
                          (if (memq 'org-files-db-outline-match faces)
                              (push line marked)
                            (push line plain)))
                        (forward-line 1))
                      (expect (length marked) :to-be 3)
                      (expect (cl-every (lambda (line)
                                          (string-match-p "Grandchild\\|Child two\\|Match in other" line))
                                        marked)
                              :to-be-truthy)
                      (expect (cl-some (lambda (line)
                                         (string-match-p "Grandchild\\|Child two\\|Match in other" line))
                                       plain)
                              :to-be nil))))

              (it "shows a read-only outline buffer that saves without faces"
                  (export '(8 9 11) nil)
                  (with-current-buffer "*org-files-db outline*"
                    (let ((target (expand-file-name "saved.org" corpus)))
                      (expect major-mode :to-be 'org-mode)
                      (expect buffer-read-only :to-be t)
                      (expect (key-binding (kbd "C-x C-s"))
                              :to-be 'org-files-db-outline-export-save)
                      (expect (org-files-db-outline-export-save target) :to-equal target)
                      (with-temp-buffer
                        (insert-file-contents target)
                        (expect (buffer-string) :to-equal
                                (buffer-substring-no-properties
                                 (with-current-buffer "*org-files-db outline*" (point-min))
                                 (with-current-buffer "*org-files-db outline*" (point-max))))
                        (expect (next-single-property-change (point-min) 'face nil (point-max))
                                :to-equal (point-max))))))

              (it "rejects results that are not headings without exporting"
                  (dolist (kind '(file link root))
                    (expect (export '(8) nil (list (record 8 kind)))
                            :to-throw 'user-error '("Outline export needs heading results")))
                  (expect 'org-files-db-process--run-process :not :to-have-been-called)
                  (expect (get-buffer "*org-files-db outline*") :to-be nil))

              (it "reports a stale index and exports nothing"
                  (spy-on 'org-files-db-process--run-process :and-return-value
                          (list :status 1 :stdout ""
                                :stderr (org-files-db-test--fixture-text
                                         "error-stale-index.stderr")))
                  (let ((org-files-db-configs (org-files-db-test--actions-configs))
                        (org-files-db-default-config "main"))
                    (expect (org-files-db-outline-export
                             (list (record 8))
                             (org-files-db-test--single-result-presentation
                              (record 8) "main"))
                            :to-throw 'user-error '("Index changed, run the query again")))
                  (expect (get-buffer "*org-files-db outline*") :to-be nil))

              (it "reports a changed source file and exports nothing"
                  (dolist (change '(("* Parent" . "* Renamed")
                                    ("#+title: Outline\n" . "#+title: Outline\n\n")
                                    ("** Match in other" . "*** Match in other")))
                    (with-temp-file file
                      (insert (string-replace
                               (car change) (cdr change)
                               (with-temp-buffer
                                 (insert-file-contents
                                  (expand-file-name "corpus/outline.org"
                                                    org-files-db-test--fixture-directory))
                                 (buffer-string)))))
                    (expect (export '(8 9 11) nil)
                            :to-throw 'user-error '("Index changed, run the query again"))
                    (expect (get-buffer "*org-files-db outline*") :to-be nil)))

              (it "reports a missing source file"
                  (delete-file file)
                  (expect (export '(8 9 11) nil)
                          :to-throw 'user-error '("Index changed, run the query again"))))))

(describe "outline export through Embark"
          (let (presentation candidates)
            (before-each
             (let ((cell (lambda (text)
                           (org-files-db-presentation--make-presentation-cell
                            :search-text text :display-text text :role 'title))))
               (setq presentation
                     (org-files-db-presentation--make-presentation
                      :version 3 :database-id "db" :generation 1
                      :config "work" :schemas nil
                      :results (vector
                                (org-files-db-presentation--make-record
                                 :kind 'heading :id 1 :file "/a.org" :line 1)
                                (org-files-db-presentation--make-record
                                 :kind 'heading :id 2 :file "/a.org" :line 2))
                      :rows (vector
                             (org-files-db-presentation--make-presentation-row
                              :result-index 0 :cells (vector (funcall cell "One")))
                             (org-files-db-presentation--make-presentation-row
                              :result-index 1 :cells (vector (funcall cell "Two"))))))
               (setq candidates (org-files-db-presentation--candidates presentation)))
             (spy-on 'org-files-db-outline-export :and-return-value 'buffer))

            (after-each
             (when (get-buffer "*org-files-db export*")
               (kill-buffer "*org-files-db export*")))

            (it "exports the candidates as an outline"
                (expect (org-files-db-embark-export-outline (reverse candidates))
                        :to-be 'buffer)
                (expect (spy-calls-args-for 'org-files-db-outline-export 0)
                        :to-equal
                        (list (list (aref (org-files-db-presentation-results presentation) 1)
                                    (aref (org-files-db-presentation-results presentation) 0))
                              presentation)))

            (it "converts the rows of the flat export buffer with o"
                (with-current-buffer (org-files-db-embark-export candidates)
                  (expect (key-binding (kbd "o")) :to-be 'org-files-db-embark-export-outline)
                  (execute-kbd-macro (kbd "o")))
                (expect (length (car (spy-calls-args-for 'org-files-db-outline-export 0)))
                        :to-be 2))

            (it "rejects other buffers and empty candidates"
                (with-temp-buffer
                  (expect (org-files-db-embark--outline-candidates)
                          :to-throw 'user-error))
                (expect (org-files-db-embark-export-outline nil)
                        :to-throw 'user-error))))

(provide 'org-files-db-test)

;;; org-files-db-test.el ends here
