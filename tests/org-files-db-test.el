;;; org-files-db-test.el --- Tests for org-files-db -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-only

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'org-files-db)
(require 'org-files-db-benchmark)

(defvar org-files-db-test--directory nil)
(defvar org-files-db-test--executable nil)

(defun org-files-db-test--write-file (name contents)
  "Write CONTENTS to NAME below the current test directory."
  (let ((file (expand-file-name name org-files-db-test--directory)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert contents))
    file))

(defun org-files-db-test--result (kind title file &optional line byte)
  "Return a result object with KIND, TITLE, FILE, LINE, and BYTE."
  `((kind . ,kind)
    (title . ,title)
    (location . ((file_path . ,file)
                 (line . ,(or line 1))
                 (byte_start . ,(or byte 0))))))

(defun org-files-db-test--candidate-visible (candidate)
  "Return the visible formatted portion of CANDIDATE."
  (substring-no-properties
   candidate 0
   (or (and (> (length candidate) 0)
            (get-text-property 0 'org-files-db-visible-length candidate))
       (length candidate))))

(describe "org-files-db"

(before-each
  (setq org-files-db-test--directory (make-temp-file "org-files-db-test-" t))
  (setq org-persist-directory
        (expand-file-name "org-persist/" org-files-db-test--directory))
  (setq org-files-db-test--executable
        (org-files-db-test--write-file
         "orgfdb"
         (concat
          "#!/bin/sh\n"
          "case \"$1\" in\n"
          "  --version) printf '%s\\n' 'orgfdb 0.1.0' ;;\n"
          "  query) printf '%s\\n' '{\"target\":\"headings\",\"output\":\"flat\",\"includes\":[\"path\"],\"results\":[{\"kind\":\"heading\",\"title\":\"Example\",\"level\":1,\"location\":{\"file_path\":\"/tmp/example.org\",\"line\":1,\"byte_start\":0}}]}' ;;\n"
          "  search) printf '%s\\n' '[{\"kind\":\"heading\",\"title\":\"Search result\",\"rank\":-1.0,\"location\":{\"file_path\":\"/tmp/search.org\",\"line\":2,\"byte_start\":3}}]' ;;\n"
          "  headings) printf '%s\\n' '[]' ;;\n"
          "  usage) printf '%s\\n' 'bad usage' >&2; exit 2 ;;\n"
          "  fail) printf '%s\\n' 'database unavailable' >&2; exit 1 ;;\n"
          "  *) printf '%s\\n' '[]' ;;\n"
          "esac\n")))
  (set-file-modes org-files-db-test--executable #o755)
  (setq org-files-db-executable org-files-db-test--executable
        org-files-db-config-file nil
        org-files-db-views nil
        org-files-db-export-layout 'flat
        org-files-db-export-linked-heading-style 'preserve))

(after-each
  (let ((root (and org-files-db-test--directory
                   (file-name-as-directory
                    (expand-file-name org-files-db-test--directory)))))
    (dolist (buffer (buffer-list))
      (let ((file (buffer-file-name buffer))
            (name (buffer-name buffer)))
        (when (or (and name
                       (string-prefix-p "*org-files-db export*" name))
                  (and root file
                       (string-prefix-p root (expand-file-name file))))
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))))
  (when (and org-files-db-test--directory
             (file-directory-p org-files-db-test--directory))
    (delete-directory org-files-db-test--directory t))
  (setq org-files-db-test--directory nil
        org-files-db-test--executable nil))

(describe "package loading"
  (it "provides the public package and modules"
    (expect (featurep 'org-files-db) :to-equal t)
    (expect (featurep 'org-files-db-query) :to-equal t)
    (expect (featurep 'org-files-db-search) :to-equal t)
    (expect (featurep 'org-files-db-export) :to-equal t)
    (expect (featurep 'org-files-db-actions) :to-equal t)
    (expect (featurep 'org-files-db-views) :to-equal t)
    (expect (featurep 'org-files-db-dblock) :to-equal t)))

(describe "API naming"
  (it "keeps the central query and search command names concise"
    (expect (fboundp 'org-files-db-query) :to-equal t)
    (expect (fboundp 'org-files-db-search) :to-equal t)
    (expect (fboundp 'org-files-db-query-run) :to-equal nil)
    (expect (fboundp 'org-files-db-search-run) :to-equal nil))

  (it "uses the actions module prefix for public actions"
    (dolist (function '(org-files-db-actions-open-result
                        org-files-db-actions-insert-file-link
                        org-files-db-actions-insert-heading-link
                        org-files-db-actions-query-insert-file-link
                        org-files-db-actions-query-insert-heading-link
                        org-files-db-actions-follow-heading-link
                        org-files-db-actions-rename-file
                        org-files-db-actions-rename-file-result
                        org-files-db-actions-backlinks))
      (expect (fboundp function) :to-equal t)))

  (it "uses the views module prefix for public view operations"
    (dolist (function '(org-files-db-views-get
                        org-files-db-views-query
                        org-files-db-views-search))
      (expect (fboundp function) :to-equal t)))

  (it "does not retain obsolete pre-release public names"
    (dolist (function '(org-files-db-open-result
                        org-files-db-insert-file-link
                        org-files-db-insert-heading-link
                        org-files-db-query-insert-file-link
                        org-files-db-query-insert-heading-link
                        org-files-db-follow-heading-link
                        org-files-db-rename-file
                        org-files-db-rename-file-result
                        org-files-db-backlinks
                        org-files-db-get-view
                        org-files-db-query-view
                        org-files-db-search-view
                        org-files-db-embark-export-org))
      (expect (fboundp function) :to-equal nil)))

  (it "retains Org dynamic-block writer callback names"
    (dolist (function '(org-dblock-write:org-files-db-query
                        org-dblock-write:org-files-db-search
                        org-dblock-write:org-files-db-backlinks))
      (expect (fboundp function) :to-equal t)))

  (it "uses the dynamic-block module prefix for insertion commands"
    (dolist (function '(org-files-db-dblock-insert-query
                        org-files-db-dblock-insert-search
                        org-files-db-dblock-insert-backlinks))
      (expect (fboundp function) :to-equal t)))

  (it "keeps core independent from specialized modules"
    (let* ((located (locate-library "org-files-db-core"))
           (source (if (and located (string-suffix-p ".elc" located))
                       (concat (file-name-sans-extension located) ".el")
                     located)))
      (expect (and source (file-readable-p source)) :to-equal t)
      (with-temp-buffer
        (insert-file-contents source)
        (goto-char (point-min))
        (expect (re-search-forward
                 "^(require 'org-files-db-[[:alnum:]-]+)" nil t)
                :to-equal nil))))

  (it "uses the owning module prefix for specialized definitions"
    (dolist (entry
             '(("org-files-db-actions" "org-files-db-actions-")
               ("org-files-db-views" "org-files-db-views-")
               ("org-files-db-query" "org-files-db-query--"
                org-files-db-query)
               ("org-files-db-search" "org-files-db-search--"
                org-files-db-search org-files-db-search-live)
               ("org-files-db-dblock" "org-files-db-dblock-"
                org-dblock-write:org-files-db-query
                org-dblock-write:org-files-db-search
                org-dblock-write:org-files-db-backlinks)
               ("org-files-db-export" "org-files-db-export-")))
      (let* ((library (car entry))
             (prefix (cadr entry))
             (exceptions (cddr entry))
             (located (locate-library library))
             (source (if (and located (string-suffix-p ".elc" located))
                         (concat (file-name-sans-extension located) ".el")
                       located)))
        (expect (and source (file-readable-p source)) :to-equal t)
        (with-temp-buffer
          (insert-file-contents source)
          (goto-char (point-min))
          (while (re-search-forward
                  "^(\\(?:cl-\\)?defun \\([^[:space:]()]+\\)"
                  nil t)
            (let ((name (match-string 1)))
              (expect (or (string-prefix-p prefix name)
                          (not (null (memq (intern name) exceptions))))
                      :to-equal t))))))))

(describe "default result action"
  (it "uses the actions-owned open-result function"
    (expect org-files-db-query-action
            :to-equal #'org-files-db-actions-open-result))

  (it "opens an original result object programmatically"
    (let* ((file (org-files-db-test--write-file "open.org" "* Open me\n"))
           (result (org-files-db-test--result "heading" "Open me" file)))
      (save-window-excursion
        (org-files-db-actions-open-result result)
        (expect (file-truename buffer-file-name)
                :to-equal (file-truename file)))))

  (it "accepts a propertized candidate through interactive invocation"
    (let* ((result '((kind . "heading") (title . "Candidate")))
           visited)
      (with-temp-buffer
        (insert (propertize "Candidate" 'org-files-db-result result))
        (goto-char (point-min))
        (cl-letf (((symbol-function 'org-files-db--visit-result)
                   (lambda (value) (setq visited value))))
          (call-interactively #'org-files-db-actions-open-result)))
      (expect visited :to-equal result))))

(describe "CLI foundation"
  (it "resolves an explicit executable"
    (expect (org-files-db--resolve-executable)
            :to-equal org-files-db-test--executable))

  (it "parses JSON from a successful command"
    (expect (org-files-db--call "headings" '("--format" "json"))
            :to-equal nil))

  (it "normalizes an empty response envelope"
    (expect (org-files-db--normalize-results
             '((target . "headings") (results . nil)))
            :to-equal nil))

  (it "uses compact vectors for nested production JSON arrays"
    (let* ((response
            (org-files-db--parse-json
             (concat
              "{\"results\":[{\"kind\":\"heading\","
              "\"title\":\"Child\",\"all_tags\":[\"one\",\"two\"],"
              "\"node_path\":[{\"kind\":\"heading\","
              "\"title\":\"Child\"}]}]}")))
           (results (org-files-db--normalize-results response))
           (result (car results)))
      (expect (listp results) :to-equal t)
      (expect (vectorp (org-files-db--get result 'all_tags)) :to-equal t)
      (expect (vectorp (org-files-db--get result 'node_path)) :to-equal t)
      (expect (org-files-db--column-value result '(tags))
              :to-equal "one,two")
      (expect (org-files-db--column-value result '(outline-path))
              :to-equal "Child")))

  (it "distinguishes present nil values from missing alist keys"
    (let ((result '((todo_keyword . nil)
                    ("priority" . "A"))))
      (expect (org-files-db--alist-value result 'todo_keyword) :to-equal nil)
      (expect (not (null (org-files-db--has-key-p result 'todo_keyword)))
              :to-equal t)
      (expect (org-files-db--alist-value result 'priority) :to-equal "A")
      (expect (org-files-db--has-key-p result 'missing) :to-equal nil)))

  (it "distinguishes present nil values from missing hash-table keys"
    (let ((result (make-hash-table :test #'equal)))
      (puthash 'todo_keyword nil result)
      (puthash "priority" "A" result)
      (expect (org-files-db--alist-value result 'todo_keyword) :to-equal nil)
      (expect (org-files-db--has-key-p result 'todo_keyword) :to-equal t)
      (expect (org-files-db--alist-value result 'priority) :to-equal "A")
      (expect (org-files-db--has-key-p result 'missing) :to-equal nil)))

  (it "distinguishes command and usage failures"
    (expect (org-files-db--call "fail" nil)
            :to-throw 'org-files-db-cli-error)
    (expect (org-files-db--call "usage" nil)
            :to-throw 'org-files-db-cli-usage-error))

  (it "adds the configured file as separate arguments"
    (let ((config (org-files-db-test--write-file "config.toml" "db_path='x'\n")))
      (setq org-files-db-config-file config)
      (expect (org-files-db--config-arguments)
              :to-equal (list "--config" config))))

  (it "resolves inherited, overridden, and explicitly disabled configurations"
    (let ((global (org-files-db-test--write-file
                   "global.toml" "db_path='global'\n"))
          (override (org-files-db-test--write-file
                     "override.toml" "db_path='override'\n")))
      (setq org-files-db-config-file global)
      (expect (org-files-db--resolve-config-file nil nil "Test")
              :to-equal global)
      (expect (org-files-db--resolve-config-file override t "Test")
              :to-equal override)
      (expect (org-files-db--resolve-config-file nil t "Test")
              :to-equal nil)
      (expect (org-files-db--config-arguments nil)
              :to-equal nil)))

  (it "expands relative configuration paths against default-directory"
    (let ((default-directory org-files-db-test--directory))
      (org-files-db-test--write-file "relative.toml" "db_path='x'\n")
      (expect (org-files-db--resolve-config-file "relative.toml" t "Test")
              :to-equal
              (expand-file-name "relative.toml" org-files-db-test--directory))))

  (it "identifies the command when configuration validation fails"
    (let (message)
      (condition-case err
          (org-files-db--resolve-config-file
           "missing.toml" t "Private query")
        (user-error (setq message (error-message-string err))))
      (expect message :to-match "Private query configuration file")
      (expect message :to-match "does not exist")))

  (it "runs the setup diagnostic without mutating the database"
    (let ((report (org-files-db-check-setup)))
      (expect (alist-get 'version report) :to-equal "orgfdb 0.1.0")
      (expect (alist-get 'read-check report) :to-equal "ok")))

  (it "lets the setup diagnostic override or disable the global configuration"
    (let ((global (org-files-db-test--write-file
                   "setup-global.toml" "db_path='global'\n"))
          (override (org-files-db-test--write-file
                     "setup-override.toml" "db_path='override'\n")))
      (setq org-files-db-config-file global)
      (expect (alist-get 'config
                         (org-files-db-check-setup
                          :config-file override))
              :to-equal override)
      (expect (alist-get 'config
                         (org-files-db-check-setup
                          :config-file nil))
              :to-equal nil))))

(describe "query and search arguments"
  (it "serializes query forms"
    (expect (org-files-db--query-string '(headings (not (done))))
            :to-equal "(headings (not (done)))"))

  (it "disables read-time evaluation for interactive query input"
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) "#.(error \"unsafe\")")))
      (expect (org-files-db--read-sexp "Query: ")
              :to-throw 'user-error)))

  (it "keeps flat output and combines requested query includes"
    (let ((arguments
           (org-files-db--query-arguments
            '(links (status "resolved")) nil nil '(path target))))
      (expect (seq-count (lambda (item) (equal item "--include")) arguments)
              :to-equal 1)
      (expect (not (null (member "path,target" arguments))) :to-equal t)
      (expect (not (null (member "--output" arguments))) :to-equal t)
      (expect (not (null (member "flat" arguments))) :to-equal t)
      (expect (car (last arguments))
              :to-equal "(links (status \"resolved\"))")))

  (it "omits query includes when no configured feature requires them"
    (expect (member "--include"
                    (org-files-db--query-arguments '(files)))
            :to-equal nil))

  (it "infers source and target includes from link columns in one query"
    (let (calls seen-includes)
      (cl-letf (((symbol-function 'org-files-db--execute-query)
                 (lambda (_query &optional _config _origin includes)
                   (setq calls (1+ (or calls 0))
                         seen-includes includes)
                   '((target . "links") (results . nil))))
                ((symbol-function 'org-files-db--present-results)
                 (lambda (&rest _) nil)))
        (org-files-db-query
         '(links (status "resolved"))
         '((source-outline-path)
           (target-outline-path))))
      (expect calls :to-equal 1)
      (expect seen-includes :to-equal '(path target))))

  (it "retains response-target fallback for default query columns"
    (let (seen-columns)
      (cl-letf (((symbol-function 'org-files-db--execute-query)
                 (lambda (&rest _)
                   '((target . "links")
                     (results . (((kind . "link")
                                  (raw_target . "file:notes.org")))))))
                ((symbol-function 'org-files-db--present-results)
                 (lambda (_results columns &rest _)
                   (setq seen-columns columns))))
        (org-files-db-query '(headings) nil))
      (expect
       (mapcar #'org-files-db--presentation-column-name
               (append seen-columns nil))
       :to-equal
       (mapcar #'org-files-db--column-name org-files-db-link-columns))))

  (it "does not request target data for the default link columns"
    (expect (org-files-db--column-includes org-files-db-link-columns)
            :to-equal '(path)))

  (it "requests only the context required by configured columns"
    (expect (org-files-db--column-includes '((target-outline-path)))
            :to-equal '(target))
    (expect (org-files-db--column-includes '((link-target)
                                             (resolution-status)))
            :to-equal nil))

  (it "supports all search scopes"
    (expect (org-files-db--search-arguments "sqlite" 'all)
            :to-equal '("--format" "json" "sqlite"))
    (expect (org-files-db--search-arguments "sqlite" 'title)
            :to-equal '("--format" "json" "--title" "sqlite"))
    (expect (org-files-db--search-arguments "sqlite" 'body)
            :to-equal '("--format" "json" "--body" "sqlite")))

  (it "executes the real fake CLI response"
    (let ((response (org-files-db--execute-query '(headings))))
      (expect (org-files-db--response-target response) :to-equal 'headings)
      (expect (length (org-files-db--normalize-results response)) :to-equal 1))))

(describe "per-command configuration"
  (it "attaches configuration without copying a fresh result alist"
    (let* ((result '((kind . "heading") (title . "Shared")))
           (configured (org-files-db--result-with-config result "/tmp/db.toml")))
      (expect (eq (cdr configured) result) :to-equal t)
      (expect (org-files-db--get configured org-files-db--result-config-key)
              :to-equal "/tmp/db.toml")))

  (it "replaces existing configuration metadata without duplicates"
    (let* ((result `((,org-files-db--result-config-key . "/tmp/old.toml")
                     (kind . "heading")))
           (configured (org-files-db--result-with-config result "/tmp/new.toml")))
      (expect (org-files-db--get configured org-files-db--result-config-key)
              :to-equal "/tmp/new.toml")
      (expect (length (seq-filter
                       (lambda (entry)
                         (eq (car entry) org-files-db--result-config-key))
                       configured))
              :to-equal 1)))

  (it "lets query calls inherit, override, and disable the global configuration"
    (let* ((global (org-files-db-test--write-file
                    "query-global.toml" "db_path='global'\n"))
           (override (org-files-db-test--write-file
                      "query-override.toml" "db_path='override'\n"))
           (result '((kind . "heading") (title . "Configured")))
           seen-config selected)
      (setq org-files-db-config-file global)
      (cl-letf (((symbol-function 'org-files-db--execute-query)
                 (lambda (_query &optional config-file _origin _includes)
                   (setq seen-config config-file)
                   `((target . "headings") (results . (,result)))))
                ((symbol-function 'org-files-db--present-results)
                 (lambda (results &rest _)
                   (setq selected (car results)))))
        (org-files-db-query '(headings) nil)
        (expect seen-config :to-equal global)
        (expect (org-files-db--result-config-file selected) :to-equal global)
        (org-files-db-query
         '(headings) nil nil :config-file override)
        (expect seen-config :to-equal override)
        (expect (org-files-db--result-config-file selected) :to-equal override)
        (org-files-db-query
         '(headings) nil nil :config-file nil)
        (expect seen-config :to-equal nil)
        (expect (org-files-db--result-config-file selected) :to-equal nil))))

  (it "lets search calls inherit, override, disable, and use legacy scope"
    (let* ((global (org-files-db-test--write-file
                    "search-global.toml" "db_path='global'\n"))
           (override (org-files-db-test--write-file
                      "search-override.toml" "db_path='override'\n"))
           (result '((kind . "heading") (title . "Found")))
           seen-config seen-scope selected)
      (setq org-files-db-config-file global)
      (cl-letf (((symbol-function 'org-files-db--execute-search)
                 (lambda (_expression &optional scope config-file _origin)
                   (setq seen-scope scope
                         seen-config config-file)
                   `((results . (,result)))))
                ((symbol-function 'org-files-db--present-results)
                 (lambda (results &rest _)
                   (setq selected (car results)))))
        (org-files-db-search "report" nil nil)
        (expect seen-scope :to-equal 'all)
        (expect seen-config :to-equal global)
        (org-files-db-search
         "report" nil nil :scope 'title :config-file override)
        (expect seen-scope :to-equal 'title)
        (expect seen-config :to-equal override)
        (expect (org-files-db--result-config-file selected) :to-equal override)
        (org-files-db-search
         "report" nil nil :scope 'body :config-file nil)
        (expect seen-scope :to-equal 'body)
        (expect seen-config :to-equal nil)
        (expect (org-files-db--result-config-file selected) :to-equal nil)
        (org-files-db-search "report" nil nil 'title)
        (expect seen-scope :to-equal 'title)
        (expect seen-config :to-equal global)
        (org-files-db-search
         "report" nil nil 'body :config-file override)
        (expect seen-scope :to-equal 'body)
        (expect seen-config :to-equal override))))

  (it "retains explicit no-config result context after the global changes"
    (let* ((global (org-files-db-test--write-file
                    "later-global.toml" "db_path='later'\n"))
           (result
            (org-files-db--result-with-config
             '((kind . "heading") (title . "Public"))
             nil)))
      (setq org-files-db-config-file global)
      (expect (org-files-db--result-config-file result)
              :to-equal nil)))

  (it "honours a dynamically bound global unless an explicit option wins"
    (let* ((global (org-files-db-test--write-file
                    "dynamic.toml" "db_path='dynamic'\n"))
           (override (org-files-db-test--write-file
                      "explicit.toml" "db_path='explicit'\n"))
           seen-config)
      (cl-letf (((symbol-function 'org-files-db--execute-query)
                 (lambda (_query &optional config-file _origin _includes)
                   (setq seen-config config-file)
                   '((target . "headings") (results . nil))))
                ((symbol-function 'org-files-db--present-results)
                 (lambda (&rest _) nil)))
        (let ((org-files-db-config-file global))
          (org-files-db-query '(headings) nil)
          (expect seen-config :to-equal global)
          (org-files-db-query
           '(headings) nil nil :config-file override)
          (expect seen-config :to-equal override))))))

(describe "columns and completion candidates"
  (it "supports auto, maximum, and fixed widths"
    (let* ((results '(((title . "One")) ((title . "A long title"))))
           (columns '((title :width auto)
                      (title :width (max 5))
                      (title :width (fixed 3)))))
      (expect (org-files-db--column-widths results columns)
              :to-equal '(12 5 3))))

  (it "omits synthetic roots from displayed outline paths"
    (let ((result
           '((title . "Child")
             (node_path . (((kind . "root") (title . "File"))
                           ((kind . "heading") (title . "Parent"))
                           ((kind . "heading") (title . "Child")))))))
      (expect (org-files-db--outline-path result)
              :to-equal "Parent » Child")))

  (it "extracts root and heading path data in source order"
    (expect
     (org-files-db--path-data
      '("Loose"
        ((kind . "root") (title . "File"))
        ((title . "Implicit"))
        ((kind . "heading") (title . "Explicit"))))
     :to-equal '("File" "Loose" "Implicit" "Explicit")))

  (it "extracts outline data from vector paths"
    (expect
     (org-files-db--path-data
      ["Loose"
       ((kind . "root") (title . "File"))
       ((kind . "heading") (title . "Explicit"))])
     :to-equal '("File" "Loose" "Explicit")))

  (it "does not duplicate a matched file root"
    (let ((result
           '((kind . "root")
             (title . "File")
             (node_path . (((kind . "root") (title . "File")))))))
      (expect (org-files-db--column-value
               result '(outline-path :include-root t))
              :to-equal "File")))

  (it "formats the structured source heading hierarchy for links"
    (let ((result
           '((kind . "link")
             (source . ((file . ((title . "source-file")))
                        (heading . ((title . "Emacs")
                                    (outline_path . ("Applications"
                                                     "Editors"
                                                     "Emacs"))))))
             (node_path . (((kind . "root") (title . "Wrong root"))
                           ((kind . "heading") (title . "Wrong path")))))))
      (expect (org-files-db--column-value result '(source-outline-path))
              :to-equal "Applications » Editors » Emacs")
      (expect (org-files-db--column-value result '(outline-path))
              :to-equal "Applications » Editors » Emacs")
      (expect (org-files-db--column-value
               result '(source-outline-path :include-root t))
              :to-equal "source-file » Applications » Editors » Emacs")
      (expect (org-files-db--column-value
               result '(source-outline-path :include-match nil))
              :to-equal "Applications » Editors")))

  (it "uses node_path as source hierarchy fallback without the link itself"
    (let ((result
           '((kind . "link")
             (node_path . (((kind . "root") (title . "notes"))
                           ((kind . "heading") (title . "Applications"))
                           ((kind . "heading") (title . "Editors"))
                           ((kind . "heading") (title . "Emacs"))
                           ((kind . "link") (raw_target . "packages.org")))))))
      (expect (org-files-db--column-value result '(source-outline-path))
              :to-equal "Applications » Editors » Emacs")))

  (it "formats nested resolved target headings"
    (let ((result
           '((kind . "link")
             (resolution_status . "resolved")
             (target . ((file . ((title . "test-subheading")))
                        (heading . ((title . "heading 3")
                                    (outline_path . ("heading 1"
                                                     "heading 2"
                                                     "heading 3")))))))))
      (expect (org-files-db--column-value result '(target-outline-path))
              :to-equal "heading 1 » heading 2 » heading 3")
      (expect (org-files-db--column-value
               result '(target-outline-path :include-root t))
              :to-equal
              "test-subheading » heading 1 » heading 2 » heading 3")
      (expect (org-files-db--column-value
               result '(target-outline-path :include-match nil))
              :to-equal "heading 1 » heading 2")))

  (it "shows only an included root for resolved file targets"
    (let ((result
           '((kind . "link")
             (resolution_status . "resolved")
             (target . ((file . ((title . "target-file"))))))))
      (expect (org-files-db--column-value result '(target-outline-path))
              :to-equal "")
      (expect (org-files-db--column-value
               result '(target-outline-path :include-root t))
              :to-equal "target-file")
      (expect (org-files-db--column-value
               result
               '(target-outline-path :include-root t :include-match nil))
              :to-equal "target-file")))

  (it "leaves unresolved and external target paths empty"
    (dolist (status '("unresolved" "ambiguous" "external" "unsupported"))
      (let ((result
             `((kind . "link")
               (resolution_status . ,status)
               (raw_target . "file:authored.org::*Raw")
               (target . ((file . ((title . "Must not appear")))
                          (heading . ((outline_path . ("Raw")))))))))
        (expect (org-files-db--column-value result '(target-outline-path))
                :to-equal ""))))

  (it "leaves resolved targets without structured data empty"
    (expect (org-files-db--column-value
             '((kind . "link")
               (resolution_status . "resolved")
               (raw_target . "file:missing.org"))
             '(target-outline-path))
            :to-equal ""))

  (it "supports custom and global outline options independently"
    (let ((result
           '((kind . "link")
             (resolution_status . "resolved")
             (source . ((file . ((title . "source")))
                        (heading . ((outline_path . ("One" "Two"))))))
             (target . ((file . ((title . "target")))
                        (heading . ((outline_path . ("Three" "Four")))))))))
      (expect (org-files-db--column-value
               result '(source-outline-path :separator " / "))
              :to-equal "One / Two")
      (let ((org-files-db-outline-path-separator " -> ")
            (org-files-db-outline-path-include-root t)
            (org-files-db-outline-path-include-match nil))
        (expect (org-files-db--column-value result '(source-outline-path))
                :to-equal "source -> One")
        (expect (org-files-db--column-value result '(target-outline-path))
                :to-equal "target -> Three"))))

  (it "applies general middle truncation to outline columns"
    (let* ((definition
            '(target-outline-path
              :width (fixed 19)
              :truncate (:position middle :marker "…")))
           (value "heading 1 » heading 2 » heading 3")
           (text (org-files-db--truncate-column-value value 19 definition)))
      (expect (string-width text) :to-equal 19)
      (expect text :to-match "…")
      (expect text :to-match "heading 3$")))

  (it "preserves Unicode display widths in suffix truncation"
    (expect (org-files-db--string-suffix-to-width "A東京" 3)
            :to-equal "京")
    (expect (string-width
             (org-files-db--string-suffix-to-width "東京A" 3))
            :to-equal 3))

  (it "keeps original result objects on candidates"
    (let* ((result '((kind . "heading") (title . "Example")))
           (candidate (car (org-files-db--make-candidates
                            (list result) '((title :width auto))))))
      (expect (get-text-property 0 'org-files-db-result candidate)
              :to-equal result)
      (expect (get-text-property 0 'consult--candidate candidate)
              :to-equal result)))

  (it "does not retain full rows on ordinary candidates"
    (let* ((result '((kind . "heading") (title . "Metadata")))
           (presentation
            (org-files-db--prepare-presentation (list result) '((title))))
           (candidate
            (car (org-files-db--presentation-candidates presentation))))
      (expect (get-text-property
               0 'org-files-db-presentation-row candidate)
              :to-equal nil)
      (expect (get-text-property 0 'org-files-db-result candidate)
              :to-equal result)))

  (it "retains metadata for row-expanded candidates"
    (let* ((result '((kind . "heading") (title . "Metadata")))
           (columns (org-files-db--normalize-columns '((title))))
           (sources (org-files-db--presentation-build-sources (list result)))
           (rows (org-files-db--presentation-build-rows sources))
           (row (aref rows 0)))
      (setf (org-files-db--presentation-row-row-source row) 'tags
            (org-files-db--presentation-row-row-value row) "project")
      (org-files-db--presentation-populate-cells rows columns)
      (org-files-db--presentation-prepare-faces rows)
      (let* ((widths
              (org-files-db--presentation-calculate-widths rows columns))
             (candidate
              (car
               (car
                (org-files-db--presentation-format-candidates
                 rows columns widths)))))
        (expect (eq (get-text-property
                     0 'org-files-db-presentation-row candidate)
                    row)
                :to-equal t)
        (expect (get-text-property 0 'org-files-db-row-source candidate)
                :to-equal 'tags)
        (expect (get-text-property 0 'org-files-db-row-value candidate)
                :to-equal "project"))))

  (it "does not display internal configuration metadata by default"
    (let* ((config (org-files-db-test--write-file
                    "candidate.toml" "db_path='candidate'\n"))
           (result
            (org-files-db--result-with-config
             '((kind . "heading") (title . "Visible title"))
             config))
           (candidate
            (car (org-files-db--make-candidates
                  (list result) '((title :width auto))))))
      (expect (substring-no-properties candidate) :to-match "Visible title")
      (expect (string-match-p (regexp-quote config)
                              (substring-no-properties candidate))
              :to-equal nil)))

  (it "uses one shared width for every row"
    (let* ((results '(((title . "A")) ((title . "Long"))))
           (candidates (org-files-db--make-candidates
                        results '((title :width auto)))))
      (expect (string-width
               (org-files-db-test--candidate-visible (car candidates)))
              :to-equal
              (string-width
               (org-files-db-test--candidate-visible (cadr candidates))))))

  (it "reuses normalized column specifications"
    (let ((columns
           (org-files-db--normalize-columns
            '((title :width (max 20)
                     :truncate (:position middle :marker "…"))))))
      (expect (eq columns (org-files-db--normalize-columns columns))
              :to-equal t)))

  (it "calculates each displayed value once before completion filtering"
    (let* ((results '(((title . "Alpha"))
                      ((title . "Beta"))))
           (columns (org-files-db--normalize-columns '((title))))
           (column (aref columns 0))
           (extractor
            (org-files-db--presentation-column-extractor column))
           (calls 0))
      (setf (org-files-db--presentation-column-extractor column)
            (lambda (source normalized-column)
              (setq calls (1+ calls))
              (funcall extractor source normalized-column)))
      (let* ((presentation
              (org-files-db--prepare-presentation results columns))
             (candidates
              (org-files-db--presentation-candidates presentation))
             (table (org-files-db--completion-table candidates)))
        (expect calls :to-equal 2)
        (all-completions "a" table)
        (all-completions "b" table)
        (expect calls :to-equal 2))))

  (it "does not recalculate presentation work during filtering"
    (let* ((presentation
            (org-files-db--prepare-presentation
             '(((title . "Alpha"))
               ((title . "Beta")))
             '((title))))
           (candidates
            (org-files-db--presentation-candidates presentation))
           (table (org-files-db--completion-table candidates)))
      (cl-letf (((symbol-function 'org-files-db--presentation-populate-cells)
                 (lambda (&rest _)
                   (error "Filtering must reuse cached cells")))
                ((symbol-function 'org-files-db--presentation-calculate-widths)
                 (lambda (&rest _)
                   (error "Filtering must reuse cached widths")))
                ((symbol-function 'org-files-db--presentation-prepare-faces)
                 (lambda (&rest _)
                   (error "Filtering must reuse cached faces")))
                ((symbol-function 'org-files-db--format-presentation-row)
                 (lambda (&rest _)
                   (error "Filtering must reuse formatted candidates"))))
        (expect (length (all-completions "Alpha" table)) :to-equal 1)
        (expect (length (all-completions "Beta" table)) :to-equal 1))))

  (it "formats complete rows without creating propertized cell copies"
    (cl-letf (((symbol-function 'org-files-db--format-presentation-cell)
               (lambda (&rest _)
                 (error "Candidate rows must be assembled directly"))))
      (let ((candidates
             (org-files-db--make-candidates
              '(((title . "Alpha") (level . 2)))
              '((title :width (fixed 10))))))
        (expect (org-files-db-test--candidate-visible (car candidates))
                :to-equal "Alpha     "))))

  (it "keeps complete untruncated column values searchable"
    (let* ((result '((title . "Visible prefix and hidden needle")))
           (presentation
            (org-files-db--prepare-presentation
             (list result)
             '((title :width (fixed 10)
                      :truncate (:position right :marker "…")))))
           (candidates
            (org-files-db--presentation-candidates presentation))
           (candidate (car candidates))
           (visible (org-files-db-test--candidate-visible candidate))
           (completion-styles '(substring)))
      (expect (string-match-p "hidden needle" visible) :to-equal nil)
      (let ((matches
             (completion-all-completions
              "hidden needle"
              (org-files-db--completion-table candidates)
              nil
              (length "hidden needle"))))
        (expect (car matches) :to-equal candidate)
        (expect (consp (cdr matches)) :to-equal nil))))

  (it "does not inspect cached cell widths for fixed columns"
    (let* ((columns
            (org-files-db--normalize-columns
             '((title :width (fixed 3)))))
           (sources
            (org-files-db--presentation-build-sources
             '(((title . "Long title")))))
           (rows (org-files-db--presentation-build-rows sources)))
      (org-files-db--presentation-populate-cells rows columns)
      (cl-letf (((symbol-function
                  'org-files-db--presentation-cell-display-width)
                 (lambda (_cell)
                   (error "Fixed widths must not inspect cell widths"))))
        (expect
         (append
          (org-files-db--presentation-calculate-widths rows columns)
          nil)
         :to-equal '(3)))))

  (it "stops maximum-width scanning after the cap is reached"
    (let* ((columns
            (org-files-db--normalize-columns
             '((title :width (max 5)))))
           (sources
            (org-files-db--presentation-build-sources
             '(((title . "ab"))
               ((title . "12345"))
               ((title . "not inspected")))))
           (rows (org-files-db--presentation-build-rows sources)))
      (org-files-db--presentation-populate-cells rows columns)
      (setf (org-files-db--presentation-cell-display-width
             (aref (org-files-db--presentation-row-cells
                    (aref rows 2))
                   0))
            'must-not-be-inspected)
      (expect
       (append
        (org-files-db--presentation-calculate-widths rows columns)
        nil)
       :to-equal '(5))))

  (it "sanitizes each distinct face once per presentation"
    (let ((calls 0)
          (results '(((title . "One") (level . 2))
                     ((title . "Two") (level . 2))
                     ((title . "Three") (level . 2)))))
      (cl-letf (((symbol-function 'org-files-db--sanitized-face)
                 (lambda (_face)
                   (setq calls (1+ calls))
                   nil)))
        (org-files-db--prepare-presentation results '((title))))
      (expect calls :to-equal 1)))

  (it "uses direct lookup for selected prepared candidates"
    (let* ((result '((kind . "heading") (title . "Example")))
           (presentation
            (org-files-db--prepare-presentation
             (list result) '((title))))
           (candidates
            (org-files-db--presentation-candidates presentation))
           (selected (substring-no-properties (car candidates))))
      (expect
       (not (null
             (gethash candidates org-files-db--candidate-lookups)))
       :to-equal t)
      (expect (org-files-db--candidate-result selected candidates)
              :to-equal result)))

  (it "preserves duplicate visible candidates and source order"
    (let* ((first '((kind . "heading") (title . "Same")))
           (second '((kind . "heading") (title . "Same")))
           (results (list first second))
           (presentation
            (org-files-db--prepare-presentation results '((title))))
           (candidates
            (org-files-db--presentation-candidates presentation))
           (first-visible
            (org-files-db-test--candidate-visible (car candidates)))
           (second-visible
            (org-files-db-test--candidate-visible (cadr candidates))))
      (expect (substring-no-properties first-visible)
              :to-equal (substring-no-properties second-visible))
      (expect (equal (car candidates) (cadr candidates)) :to-equal nil)
      (expect
       (mapcar
        (lambda (candidate)
          (get-text-property 0 'org-files-db-result candidate))
        candidates)
       :to-equal results)))

  (it "reports repeatable presentation benchmark phases"
    (let* ((summary
            (org-files-db-benchmark--presentation
             '(((title . "Überblick"))
               ((title . "東京")))
             '((title :width auto))
             :iterations 2))
           (phases (plist-get summary :phases))
           (formatting
            (cdr (assq :candidate-formatting phases)))
           (completion
            (cdr (assq :completion-filter phases)))
           (metrics
            (cdr (assq :result-value-extraction
                       (plist-get summary :phase-metrics))))
           (conses
            (cdr (assq :conses (plist-get metrics :allocation)))))
      (expect (plist-get summary :result-count) :to-equal 2)
      (expect (plist-get summary :iterations) :to-equal 2)
      (expect (numberp (plist-get summary :garbage-collection-time))
              :to-equal t)
      (expect (numberp (plist-get summary :candidate-characters))
              :to-equal t)
      (expect (numberp (plist-get formatting :minimum)) :to-equal t)
      (expect (numberp (plist-get formatting :median)) :to-equal t)
      (expect (numberp (plist-get formatting :maximum)) :to-equal t)
      (expect (numberp (plist-get completion :median)) :to-equal t)
      (expect (numberp
               (plist-get
                (plist-get metrics :garbage-collection-time) :median))
              :to-equal t)
      (expect (numberp (plist-get conses :median)) :to-equal t)))

  (it "bounds and restores GC policy for large presentations"
    (let ((org-files-db--large-presentation-row-count 1)
          (gc-cons-threshold 800000)
          (gc-cons-percentage 0.1)
          (original-threshold 800000)
          observed-threshold
          (collections 0))
      (cl-letf (((symbol-function 'org-files-db--prepare-presentation-1)
                 (lambda (&rest _)
                   (setq observed-threshold gc-cons-threshold)
                   (make-org-files-db--presentation
                    :timings '(:total 0.0)
                    :phase-metrics nil)))
                ((symbol-function 'garbage-collect)
                 (lambda () (setq collections (1+ collections)) nil)))
        (org-files-db--prepare-presentation '(((title . "Large"))) '((title)))
        (expect observed-threshold
                :to-be-greater-than original-threshold)
        (expect collections :to-equal 1)
        (expect gc-cons-threshold :to-equal original-threshold))))

  (it "avoids an unnecessary boundary GC under a generous GC policy"
    (let ((org-files-db--large-presentation-row-count 1)
          (gc-cons-threshold 800000)
          (gc-cons-percentage 1.0)
          observed-threshold
          (collections 0))
      (cl-letf (((symbol-function 'org-files-db--prepare-presentation-1)
                 (lambda (&rest _)
                   (setq observed-threshold gc-cons-threshold)
                   (make-org-files-db--presentation
                    :timings '(:total 0.0)
                    :phase-metrics nil)))
                ((symbol-function 'garbage-collect)
                 (lambda () (setq collections (1+ collections)) nil)))
        (org-files-db--prepare-presentation '(((title . "Large"))) '((title)))
        (expect observed-threshold :to-equal 800000)
        (expect collections :to-equal 0))))

  (it "restores GC policy when large presentation preparation fails"
    (let ((org-files-db--large-presentation-row-count 1)
          (original-threshold gc-cons-threshold))
      (cl-letf (((symbol-function 'org-files-db--prepare-presentation-1)
                 (lambda (&rest _) (error "preparation failed"))))
        (expect
         (org-files-db--prepare-presentation '(((title . "Large"))) '((title)))
         :to-throw 'error)
        (expect gc-cons-threshold :to-equal original-threshold))))

  (it "preserves mapped JSON fields in normalized columns"
    (let ((result
           '((file_id . 7)
             (parent_id . 6)
             (scheduled_raw . "<2026-08-06 Thu>")
             (deadline_raw . "<2026-08-07 Fri>")
             (closed_raw . "[2026-08-05 Wed]")
             (link_type . "file")
             (link_path . "notes.org")
             (location . ((byte_end . 42))))))
      (dolist (case '((byte-end "42")
                      (file-id "7")
                      (parent-id "6")
                      (scheduled-raw "<2026-08-06 Thu>")
                      (deadline-raw "<2026-08-07 Fri>")
                      (closed-raw "[2026-08-05 Wed]")
                      (link-type "file")
                      (link-path "notes.org")))
        (expect (org-files-db--column-value result (list (car case)))
                :to-equal (cadr case)))))

  (it "formats common string lists and scalar values without general format"
    (expect (org-files-db--presentation-display-value '("one" "two"))
            :to-equal "one,two")
    (expect (org-files-db--presentation-display-value 42) :to-equal "42")
    (expect (org-files-db--presentation-display-value 'ready)
            :to-equal "ready"))

  (it "keeps hash-table JSON compatible with shared result access"
    (let ((result
           (org-files-db--parse-json-as
            (concat
             "{\"kind\":\"heading\",\"title\":\"Hash\","
             "\"node_path\":["
             "{\"kind\":\"file\",\"title\":\"Root\"},"
             "{\"kind\":\"heading\",\"title\":\"Parent\"},"
             "{\"kind\":\"heading\",\"title\":\"Hash\"}]}")
            'hash-table)))
      (expect (hash-table-p result) :to-equal t)
      (expect (org-files-db--column-value result '(title))
              :to-equal "Hash")
      (expect (org-files-db--column-value result '(outline-path))
              :to-equal "Parent » Hash"))))

  (it "normalizes direct arrays of hash-table results"
    (let* ((response
            (org-files-db--parse-json-as
             "[{\"kind\":\"heading\",\"title\":\"Hash\"}]"
             'hash-table))
           (results (org-files-db--normalize-results response)))
      (expect (length results) :to-equal 1)
      (expect (hash-table-p (car results)) :to-equal t)))

(describe "UTF-8 locations"
  (it "converts zero-based UTF-8 offsets to Emacs positions"
    (let* ((file (org-files-db-test--write-file "utf8.org" "äöü\n* Ziel\n"))
           (prefix "äöü\n")
           (byte (length (encode-coding-string prefix 'utf-8-unix))))
      (expect (org-files-db--utf8-byte-position file byte 'utf-8-unix)
              :to-equal (1+ (length prefix))))))

(describe "views"
  (before-each
    (setq org-files-db-views
          '(("open"
             :command query
             :query (headings (not (done)))
             :columns ((title :width auto)))
            ("fts"
             :command search
             :expression "sqlite"
             :scope title))))

  (it "looks up a view without exposing the configured object"
    (let ((view (org-files-db-views-get "open")))
      (setcar view "changed")
      (expect (caar org-files-db-views) :to-equal "open")))

  (it "rejects duplicate names"
    (setq org-files-db-views
          '(("same" :command query :query (headings))
            ("same" :command search :expression "x")))
    (expect (org-files-db-views--validate)
            :to-throw 'user-error))

  (it "delegates a query view to the generic query command"
    (let (arguments)
      (cl-letf (((symbol-function 'org-files-db-query)
                 (lambda (&rest args) (setq arguments args))))
        (org-files-db-views-query "open"))
      (expect (car arguments) :to-equal '(headings (not (done))))
      (expect (plist-get (nthcdr 3 arguments) :config-file) :to-equal nil)))

  (it "delegates a search view to the generic search command"
    (let (arguments)
      (cl-letf (((symbol-function 'org-files-db-search)
                 (lambda (&rest args) (setq arguments args))))
        (org-files-db-views-search "fts"))
      (expect (car arguments) :to-equal "sqlite")
      (expect (plist-get (nthcdr 3 arguments) :scope) :to-equal 'title)
      (expect (plist-get (nthcdr 3 arguments) :config-file) :to-equal nil)))

  (it "passes an inherited global configuration explicitly"
    (let ((config (org-files-db-test--write-file
                   "view-global.toml" "db_path='global'\n"))
          arguments)
      (setq org-files-db-config-file config)
      (cl-letf (((symbol-function 'org-files-db-query)
                 (lambda (&rest args) (setq arguments args))))
        (org-files-db-views-query "open"))
      (expect (plist-get (nthcdr 3 arguments) :config-file)
              :to-equal config)))

  (it "lets a view override or explicitly disable the global configuration"
    (let ((global (org-files-db-test--write-file
                   "view-default.toml" "db_path='global'\n"))
          (override (org-files-db-test--write-file
                     "view-private.toml" "db_path='private'\n"))
          query-arguments search-arguments)
      (setq org-files-db-config-file global
            org-files-db-views
            `(("private"
               :command query
               :config-file ,override
               :query (headings))
              ("without"
               :command search
               :config-file nil
               :expression "public")))
      (cl-letf (((symbol-function 'org-files-db-query)
                 (lambda (&rest args) (setq query-arguments args)))
                ((symbol-function 'org-files-db-search)
                 (lambda (&rest args) (setq search-arguments args))))
        (org-files-db-views-query "private")
        (org-files-db-views-search "without"))
      (expect (plist-get (nthcdr 3 query-arguments) :config-file)
              :to-equal override)
      (expect (not (null
                    (plist-member (nthcdr 3 search-arguments)
                                  :config-file)))
              :to-equal t)
      (expect (plist-get (nthcdr 3 search-arguments) :config-file)
              :to-equal nil))))

(describe "Org rendering and Embark export"
  (it "renders flat results as linked headings"
    (let* ((file (org-files-db-test--write-file "flat.org" "* Heading\n"))
           (result (org-files-db-test--result "heading" "Heading" file))
           (text (org-files-db-export-render-results (list result) 'flat 0)))
      (expect text :to-match "^\\* \\[\\[org-files-db:")))

  (it "keeps identical path labels from different files separate"
    (let ((left '((kind . "heading")
                  (title . "Same")
                  (level . 1)
                  (location . ((file_path . "/tmp/left.org")
                               (line . 1)))))
          (right '((kind . "heading")
                   (title . "Same")
                   (level . 1)
                   (location . ((file_path . "/tmp/right.org")
                                (line . 1))))))
      (expect (equal (org-files-db-export--path-node-key left)
                     (org-files-db-export--path-node-key right))
              :to-equal nil)))

  (it "renders an outline from path nodes"
    (let* ((file (org-files-db-test--write-file "tree.org" "* Parent\n** Child\n"))
           (result `((kind . "heading")
                     (id . 2)
                     (title . "Child")
                     (level . 2)
                     (node_path . (((kind . "heading")
                                    (id . 1)
                                    (title . "Parent")
                                    (level . 1))))
                     (location . ((file_path . ,file)
                                  (line . 2)
                                  (byte_start . 9)))))
           (text (org-files-db-export-render-results (list result) 'outline 0)))
      (expect text :to-match "^\\* Parent")
      (expect text :to-match "\\*\\* \\[\\[org-files-db:")))

  (it "preserves an authored file link in a linked heading"
    (let* ((target (org-files-db-test--write-file "target.org" "#+TITLE: Target\n"))
           (source (org-files-db-test--write-file
                    "source.org"
                    "* [[file:target.org][Alias]] trailing\n"))
           (result (org-files-db-test--result "heading" "Alias trailing" source)))
      (expect (org-files-db-export--result-text result)
              :to-match (regexp-quote (expand-file-name target)))))

  (it "exports only result-bearing candidates"
    (let* ((file (org-files-db-test--write-file "candidate.org" "* One\n"))
           (result (org-files-db-test--result "heading" "One" file))
           (candidate (propertize "One" 'org-files-db-result result))
           exported)
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _)
                   (setq exported (with-current-buffer buffer
                                    (buffer-string))))))
        (org-files-db-export-embark-org (list candidate)))
      (expect exported :to-match "org-files-db results")
      (expect exported :to-match "One"))))

(describe "link actions"
  (it "inserts a normal file link"
    (let* ((file (org-files-db-test--write-file "note.org" "#+TITLE: Note\n"))
           (result (org-files-db-test--result "file" "Note" file)))
      (with-temp-buffer
        (setq buffer-file-name
              (expand-file-name "origin.org" org-files-db-test--directory))
        (org-files-db-actions-insert-file-link result 'file)
        (expect (buffer-string) :to-match "\\[\\[file:"))))

  (it "inserts a brittle heading link without modifying the target"
    (let* ((file (org-files-db-test--write-file "heading.org" "* Target\n"))
           (result (org-files-db-test--result "heading" "Target" file)))
      (with-temp-buffer
        (setq buffer-file-name
              (expand-file-name "origin.org" org-files-db-test--directory))
        (org-files-db-actions-insert-heading-link result 'heading)
        (expect (buffer-string) :to-match "::\\*Target"))))

  (it "detects a file-level CUSTOM_ID when generating unique IDs"
    (with-temp-buffer
      (org-mode)
      (insert ":PROPERTIES:\n:CUSTOM_ID: file-root\n:END:\n\n* Heading\n")
      (goto-char (point-min))
      (expect (org-files-db-actions--custom-id-used-p "file-root") :to-equal t)))

  (it "follows a file link embedded in a heading"
    (let* ((target (org-files-db-test--write-file "follow.org" "#+TITLE: Follow\n"))
           (source (org-files-db-test--write-file
                    "source-follow.org"
                    "* [[file:follow.org][Follow]]\n"))
           (result (org-files-db-test--result "heading" "Follow" source)))
      (save-window-excursion
        (org-files-db-actions-follow-heading-link result)
        (expect (file-truename buffer-file-name)
                :to-equal (file-truename target)))))

  (it "uses the producing result configuration for an indexed ID follow-up"
    (let* ((source-config (org-files-db-test--write-file
                           "source-db.toml" "db_path='source'\n"))
           (other-config (org-files-db-test--write-file
                          "other-db.toml" "db_path='other'\n"))
           (source-result
            (org-files-db--result-with-config
             '((kind . "heading") (title . "Alias"))
             source-config))
           seen-config returned)
      (setq org-files-db-config-file other-config)
      (cl-letf (((symbol-function 'org-files-db--execute-query)
                 (lambda (_query &optional config-file _origin _includes)
                   (setq seen-config config-file)
                   '((target . "headings")
                     (results . (((kind . "heading")
                                  (title . "Target")))))))
                ((symbol-function 'org-files-db--visit-result)
                 (lambda (_) nil)))
        (setq returned
              (org-files-db--goto-linked-target
               (list :type "id"
                     :path "target-id"
                     :source-result source-result))))
      (expect seen-config :to-equal source-config)
      (expect (org-files-db--result-config-file returned)
              :to-equal source-config)))

  (it "rewrites relative file links while preserving search options"
    (let* ((source (expand-file-name "a/source.org" org-files-db-test--directory))
           (new (expand-file-name "b/new.org" org-files-db-test--directory))
           (info (list :file source
                       :path "../old.org"
                       :search "#target"
                       :description "Target"
                       :format "bracket")))
      (expect (org-files-db-actions--format-rewritten-link info new)
              :to-match "new.org::#target")))

  (it "rebases relative self-links from the renamed source directory"
    (let* ((old (expand-file-name "a/old.org" org-files-db-test--directory))
           (new (expand-file-name "b/new.org" org-files-db-test--directory))
           (info (list :file old
                       :path "old.org"
                       :description "Self"
                       :format "bracket")))
      (org-files-db-test--write-file "a/old.org" "* Self\n")
      (expect (org-files-db-actions--format-rewritten-link info new old)
              :to-match "\\[\\[file:new\\.org\\]")))

  (it "queries only path-based file links during rename"
    (let (query)
      (cl-letf (((symbol-function 'org-files-db--execute-query)
                 (lambda (value &rest _) (setq query value) nil)))
        (org-files-db-actions--incoming-file-link-results "/tmp/old.org"))
      (expect query :to-equal
              '(links
                (and
                 (link-type "file")
                 (target
                  (files (file-path "/tmp/old.org" :exact t))))))))

  (it "propagates result configuration to the rename follow-up query"
    (let* ((config (org-files-db-test--write-file
                    "rename.toml" "db_path='rename'\n"))
           (file (org-files-db-test--write-file "rename.org" "#+TITLE: Rename\n"))
           (result
            (org-files-db--result-with-config
             (org-files-db-test--result "file" "Rename" file)
             config))
           arguments)
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _)
                   (expand-file-name "renamed.org"
                                     org-files-db-test--directory)))
                ((symbol-function 'org-files-db-actions-rename-file)
                 (lambda (&rest args) (setq arguments args))))
        (org-files-db-actions-rename-file-result result))
      (expect (plist-get (nthcdr 3 arguments) :config-file)
              :to-equal config)))

  (it "accepts a destination below a writable existing ancestor"
    (let ((destination
           (expand-file-name "new/deep/file.org"
                             org-files-db-test--directory)))
      (expect (org-files-db-actions--writable-parent-directory-p destination)
              :to-equal t)))

(describe "dynamic blocks"
  (it "requests path context only for outline query blocks"
    (let (seen-includes)
      (with-temp-buffer
        (org-mode)
        (cl-letf (((symbol-function 'org-files-db--execute-query)
                   (lambda (_query &optional _config _origin includes)
                     (push includes seen-includes)
                     '((target . "headings") (results . nil)))))
          (org-dblock-write:org-files-db-query
           '(:query "(headings)" :layout flat))
          (org-dblock-write:org-files-db-query
           '(:query "(headings)" :layout outline))))
      (expect (nreverse seen-includes) :to-equal '(nil (path)))))

  (it "renders direct query blocks through the shared query executor"
    (let ((config (org-files-db-test--write-file
                   "block-query.toml" "db_path='query'\n"))
          (result '((kind . "heading")
                    (title . "Dynamic")
                    (location . ((file_path . "/tmp/dynamic.org")
                                 (line . 1)
                                 (byte_start . 0)))))
          seen-config)
      (with-temp-buffer
        (org-mode)
        (cl-letf (((symbol-function 'org-files-db--execute-query)
                   (lambda (_query &optional config-file _origin _includes)
                     (setq seen-config config-file)
                     `((target . "headings")
                       (results . (,result))))))
          (org-dblock-write:org-files-db-query
           `(:query "(headings)" :config-file ,config :layout flat)))
        (expect (buffer-string) :to-match "Dynamic"))
      (expect seen-config :to-equal config)))

  (it "lets direct dynamic blocks inherit or disable configuration"
    (let ((global (org-files-db-test--write-file
                   "block-global.toml" "db_path='global'\n"))
          seen-configs)
      (setq org-files-db-config-file global)
      (with-temp-buffer
        (org-mode)
        (cl-letf (((symbol-function 'org-files-db--execute-query)
                   (lambda (_query &optional config-file _origin _includes)
                     (push config-file seen-configs)
                     '((target . "headings") (results . nil)))))
          (org-dblock-write:org-files-db-query
           '(:query "(headings)" :layout flat))
          (org-dblock-write:org-files-db-query
           '(:query "(headings)" :config-file none :layout flat))))
      (expect (nreverse seen-configs) :to-equal (list global nil))))

  (it "uses a predefined view configuration in dynamic blocks"
    (let ((config (org-files-db-test--write-file
                   "block-view.toml" "db_path='view'\n"))
          seen-config)
      (setq org-files-db-views
            `(("private"
               :command query
               :config-file ,config
               :query (headings))))
      (with-temp-buffer
        (org-mode)
        (cl-letf (((symbol-function 'org-files-db--execute-query)
                   (lambda (_query &optional config-file _origin _includes)
                     (setq seen-config config-file)
                     '((target . "headings") (results . nil)))))
          (org-dblock-write:org-files-db-query
           '(:view "private" :layout flat))))
      (expect seen-config :to-equal config)))

  (it "lets view-backed dynamic blocks inherit or disable configuration"
    (let ((global (org-files-db-test--write-file
                   "block-view-global.toml" "db_path='global'\n"))
          seen-configs)
      (setq org-files-db-config-file global
            org-files-db-views
            '(("inherited" :command query :query (headings))
              ("without" :command query :config-file nil
               :query (headings))))
      (with-temp-buffer
        (org-mode)
        (cl-letf (((symbol-function 'org-files-db--execute-query)
                   (lambda (_query &optional config-file _origin _includes)
                     (push config-file seen-configs)
                     '((target . "headings") (results . nil)))))
          (org-dblock-write:org-files-db-query
           '(:view "inherited" :layout flat))
          (org-dblock-write:org-files-db-query
           '(:view "without" :layout flat))))
      (expect (nreverse seen-configs) :to-equal (list global nil))))

  (it "passes direct search block configuration to the shared executor"
    (let ((config (org-files-db-test--write-file
                   "block-search.toml" "db_path='search'\n"))
          seen-config seen-scope)
      (with-temp-buffer
        (org-mode)
        (cl-letf (((symbol-function 'org-files-db--execute-search)
                   (lambda (_expression &optional scope config-file _origin)
                     (setq seen-scope scope
                           seen-config config-file)
                     '((results . nil)))))
          (org-dblock-write:org-files-db-search
           `(:expression "report"
             :scope title
             :config-file ,config
             :layout flat))))
      (expect seen-scope :to-equal 'title)
      (expect seen-config :to-equal config)))

  (it "does not interpret nil text as a configuration path"
    (expect (org-files-db-dblock--config-file
             '(:query "(headings)" :config-file "nil")
             'query)
            :to-throw 'user-error))

  (it "rejects conflicting query block parameters"
    (expect (org-files-db-dblock--query-definition
             '(:query "(headings)" :view "open"))
     :to-throw 'user-error)))

(describe "live search integration"
  (it "passes the minimum input as a Consult keyword option"
    (let ((features (cons 'consult features))
          dynamic-arguments)
      (cl-letf (((symbol-function 'org-files-db-search--consult-dynamic-collection)
                 (lambda (&rest arguments)
                   (setq dynamic-arguments arguments)
                   'collection))
                ((symbol-function 'org-files-db-search--consult-read)
                 (lambda (&rest _) nil)))
        (condition-case nil
            (org-files-db-search-live)
          (user-error nil)))
      (expect (plist-get (cdr dynamic-arguments) :min-input)
              :to-equal org-files-db-search-min-input)))

  (it "passes override and explicit nil configuration to live searches"
    (let* ((features (cons 'consult features))
           (global (org-files-db-test--write-file
                    "live-global.toml" "db_path='global'\n"))
           (override (org-files-db-test--write-file
                      "live-override.toml" "db_path='override'\n"))
           (result '((kind . "heading") (title . "Live")))
           seen-config seen-scope)
      (setq org-files-db-config-file global)
      (cl-letf (((symbol-function 'org-files-db-search--consult-dynamic-collection)
                 (lambda (function &rest _) function))
                ((symbol-function 'org-files-db-search--live-candidates)
                 (lambda (_input _columns scope config-file)
                   (setq seen-scope scope
                         seen-config config-file)
                   nil))
                ((symbol-function 'org-files-db-search--consult-read)
                 (lambda (collection &rest _)
                   (funcall collection "report")
                   result)))
        (org-files-db-search-live nil #'ignore)
        (expect seen-scope :to-equal 'all)
        (expect seen-config :to-equal global)
        (org-files-db-search-live
         nil #'ignore :scope 'title :config-file override)
        (expect seen-scope :to-equal 'title)
        (expect seen-config :to-equal override)
        (org-files-db-search-live
         nil #'ignore :config-file nil)
        (expect seen-scope :to-equal 'all)
        (expect seen-config :to-equal nil))))

  (it "retains live-search configuration on generated candidates"
    (let* ((config (org-files-db-test--write-file
                    "live-result.toml" "db_path='live'\n"))
           (result '((kind . "heading") (title . "Live result")))
           arguments candidates candidate-result)
      (cl-letf (((symbol-function 'org-files-db--start-process)
                 (lambda (_command args callback)
                   (setq arguments args)
                   (funcall callback `((results . (,result))) nil)
                   'fake-process)))
        (setq candidates
              (org-files-db-search--live-candidates
               "report" '((title :width auto)) 'all config)))
      (setq candidate-result
            (get-text-property 0 'org-files-db-result (car candidates)))
      (expect (not (null (member config arguments))) :to-equal t)
      (expect (org-files-db--result-config-file candidate-result)
              :to-equal config))))

(describe "asynchronous process support"
  (it "delivers parsed JSON to its callback"
    (let (done value failure process)
      (setq process
            (org-files-db--start-process
             "search" '("--format" "json" "x")
             (lambda (result error-data)
               (setq value result failure error-data done t))))
      (while (and (not done) (process-live-p process))
        (accept-process-output process 0.05))
      (accept-process-output process 0.05)
      (expect failure :to-equal nil)
      (expect (org-files-db--result-title (car value))
              :to-equal "Search result"))))

))

(provide 'org-files-db-test)

;;; org-files-db-test.el ends here
