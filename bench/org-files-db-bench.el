;;; org-files-db-bench.el --- On-demand end-to-end benchmark -*- lexical-binding: t; -*-

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

;; Measures the complete Emacs path from a query until the candidate
;; list can be used, with a real orgfdb executable and a generated
;; corpus.  Run it with `make bench'.  It is not part of `make ci'.
;;
;; Two paths are measured for each corpus size:
;;
;; - one-shot: `org-files-db-query' with presentation-json output,
;; - cached: `org-files-db-view' with `org-files-db-cache-mode' and a
;;   watcher, which uses `orgfdb view read'.
;;
;; The environment variable ORGFDB selects the executable.  The
;; variable ORGFDB_BENCH_SIZES overrides the sizes (space separated).
;; The script prints a message and exits normally when orgfdb is not
;; found.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-files-db)
(require 'org-files-db-cache)

(defconst org-files-db-bench--sizes '(100 1000 10000 50000)
  "Default numbers of headings in the generated corpus.")

(defconst org-files-db-bench--headings-per-file 500
  "Approximate number of headings in each generated file.")

(defconst org-files-db-bench--runs 3
  "Number of measured runs for each size and path.")

(defconst org-files-db-bench--columns
  '((title) (tags) (file-name) (outline-path))
  "Columns used for both measured paths.")

(defconst org-files-db-bench--query "(headings)"
  "Structural query used for both measured paths.")

(defconst org-files-db-bench--view-name "bench"
  "Name of the predefined view used for the cached path.")

(defconst org-files-db-bench--tags '("work" "home" "idea" "later" "review")
  "Tags used by the generated corpus.")

(defconst org-files-db-bench--keywords '("TODO" "NEXT" "DONE" nil nil)
  "Todo keywords used by the generated corpus.")

;;;; Helpers

(defun org-files-db-bench--executable ()
  "Return the orgfdb executable to measure, or nil."
  (let ((env (getenv "ORGFDB")))
    (if (and env (not (string-empty-p env)))
        (and (file-executable-p env) (expand-file-name env))
      (executable-find "orgfdb"))))

(defun org-files-db-bench--median (values)
  "Return the median of the numbers in VALUES."
  (let* ((sorted (sort (copy-sequence values) #'<))
         (count (length sorted)))
    (if (cl-oddp count)
        (nth (/ count 2) sorted)
      (/ (+ (nth (1- (/ count 2)) sorted) (nth (/ count 2) sorted)) 2.0))))

(defmacro org-files-db-bench--measure (&rest body)
  "Evaluate BODY and return (VALUE SECONDS GC-SECONDS)."
  (declare (indent 0))
  `(let ((start (float-time))
         (gc-start gc-elapsed))
     (let ((value (progn ,@body)))
       (list value (- (float-time) start) (- gc-elapsed gc-start)))))

;;;; Corpus

(defun org-files-db-bench--write-file (file count seed)
  "Write FILE with COUNT headings, varied by SEED."
  (with-temp-file file
    (insert "#+TITLE: Benchmark file " (number-to-string seed) "\n\n")
    (let ((level-stack 0)
          (tag-count (length org-files-db-bench--tags))
          (keyword-count (length org-files-db-bench--keywords)))
      (dotimes (index count)
        (let* ((number (+ (* seed 100000) index))
               (level
                (cond ((zerop (% index 50)) 1)
                      ((< level-stack 4) (min 4 (1+ level-stack)))
                      (t (+ 2 (% index 3)))))
               (keyword (nth (% number keyword-count)
                             org-files-db-bench--keywords))
               (tags
                (and (zerop (% number 3))
                     (format "  :%s:%s:"
                             (nth (% number tag-count)
                                  org-files-db-bench--tags)
                             (nth (% (+ number 2) tag-count)
                                  org-files-db-bench--tags)))))
          (setq level-stack level)
          (insert (make-string level ?*) " "
                  (if keyword (concat keyword " ") "")
                  (format "Heading %d of file %d about topic %d"
                          index seed (% number 97))
                  (or tags "")
                  "\n")
          (when (zerop (% number 5))
            (insert "Some body text for the heading.\n")))))))

(defun org-files-db-bench--make-corpus (directory count)
  "Generate COUNT headings below DIRECTORY and return the config file."
  (let* ((files (max 1 (ceiling count org-files-db-bench--headings-per-file)))
         (remaining count)
         (notes (expand-file-name "notes" directory))
         (config (expand-file-name "orgfdb.toml" directory)))
    (make-directory notes t)
    (dotimes (index files)
      (let ((rows (min remaining
                       (ceiling remaining (- files index)))))
        (org-files-db-bench--write-file
         (expand-file-name (format "file-%03d.org" index) notes)
         rows index)
        (setq remaining (- remaining rows))))
    (with-temp-file config
      (insert "db_path = \"orgfdb.sqlite\"\n"
              "[[dirs]]\npath = \"notes\"\nrecursive = false\n"))
    config))

;;;; Raw measurements

(defun org-files-db-bench--discard-time (arguments)
  "Return seconds orgfdb needs for ARGUMENTS with stdout discarded."
  (let ((program (org-files-db-process--resolve-executable)))
    (let ((start (float-time)))
      (unless (zerop (apply #'call-process program nil (list nil nil) nil
                            arguments))
        (error "Orgfdb failed: %S" arguments))
      (- (float-time) start))))

(defun org-files-db-bench--payload-bytes (arguments)
  "Return the stdout size in bytes of orgfdb run with ARGUMENTS."
  (let ((file (make-temp-file "orgfdb-payload"))
        (program (org-files-db-process--resolve-executable)))
    (unwind-protect
        (progn
          (unless (zerop (apply #'call-process program nil (list :file file)
                                nil arguments))
            (error "Orgfdb failed: %S" arguments))
          (file-attribute-size (file-attributes file)))
      (delete-file file))))

(defun org-files-db-bench--query-arguments (config)
  "Return one-shot query arguments for CONFIG file."
  (list "query" "--format" "presentation-json"
        "--presentation-spec-json"
        (org-files-db-presentation--spec-json
         org-files-db-bench--columns nil nil)
        "--config" config
        org-files-db-bench--query))

(defun org-files-db-bench--view-arguments (entry)
  "Return `view read' arguments for cache ENTRY."
  (let ((resolved (org-files-db-cache--entry-resolved entry)))
    (list "view" "read"
          "--config" (org-files-db-views--resolved-config-file resolved)
          (org-files-db-cache--entry-rust-name entry))))

(defun org-files-db-bench--phases (arguments)
  "Measure the phases of one run of orgfdb ARGUMENTS.
Return a plist of seconds.  Keys with the suffix -gc are GC seconds."
  (let* ((orgfdb (org-files-db-bench--discard-time arguments))
         (raw (org-files-db-bench--measure
               (org-files-db-process--call-raw arguments)))
         (parsed (org-files-db-bench--measure
                  (org-files-db-process--parse-json (nth 0 raw))))
         (decoded (org-files-db-bench--measure
                   (org-files-db-presentation--decode (nth 0 parsed))))
         (candidates (org-files-db-bench--measure
                      (org-files-db-presentation--candidates
                       (nth 0 decoded)))))
    (list :orgfdb orgfdb
          :process (nth 1 raw)
          :transfer (max 0.0 (- (nth 1 raw) orgfdb))
          :parse (nth 1 parsed) :parse-gc (nth 2 parsed)
          :decode (nth 1 decoded) :decode-gc (nth 2 decoded)
          :candidates (nth 1 candidates) :candidates-gc (nth 2 candidates))))

(defun org-files-db-bench--usable (function)
  "Call FUNCTION and return seconds until completion is usable.
Replace `completing-read' so that it records the time and returns the
first candidate.  Return a plist with :total, :gc and :gcs."
  (let ((start (float-time))
        (gc-start gc-elapsed)
        (gcs-start gcs-done)
        reached gc-reached gcs-reached)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (setq reached (float-time)
                       gc-reached gc-elapsed
                       gcs-reached gcs-done)
                 (car (org-files-db-presentation--completion-candidates
                       table)))))
      (funcall function))
    (list :total (- reached start)
          :gc (- gc-reached gc-start)
          :gcs (- gcs-reached gcs-start))))

;;;; Paths

(defun org-files-db-bench--one-shot (config)
  "Measure the one-shot path for CONFIG file.  Return a list of plists."
  (let ((arguments (org-files-db-bench--query-arguments config))
        (org-files-db-configs (list (cons "bench" config)))
        (org-files-db-default-config "bench")
        runs)
    (dotimes (_ org-files-db-bench--runs)
      (garbage-collect)
      (let ((phases (org-files-db-bench--phases arguments)))
        (garbage-collect)
        (push (append
               phases
               (org-files-db-bench--usable
                (lambda ()
                  (org-files-db-query
                   org-files-db-bench--query
                   :columns org-files-db-bench--columns
                   :action #'ignore))))
              runs)))
    (nreverse runs)))

(defun org-files-db-bench--cached (config)
  "Measure the cached path for CONFIG file.  Return a list of plists.
The first element is the registration and first read, the rest are
warm reads."
  (let* ((org-files-db-configs (list (cons "bench" config)))
         (org-files-db-default-config "bench")
         (org-files-db-views
          (list (list org-files-db-bench--view-name
                      :query org-files-db-bench--query
                      :columns org-files-db-bench--columns
                      :cache t
                      :action #'ignore)))
         runs setup)
    (unwind-protect
        (progn
          (setq setup (org-files-db-bench--measure
                       (org-files-db-cache-start)))
          (let* ((entry (org-files-db-cache--entry
                         org-files-db-bench--view-name))
                 (arguments (org-files-db-bench--view-arguments entry))
                 (first-read (org-files-db-bench--measure
                              (org-files-db-process--call-raw arguments))))
            (dotimes (_ org-files-db-bench--runs)
              (garbage-collect)
              (let ((phases (org-files-db-bench--phases arguments)))
                (garbage-collect)
                (push (append
                       phases
                       (org-files-db-bench--usable
                        (lambda ()
                          (org-files-db-view
                           org-files-db-bench--view-name))))
                      runs)))
            (cons (list :register (nth 1 setup)
                        :first-read (nth 1 first-read))
                  (nreverse runs))))
      (org-files-db-cache-stop t))))

;;;; Report

(defun org-files-db-bench--format (seconds)
  "Format SECONDS for a table cell."
  (format "%.3f" seconds))

(defun org-files-db-bench--row (label results key)
  "Return a table row for LABEL using KEY from RESULTS per size."
  (concat "| " label " | "
          (mapconcat
           (lambda (runs)
             (org-files-db-bench--format
              (org-files-db-bench--median
               (mapcar (lambda (run) (plist-get run key)) runs))))
           results " | ")
          " |\n"))

(defun org-files-db-bench--table (title sizes results)
  "Return an Org table named TITLE for SIZES and RESULTS.
RESULTS is a list with one list of run plists per size."
  (concat
   (format "#+caption: %s (median of %d runs, seconds)\n" title
           org-files-db-bench--runs)
   "| Phase | " (mapconcat #'number-to-string sizes " | ") " |\n"
   "|" (mapconcat (lambda (_) "---") (cons nil sizes) "+") "|\n"
   (org-files-db-bench--row "orgfdb (stdout discarded)" results :orgfdb)
   (org-files-db-bench--row "process call (wall)" results :process)
   (org-files-db-bench--row "transfer (process - orgfdb)" results :transfer)
   (org-files-db-bench--row "JSON parse" results :parse)
   (org-files-db-bench--row "JSON parse, GC part" results :parse-gc)
   (org-files-db-bench--row "presentation decode" results :decode)
   (org-files-db-bench--row "decode, GC part" results :decode-gc)
   (org-files-db-bench--row "candidate construction" results :candidates)
   (org-files-db-bench--row "candidates, GC part" results :candidates-gc)
   (org-files-db-bench--row "time until completion usable" results :total)
   (org-files-db-bench--row "usable, GC part" results :gc)
   (org-files-db-bench--row "usable, GC runs" results :gcs)))

(defun org-files-db-bench--info (orgfdb sizes payloads rebuilds)
  "Return setup information for ORGFDB, SIZES, PAYLOADS and REBUILDS."
  (concat
   (format "Emacs: %s\n" emacs-version)
   (format "orgfdb: %s (%s)\n" orgfdb
           (string-trim
            (with-output-to-string
              (with-current-buffer standard-output
                (call-process orgfdb nil t nil "--version")))))
   (format "gc-cons-threshold: %d, gc-cons-percentage: %s\n"
           gc-cons-threshold gc-cons-percentage)
   (format "System: %s, %d CPUs\n" system-configuration
           (or (and (fboundp 'num-processors) (num-processors)) 0))
   (format "Sizes (headings): %s\n" (mapconcat #'number-to-string sizes " "))
   (format "Payload bytes: %s\n"
           (mapconcat #'number-to-string payloads " "))
   (format "Rebuild seconds: %s\n"
           (mapconcat #'org-files-db-bench--format rebuilds " "))))

;;;###autoload
(defun org-files-db-bench-run ()
  "Run the benchmark and print the report to standard output."
  (let ((orgfdb (org-files-db-bench--executable)))
    (if (not orgfdb)
        (message "orgfdb not found (set ORGFDB or add it to PATH), skipping benchmark")
      (let* ((org-files-db-executable orgfdb)
             (sizes (if-let* ((env (getenv "ORGFDB_BENCH_SIZES")))
                        (mapcar #'string-to-number (split-string env))
                      org-files-db-bench--sizes))
             payloads rebuilds one-shot cached)
        (dolist (size sizes)
          (let* ((directory (make-temp-file "orgfdb-bench" t))
                 (config (org-files-db-bench--make-corpus directory size)))
            (unwind-protect
                (progn
                  (message "Size %d: rebuild" size)
                  (push (org-files-db-bench--discard-time
                         (list "rebuild" "--config" config))
                        rebuilds)
                  (push (org-files-db-bench--payload-bytes
                         (org-files-db-bench--query-arguments config))
                        payloads)
                  (message "Size %d: one-shot" size)
                  (push (org-files-db-bench--one-shot config) one-shot)
                  (message "Size %d: cached" size)
                  (push (org-files-db-bench--cached config) cached))
              (delete-directory directory t))))
        (setq payloads (nreverse payloads)
              rebuilds (nreverse rebuilds)
              one-shot (nreverse one-shot)
              cached (nreverse cached))
        (princ (org-files-db-bench--info orgfdb sizes payloads rebuilds))
        (princ "\n")
        (princ (org-files-db-bench--table
                "One-shot query" sizes one-shot))
        (princ "\n")
        (princ (org-files-db-bench--table
                "Cached view read" sizes (mapcar #'cdr cached)))
        (princ "\n")
        (dolist (key '(:register :first-read))
          (princ (format "Cached %s seconds: %s\n" key
                         (mapconcat
                          (lambda (entry)
                            (org-files-db-bench--format
                             (plist-get (car entry) key)))
                          cached " "))))))))

(provide 'org-files-db-bench)

;;; org-files-db-bench.el ends here
