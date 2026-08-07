;;; org-files-db-benchmark.el --- Presentation benchmarks -*- lexical-binding: t; -*-

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

;; Development-only benchmarks for eager org-files-db result presentation.
;; Load this file alongside the package sources, then evaluate
;; `org-files-db-benchmark-run' or call it interactively.

;;; Code:

(require 'cl-lib)
(require 'org-files-db-cache)
(require 'org-files-db-core)

(declare-function consult--lookup-candidate "consult"
                  (selected candidates input narrow))
(declare-function consult--read "consult" (candidates &rest options))

(defconst org-files-db-benchmark--counts '(100 1000 10000 50000)
  "Synthetic result counts used by the presentation benchmark.")

(defconst org-files-db-benchmark--basic-columns
  '((todo-keyword)
    (title)
    (file-name :width (max 30)))
  "Representative low-cost benchmark columns.")

(defconst org-files-db-benchmark--expensive-columns
  '((todo-keyword)
    (priority :width (fixed 3))
    (outline-path :width (max 80))
    (tags)
    (file-name :width (max 30)))
  "Representative higher-cost benchmark columns.")

(defconst org-files-db-benchmark--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this benchmark file.")

(defconst org-files-db-benchmark--phases
  '(:column-normalization
    :presentation-source-construction
    :presentation-row-construction
    :result-value-extraction
    :sort-key-preparation
    :sorting
    :shared-width-calculation
    :face-preparation
    :candidate-formatting
    :boundary-garbage-collection
    :completion-empty
    :completion-filter
    :total)
  "Presentation timing phases reported by benchmark helpers.")

(defconst org-files-db-benchmark--real-query-phases
  '(:column-normalization
    :cli-execution
    :json-parsing
    :result-normalization
    :presentation
    :total)
  "Top-level timing phases reported for real queries.")

(defconst org-files-db-benchmark--allocation-metric-keys
  '(:conses :floats :vector-cells :symbols :string-characters
            :intervals :strings)
  "Keys corresponding to the counters returned by `memory-use-counts'.")

(defun org-files-db-benchmark--allocation-metrics (&optional before)
  "Return an allocation snapshot or the allocation delta since BEFORE."
  (if (null before)
      (memory-use-counts)
    (let ((after (memory-use-counts))
          (keys org-files-db-benchmark--allocation-metric-keys)
          result)
      (while before
        (setq result
              (append result
                      (list (pop keys)
                            (- (pop after) (pop before))))))
      result)))

(defun org-files-db-benchmark--completion (candidates input)
  "Return seconds needed to complete INPUT against prepared CANDIDATES."
  (let ((completion-styles '(substring))
        (table (org-files-db--completion-table candidates))
        (started (float-time)))
    (completion-all-completions input table nil (length input))
    (org-files-db--elapsed-seconds started)))

(defun org-files-db-benchmark--phase-summary (runs phase)
  "Return timing statistics for PHASE across benchmark RUNS."
  (let* ((values
          (sort
           (mapcar (lambda (run) (plist-get run phase)) runs)
           #'<))
         (count (length values))
         (middle (/ count 2))
         (median
          (if (cl-oddp count)
              (nth middle values)
            (/ (+ (nth (1- middle) values)
                  (nth middle values))
               2.0))))
    (list :minimum (car values)
          :median median
          :maximum (car (last values)))))

(defun org-files-db-benchmark--presentation-metric-summary (runs phase)
  "Summarize allocation and GC metrics for PHASE across benchmark RUNS."
  (let ((metrics
         (delq
          nil
          (mapcar
           (lambda (run)
             (seq-find
              (lambda (metric) (eq (plist-get metric :phase) phase))
              (plist-get run :phase-metrics)))
           runs))))
    (when metrics
      (list
       :garbage-collections
       (org-files-db-benchmark--phase-summary metrics :garbage-collections)
       :garbage-collection-time
       (org-files-db-benchmark--phase-summary
        metrics :garbage-collection-time)
       :allocation
       (mapcar
        (lambda (key)
          (cons
           key
           (org-files-db-benchmark--phase-summary
            (mapcar
             (lambda (metric)
               (list key (or (plist-get (plist-get metric :allocation) key) 0)))
             metrics)
            key)))
        org-files-db-benchmark--allocation-metric-keys)))))

(cl-defun org-files-db-benchmark--presentation
    (results columns &key (iterations 5))
  "Benchmark eager presentation of RESULTS with COLUMNS.
Return minimum, median, and maximum timings across ITERATIONS without opening
completion."
  (unless (and (integerp iterations) (> iterations 0))
    (user-error "Benchmark iterations must be a positive integer"))
  (let ((org-files-db--presentation-allocation-metrics-function
         #'org-files-db-benchmark--allocation-metrics)
        runs candidate-character-count)
    (dotimes (_ iterations)
      (garbage-collect)
      (let* ((gc-before (if (boundp 'gcs-done) gcs-done 0))
             (gc-time-before (if (boundp 'gc-elapsed) gc-elapsed 0.0))
             (presentation
              (org-files-db--prepare-presentation results columns))
             (candidates
              (org-files-db--presentation-candidates presentation))
             (timings
              (append
               (org-files-db--presentation-timings presentation)
               (list
                :completion-empty
                (org-files-db-benchmark--completion candidates "")
                :completion-filter
                (org-files-db-benchmark--completion candidates "Heading 999")
                :garbage-collections
                (- (if (boundp 'gcs-done) gcs-done 0) gc-before)
                :garbage-collection-time
                (- (if (boundp 'gc-elapsed) gc-elapsed 0.0)
                   gc-time-before)
                :phase-metrics
                (org-files-db--presentation-phase-metrics presentation)))))
        (setq candidate-character-count 0)
        (dolist (candidate candidates)
          (setq candidate-character-count
                (+ candidate-character-count (length candidate))))
        (push timings runs)))
    (setq runs (nreverse runs))
    (list :result-count (length results)
          :column-count (length columns)
          :iterations iterations
          :garbage-collections
          (cl-loop for run in runs
                   sum (plist-get run :garbage-collections))
          :garbage-collection-time
          (cl-loop for run in runs
                   sum (plist-get run :garbage-collection-time))
          :candidate-characters candidate-character-count
          :phases
          (mapcar
           (lambda (phase)
             (cons phase
                   (org-files-db-benchmark--phase-summary runs phase)))
           org-files-db-benchmark--phases)
          :phase-metrics
          (mapcar
           (lambda (metric)
             (let ((phase (plist-get metric :phase)))
               (cons
                phase
                (org-files-db-benchmark--presentation-metric-summary
                 runs phase))))
           (plist-get (car runs) :phase-metrics))
          :runs runs)))

(defun org-files-db-benchmark--title (index)
  "Return a representative title for synthetic result INDEX."
  (cond
   ((zerop (% index 100)) "Duplicate title")
   ((zerop (% index 31)) "Überblick 東京")
   ((zerop (% index 17)) "Résumé with a longer descriptive title")
   (t (format "Heading %d" index))))

(defun org-files-db-benchmark--file (index)
  "Return a representative file path for synthetic result INDEX."
  (format "/tmp/org-files-db/%02d/shared-%03d.org"
          (% index 37)
          (% index 200)))

(defun org-files-db-benchmark--result (index)
  "Return one deterministic synthetic result for INDEX."
  (let* ((title (org-files-db-benchmark--title index))
         (file (org-files-db-benchmark--file index))
         (level (1+ (% index 6)))
         (todo (cond
                ((zerop (% index 5)) "NEXT")
                ((zerop (% index 7)) "DONE")
                (t nil)))
         (todo-type (and todo (if (equal todo "DONE") "closed" "open")))
         (priority (and (zerop (% index 3))
                        (char-to-string (+ ?A (% (/ index 3) 3)))))
         (tags (unless (zerop (% index 11))
                 (list (format "tag-%d" (% index 13))
                       (if (zerop (% index 9)) "über" "project")))))
    `((kind . "heading")
      (id . ,index)
      (file_id . ,(% index 500))
      (level . ,level)
      (title . ,title)
      (todo_keyword . ,todo)
      (todo_type . ,todo-type)
      (priority . ,priority)
      (all_tags . ,tags)
      (location . ((file_path . ,file)
                   (line . ,(1+ index))
                   (byte_start . ,(* index 24))))
      (node_path
       . (((kind . "file") (title . "Notes"))
          ((kind . "heading") (title . "Projects") (level . 1))
          ((kind . "heading") (title . "Editors") (level . 2))
          ((kind . "heading") (title . ,title) (level . ,level)))))))

(defun org-files-db-benchmark--results (count)
  "Return COUNT deterministic synthetic results."
  (cl-loop for index below count
           collect (org-files-db-benchmark--result index)))

(defun org-files-db-benchmark--phase (summary phase)
  "Return PHASE statistics from benchmark SUMMARY."
  (cdr (assq phase (plist-get summary :phases))))

(defun org-files-db-benchmark--seconds (value)
  "Format floating-point seconds VALUE compactly."
  (format "%.6f" value))

(defun org-files-db-benchmark--insert-metric-summary (metrics)
  "Insert median phase allocation and GC METRICS in the current buffer."
  (insert "\nMedian phase allocation and GC\n")
  (insert (format "%-36s %6s %10s %12s %12s %12s\n"
                  "Phase" "GCs" "GC time" "Conses"
                  "Vector cells" "String chars"))
  (dolist (entry metrics)
    (let* ((phase (car entry))
           (phase-metrics (cdr entry))
           (allocation (plist-get phase-metrics :allocation)))
      (insert
       (format
        "%-36s %6s %10s %12d %12d %12d\n"
        phase
        (plist-get
         (plist-get phase-metrics :garbage-collections) :median)
        (org-files-db-benchmark--seconds
         (plist-get
          (plist-get phase-metrics :garbage-collection-time) :median))
        (plist-get (cdr (assq :conses allocation)) :median)
        (plist-get (cdr (assq :vector-cells allocation)) :median)
        (plist-get (cdr (assq :string-characters allocation)) :median))))))

(defun org-files-db-benchmark--insert-summary (label summary)
  "Insert LABEL and presentation benchmark SUMMARY in the current buffer."
  (insert (format "\n%s\n" label))
  (insert (make-string (length label) ?-) "\n")
  (insert (format (concat "Rows: %d, columns: %d, iterations: %d, "
                          "GCs: %d, GC time: %s, candidate chars: %d\n")
                  (plist-get summary :result-count)
                  (plist-get summary :column-count)
                  (plist-get summary :iterations)
                  (plist-get summary :garbage-collections)
                  (org-files-db-benchmark--seconds
                   (plist-get summary :garbage-collection-time))
                  (plist-get summary :candidate-characters)))
  (insert (format "%-36s %12s %12s %12s\n"
                  "Phase" "Minimum" "Median" "Maximum"))
  (dolist (phase org-files-db-benchmark--phases)
    (let ((statistics (org-files-db-benchmark--phase summary phase)))
      (insert
       (format "%-36s %12s %12s %12s\n"
               phase
               (org-files-db-benchmark--seconds
                (plist-get statistics :minimum))
               (org-files-db-benchmark--seconds
                (plist-get statistics :median))
               (org-files-db-benchmark--seconds
                (plist-get statistics :maximum))))))
  (org-files-db-benchmark--insert-metric-summary
   (plist-get summary :phase-metrics)))

(defun org-files-db-benchmark--time-json-access
    (results keys repetitions)
  "Measure repeated access to KEYS in RESULTS for REPETITIONS."
  (let ((started (float-time))
        observed)
    (dotimes (_ repetitions)
      (dolist (result results)
        (setq observed (apply #'org-files-db--get result keys))))
    (ignore observed)
    (- (float-time) started)))

(defun org-files-db-benchmark--time-json
    (text object-type array-type columns iterations)
  "Benchmark TEXT with OBJECT-TYPE, ARRAY-TYPE, and COLUMNS for ITERATIONS."
  (let ((gc-before (if (boundp 'gcs-done)
                       (symbol-value 'gcs-done)
                     0))
        parse-times direct-times nested-times complete-times)
    (dotimes (_ iterations)
      (let ((started (float-time)) parsed results)
        (setq parsed
              (org-files-db--parse-json-as text object-type array-type))
        (push (- (float-time) started) parse-times)
        (setq results (org-files-db--normalize-results parsed))
        (push (org-files-db-benchmark--time-json-access
               results '(title) 1000)
              direct-times)
        (push (org-files-db-benchmark--time-json-access
               results '(location file_path) 1000)
              nested-times)
        (setq started (float-time))
        (org-files-db--prepare-presentation results columns)
        (push (- (float-time) started) complete-times)))
    (list :object-type object-type
          :array-type array-type
          :garbage-collections
          (- (if (boundp 'gcs-done)
                 (symbol-value 'gcs-done)
               0)
             gc-before)
          :parse
          (org-files-db-benchmark--phase-summary
           (mapcar (lambda (value) (list :value value)) parse-times)
           :value)
          :direct-access
          (org-files-db-benchmark--phase-summary
           (mapcar (lambda (value) (list :value value)) direct-times)
           :value)
          :nested-access
          (org-files-db-benchmark--phase-summary
           (mapcar (lambda (value) (list :value value)) nested-times)
           :value)
          :presentation
          (org-files-db-benchmark--phase-summary
           (mapcar (lambda (value) (list :value value)) complete-times)
           :value))))

(defun org-files-db-benchmark--insert-json-summary (summary)
  "Insert JSON representation benchmark SUMMARY."
  (let ((parse (plist-get summary :parse))
        (direct (plist-get summary :direct-access))
        (nested (plist-get summary :nested-access))
        (presentation (plist-get summary :presentation)))
    (insert
     (format
      (concat "%-12s/%-6s parse=%s, direct access=%s, nested access=%s, "
              "presentation=%s, GCs=%d\n")
      (plist-get summary :object-type)
      (plist-get summary :array-type)
      (org-files-db-benchmark--seconds (plist-get parse :median))
      (org-files-db-benchmark--seconds (plist-get direct :median))
      (org-files-db-benchmark--seconds (plist-get nested :median))
      (org-files-db-benchmark--seconds
       (plist-get presentation :median))
      (plist-get summary :garbage-collections)))))

(defun org-files-db-benchmark--fixture-text ()
  "Return the representative orgfdb response fixture text."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name
      "fixtures/representative-query-response.json"
      org-files-db-benchmark--directory))
    (buffer-string)))

;;;###autoload
(defun org-files-db-benchmark-run (&optional iterations)
  "Run presentation benchmarks and display a report.
ITERATIONS defaults to three.  The 50,000-row case may take substantial time."
  (interactive "P")
  (let ((iterations (or (and iterations (prefix-numeric-value iterations)) 3))
        (buffer (get-buffer-create "*org-files-db benchmark*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "org-files-db eager presentation benchmark\n")
        (insert (format "Emacs: %s\n" emacs-version))
        (insert (format "Iterations: %d\n" iterations))
        (dolist (count org-files-db-benchmark--counts)
          (let ((results (org-files-db-benchmark--results count)))
            (org-files-db-benchmark--insert-summary
             (format "%d rows, basic columns" count)
             (org-files-db-benchmark--presentation
              results org-files-db-benchmark--basic-columns
              :iterations iterations))
            (org-files-db-benchmark--insert-summary
             (format "%d rows, expensive columns" count)
             (org-files-db-benchmark--presentation
              results org-files-db-benchmark--expensive-columns
              :iterations iterations))))
        (insert "\nRepresentative JSON response\n")
        (insert "----------------------------\n")
        (let ((text (org-files-db-benchmark--fixture-text)))
          (dolist (object-type '(alist hash-table))
            (dolist (array-type '(list array))
              (org-files-db-benchmark--insert-json-summary
               (org-files-db-benchmark--time-json
                text object-type array-type
                org-files-db-benchmark--expensive-columns iterations)))))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

(defun org-files-db-benchmark--phase-summary-from-values (values)
  "Return minimum, median, and maximum timing statistics for VALUES."
  (org-files-db-benchmark--phase-summary
   (mapcar (lambda (value) (list :value value)) values)
   :value))

(defun org-files-db-benchmark--insert-real-query-summary (summary)
  "Insert real-query benchmark SUMMARY in the current buffer."
  (insert "\nReal orgfdb query\n")
  (insert "-----------------\n")
  (insert (format (concat "Rows: %d, iterations: %d, GCs: %d, "
                          "GC time: %s, candidate chars: %d\n")
                  (plist-get summary :result-count)
                  (plist-get summary :iterations)
                  (plist-get summary :garbage-collections)
                  (org-files-db-benchmark--seconds
                   (plist-get summary :garbage-collection-time))
                  (plist-get summary :candidate-characters)))
  (insert (format "%-24s %12s %12s %12s\n"
                  "Phase" "Minimum" "Median" "Maximum"))
  (dolist (phase org-files-db-benchmark--real-query-phases)
    (let ((statistics (cdr (assq phase (plist-get summary :phases)))))
      (insert
       (format "%-24s %12s %12s %12s\n"
               phase
               (org-files-db-benchmark--seconds
                (plist-get statistics :minimum))
               (org-files-db-benchmark--seconds
                (plist-get statistics :median))
               (org-files-db-benchmark--seconds
                (plist-get statistics :maximum))))))
  (insert "\nPresentation subphases\n")
  (insert (format "%-36s %12s %12s %12s\n"
                  "Phase" "Minimum" "Median" "Maximum"))
  (dolist (entry (plist-get summary :presentation-phases))
    (let ((statistics (cdr entry)))
      (insert
       (format "%-36s %12s %12s %12s\n"
               (car entry)
               (org-files-db-benchmark--seconds
                (plist-get statistics :minimum))
               (org-files-db-benchmark--seconds
                (plist-get statistics :median))
               (org-files-db-benchmark--seconds
                (plist-get statistics :maximum))))))
  (org-files-db-benchmark--insert-metric-summary
   (plist-get summary :presentation-phase-metrics)))

;;;###autoload
(defun org-files-db-benchmark-query (query &optional iterations)
  "Benchmark one real synchronous orgfdb QUERY.
Use the representative basic columns and repeat ITERATIONS times, defaulting
to three.  The current `org-files-db-config-file' controls the database."
  (interactive
   (list (org-files-db--read-sexp "Benchmark query: ")
         (when current-prefix-arg
           (prefix-numeric-value current-prefix-arg))))
  (let* ((iterations (or iterations 3))
         (config-file
          (org-files-db--resolve-config-file nil nil "Benchmark query"))
         (gc-before (if (boundp 'gcs-done) gcs-done 0))
         (gc-time-before (if (boundp 'gc-elapsed) gc-elapsed 0.0))
         column-times cli-times parse-times normalization-times
         presentation-times presentation-runs total-times result-count
         candidate-character-count)
    (unless (and (integerp iterations) (> iterations 0))
      (user-error "Benchmark iterations must be a positive integer"))
    (dotimes (_ iterations)
      (let ((total-started (float-time))
            started columns includes arguments text response results)
        (setq started (float-time)
              columns
              (org-files-db--normalize-columns
               org-files-db-benchmark--basic-columns))
        (push (- (float-time) started) column-times)
        (setq includes (org-files-db--column-includes columns)
              arguments
              (org-files-db--query-arguments
               query config-file "Benchmark query" includes)
              started (float-time)
              text (org-files-db--call-raw (cons "query" arguments)))
        (push (- (float-time) started) cli-times)
        (setq started (float-time)
              response (org-files-db--parse-json text))
        (push (- (float-time) started) parse-times)
        (setq started (float-time)
              results
              (org-files-db--results-with-config
               (org-files-db--normalize-results response)
               config-file)
              result-count (length results))
        (push (- (float-time) started) normalization-times)
        (setq started (float-time))
        (let* ((org-files-db--presentation-allocation-metrics-function
                #'org-files-db-benchmark--allocation-metrics)
               (presentation
                (org-files-db--prepare-presentation results columns))
               (candidates
                (org-files-db--presentation-candidates presentation)))
          (setq candidate-character-count 0)
          (dolist (candidate candidates)
            (setq candidate-character-count
                  (+ candidate-character-count (length candidate))))
          (push
           (append
            (org-files-db--presentation-timings presentation)
            (list :phase-metrics
                  (org-files-db--presentation-phase-metrics presentation)))
           presentation-runs))
        (push (- (float-time) started) presentation-times)
        (push (- (float-time) total-started) total-times)))
    (let ((summary
           (list
            :result-count result-count
            :iterations iterations
            :garbage-collections
            (- (if (boundp 'gcs-done) gcs-done 0) gc-before)
            :garbage-collection-time
            (- (if (boundp 'gc-elapsed) gc-elapsed 0.0) gc-time-before)
            :candidate-characters candidate-character-count
            :phases
            (list
             (cons :column-normalization
                   (org-files-db-benchmark--phase-summary-from-values
                    column-times))
             (cons :cli-execution
                   (org-files-db-benchmark--phase-summary-from-values
                    cli-times))
             (cons :json-parsing
                   (org-files-db-benchmark--phase-summary-from-values
                    parse-times))
             (cons :result-normalization
                   (org-files-db-benchmark--phase-summary-from-values
                    normalization-times))
             (cons :presentation
                   (org-files-db-benchmark--phase-summary-from-values
                    presentation-times))
             (cons :total
                   (org-files-db-benchmark--phase-summary-from-values
                    total-times)))
            :presentation-phases
            (mapcar
             (lambda (phase)
               (cons
                phase
                (org-files-db-benchmark--phase-summary
                 presentation-runs phase)))
             (seq-filter
              (lambda (phase)
                (not (memq phase '(:completion-empty :completion-filter))))
              org-files-db-benchmark--phases))
            :presentation-phase-metrics
            (mapcar
             (lambda (metric)
               (let ((phase (plist-get metric :phase)))
                 (cons
                  phase
                  (org-files-db-benchmark--presentation-metric-summary
                   presentation-runs phase))))
             (plist-get (car presentation-runs) :phase-metrics)))))
      (with-current-buffer (get-buffer-create "*org-files-db benchmark*")
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (org-files-db-benchmark--insert-real-query-summary summary))
        (special-mode)
        (display-buffer (current-buffer)))
      summary)))

(defun org-files-db-benchmark--frontend-once (candidates frontend)
  "Measure one FRONTEND initialization using prepared CANDIDATES."
  (let ((started (float-time))
        elapsed)
    (condition-case nil
        (minibuffer-with-setup-hook
            (lambda ()
              (setq elapsed (- (float-time) started))
              (abort-recursive-edit))
          (pcase frontend
            ('standard
             (completing-read
              "Benchmark: "
              (org-files-db--completion-table candidates)
              nil t))
            ('consult
             (unless (require 'consult nil t)
               (user-error "Consult is not installed"))
             (consult--read
              candidates
              :prompt "Benchmark: "
              :category org-files-db--completion-category
              :require-match t
              :sort nil
              :lookup #'consult--lookup-candidate))
            (_ (user-error "Unsupported completion frontend: %S" frontend))))
      (quit nil))
    elapsed))

;;;###autoload
(defun org-files-db-benchmark-compare-frontends (&optional count iterations)
  "Compare completion initialization for COUNT prepared rows.
COUNT defaults to 10,000 and ITERATIONS defaults to three.  Run this command
interactively because each measurement briefly opens a minibuffer."
  (interactive)
  (let ((count (or count 10000))
        (iterations (or iterations 3)))
    (unless (and (integerp count) (> count 0))
      (user-error "Benchmark row count must be a positive integer"))
    (unless (and (integerp iterations) (> iterations 0))
      (user-error "Benchmark iterations must be a positive integer"))
    (let* ((presentation
            (org-files-db--prepare-presentation
             (org-files-db-benchmark--results count)
             org-files-db-benchmark--basic-columns))
           (candidates
            (org-files-db--presentation-candidates presentation)))
      (dolist (frontend '(standard consult))
        (when (or (eq frontend 'standard)
                  (require 'consult nil t))
          (let (times)
            (dotimes (_ iterations)
              (push
               (org-files-db-benchmark--frontend-once candidates frontend)
               times))
            (message "%s completion initialization: %S"
                     frontend
                     (sort times #'<))))))))

(defun org-files-db-benchmark--time-call (function iterations)
  "Return timing summary for calling FUNCTION over ITERATIONS."
  (let (times)
    (dotimes (_ iterations)
      (garbage-collect)
      (let ((started (float-time)))
        (funcall function)
        (push (- (float-time) started) times)))
    (org-files-db-benchmark--phase-summary-from-values times)))

;;;###autoload
(defun org-files-db-benchmark-view-cache (&optional count iterations)
  "Benchmark cold, warm, patched, and rebuilt prepared-view paths.
COUNT defaults to 10,000 rows and ITERATIONS defaults to three.  This helper
uses deterministic synthetic results and does not execute orgfdb."
  (interactive)
  (let* ((count (or count 10000))
         (iterations (or iterations 3))
         (results (org-files-db-benchmark--results count))
         (columns org-files-db-benchmark--expensive-columns)
         (normalized (org-files-db--normalize-columns columns))
         (state '(:database-id "benchmark" :generation 1))
         (view `("benchmark"
                 :command query
                 :pre-cache t
                 :query (headings)
                 :columns ,columns))
         (cold
          (org-files-db-benchmark--time-call
           (lambda ()
             (org-files-db-cache--prepare results normalized))
           iterations))
         (presentation
          (org-files-db-cache--prepare results normalized))
         (entry
          (make-org-files-db-cache--entry
           :key (org-files-db-cache--cache-key view nil state)
           :view-name "benchmark"
           :view-token (org-files-db-cache--view-token view nil)
           :database-id "benchmark"
           :generation 1
           :complete-p t
           :results results
           :columns normalized
           :presentation presentation
           :result-count count
           :estimated-memory
           (org-files-db-cache--estimated-memory results presentation)
           :last-used 0.0))
         (warm
          (org-files-db-benchmark--time-call
           (lambda ()
             (unless
                 (org-files-db-cache--entry-current-p
                  entry state (org-files-db-cache--entry-key entry))
               (error "Benchmark cache unexpectedly stale"))
             (org-files-db--presentation-candidates
              (org-files-db-cache--entry-presentation entry)))
           iterations))
         (warm-completion
          (org-files-db-benchmark--time-call
           (lambda ()
             (org-files-db-benchmark--completion
              (org-files-db--presentation-candidates presentation)
              "Heading 999"))
           iterations))
         (affected
          (list (org-files-db-cache--result-owner-path (car results))))
         (replacement
          (seq-filter
           (lambda (result)
             (member (org-files-db-cache--result-owner-path result) affected))
           results))
         (logical-preparation
          (org-files-db-benchmark--time-call
           (lambda ()
             (org-files-db-cache--prepare-logical-data results normalized))
           iterations))
         (full-logical
          (org-files-db-cache--prepare-logical-data results normalized))
         (worker-payload
          (list :ok t
                :database-id "benchmark"
                :source-generation 0
                :target-generation 1
                :cache-key "benchmark-key"
                :view-token "benchmark-view"
                :refresh-token 1
                :refresh-type 'full
                :logical-data full-logical))
         (worker-transfer
          (org-files-db-benchmark--time-call
           (lambda ()
             (let ((file (make-temp-file "org-files-db-benchmark-cache-")))
               (unwind-protect
                   (progn
                     (org-files-db-cache--write-data-file file worker-payload)
                     (org-files-db-cache--read-data-file file))
                 (org-files-db-cache--delete-transport-file file))))
           iterations))
         (logical
          (org-files-db-cache--prepare-logical-data replacement normalized))
         (publication
          (org-files-db-benchmark--time-call
           (lambda ()
             (org-files-db-cache--presentation-from-logical-data full-logical))
           iterations))
         (patch
          (org-files-db-benchmark--time-call
           (lambda ()
             (let* ((restricted-rows
                     (org-files-db-cache--rows-from-logical-data logical))
                    (rows
                     (org-files-db-cache--apply-patch-rows
                      entry affected nil restricted-rows)))
               (org-files-db-cache--presentation-from-rows rows normalized)))
           iterations))
         (rebuild
          (org-files-db-benchmark--time-call
           (lambda ()
             (org-files-db-cache--prepare results normalized))
           iterations))
         (buffer (get-buffer-create "*org-files-db cache benchmark*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "org-files-db prepared view cache benchmark\n")
        (insert (format "Rows: %d, iterations: %d\n" count iterations))
        (insert (format "Estimated retained bytes: %d\n\n"
                        (org-files-db-cache--entry-estimated-memory entry)))
        (dolist (measurement
                 `((cold . ,cold)
                   (warm . ,warm)
                   (warm-completion . ,warm-completion)
                   (logical-preparation . ,logical-preparation)
                   (worker-transfer . ,worker-transfer)
                   (main-publication . ,publication)
                   (patch . ,patch)
                   (rebuild . ,rebuild)))
          (insert
           (format "%-20s min=%s median=%s max=%s\n"
                   (car measurement)
                   (org-files-db-benchmark--seconds
                    (plist-get (cdr measurement) :minimum))
                   (org-files-db-benchmark--seconds
                    (plist-get (cdr measurement) :median))
                   (org-files-db-benchmark--seconds
                    (plist-get (cdr measurement) :maximum)))))
        (special-mode)))
    (pop-to-buffer buffer)
    (list :count count :iterations iterations
          :estimated-memory
          (org-files-db-cache--entry-estimated-memory entry)
          :cold cold :warm warm :warm-completion warm-completion
          :logical-preparation logical-preparation
          :worker-transfer worker-transfer
          :main-publication publication
          :patch patch :rebuild rebuild)))

(provide 'org-files-db-benchmark)

;;; org-files-db-benchmark.el ends here
