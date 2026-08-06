;;; org-files-db-benchmark.el --- Presentation benchmarks -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-only

;;; Commentary:

;; Development-only benchmarks for eager org-files-db result presentation.
;; Load this file from the repository root with:
;;
;;   emacs -Q -L lisp -l benchmarks/org-files-db-benchmark.el
;;
;; Then evaluate `org-files-db-benchmark-run' or call it interactively.

;;; Code:

(require 'cl-lib)
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
  (let (results)
    (dotimes (index count (nreverse results))
      (push (org-files-db-benchmark--result index) results))))

(defun org-files-db-benchmark--phase (summary phase)
  "Return PHASE statistics from benchmark SUMMARY."
  (cdr (assq phase (plist-get summary :phases))))

(defun org-files-db-benchmark--seconds (value)
  "Format floating-point seconds VALUE compactly."
  (format "%.6f" value))

(defun org-files-db-benchmark--insert-summary (label summary)
  "Insert LABEL and presentation benchmark SUMMARY in the current buffer."
  (insert (format "\n%s\n" label))
  (insert (make-string (length label) ?-) "\n")
  (insert (format "Rows: %d, columns: %d, iterations: %d, GCs: %d\n"
                  (plist-get summary :result-count)
                  (plist-get summary :column-count)
                  (plist-get summary :iterations)
                  (plist-get summary :garbage-collections)))
  (insert (format "%-36s %12s %12s %12s\n"
                  "Phase" "Minimum" "Median" "Maximum"))
  (dolist (phase org-files-db--benchmark-phases)
    (let ((statistics (org-files-db-benchmark--phase summary phase)))
      (insert
       (format "%-36s %12s %12s %12s\n"
               phase
               (org-files-db-benchmark--seconds
                (plist-get statistics :minimum))
               (org-files-db-benchmark--seconds
                (plist-get statistics :median))
               (org-files-db-benchmark--seconds
                (plist-get statistics :maximum)))))))

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

(defun org-files-db-benchmark--time-json (text object-type columns iterations)
  "Benchmark TEXT with OBJECT-TYPE and COLUMNS for ITERATIONS."
  (let ((gc-before (if (boundp 'gcs-done)
                       (symbol-value 'gcs-done)
                     0))
        parse-times direct-times nested-times complete-times)
    (dotimes (_ iterations)
      (let ((started (float-time)) parsed results)
        (setq parsed (org-files-db--parse-json-as text object-type))
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
          :garbage-collections
          (- (if (boundp 'gcs-done)
                 (symbol-value 'gcs-done)
               0)
             gc-before)
          :parse
          (org-files-db--benchmark-phase-summary
           (mapcar (lambda (value) (list :value value)) parse-times)
           :value)
          :direct-access
          (org-files-db--benchmark-phase-summary
           (mapcar (lambda (value) (list :value value)) direct-times)
           :value)
          :nested-access
          (org-files-db--benchmark-phase-summary
           (mapcar (lambda (value) (list :value value)) nested-times)
           :value)
          :presentation
          (org-files-db--benchmark-phase-summary
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
      (concat "%-12s parse=%s, direct access=%s, nested access=%s, "
              "presentation=%s, GCs=%d\n")
      (plist-get summary :object-type)
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
             (org-files-db--benchmark-presentation
              results org-files-db-benchmark--basic-columns
              :iterations iterations))
            (org-files-db-benchmark--insert-summary
             (format "%d rows, expensive columns" count)
             (org-files-db--benchmark-presentation
              results org-files-db-benchmark--expensive-columns
              :iterations iterations))))
        (insert "\nRepresentative JSON response\n")
        (insert "----------------------------\n")
        (let ((text (org-files-db-benchmark--fixture-text)))
          (dolist (object-type '(alist hash-table))
            (org-files-db-benchmark--insert-json-summary
             (org-files-db-benchmark--time-json
              text object-type org-files-db-benchmark--expensive-columns
              iterations))))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

(defun org-files-db-benchmark--phase-summary-from-values (values)
  "Return minimum, median, and maximum timing statistics for VALUES."
  (org-files-db--benchmark-phase-summary
   (mapcar (lambda (value) (list :value value)) values)
   :value))

(defun org-files-db-benchmark--insert-real-query-summary (summary)
  "Insert real-query benchmark SUMMARY in the current buffer."
  (insert "\nReal orgfdb query\n")
  (insert "-----------------\n")
  (insert (format "Rows: %d, iterations: %d, GCs: %d\n"
                  (plist-get summary :result-count)
                  (plist-get summary :iterations)
                  (plist-get summary :garbage-collections)))
  (insert (format "%-24s %12s %12s %12s\n"
                  "Phase" "Minimum" "Median" "Maximum"))
  (dolist (phase '(:column-normalization :cli-execution :json-parsing
                   :result-normalization :presentation :total))
    (let ((statistics (cdr (assq phase (plist-get summary :phases)))))
      (insert
       (format "%-24s %12s %12s %12s\n"
               phase
               (org-files-db-benchmark--seconds
                (plist-get statistics :minimum))
               (org-files-db-benchmark--seconds
                (plist-get statistics :median))
               (org-files-db-benchmark--seconds
                (plist-get statistics :maximum)))))))

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
         (gc-before (if (boundp 'gcs-done)
                        (symbol-value 'gcs-done)
                      0))
         column-times cli-times parse-times normalization-times
         presentation-times total-times result-count)
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
        (org-files-db--prepare-presentation results columns)
        (push (- (float-time) started) presentation-times)
        (push (- (float-time) total-started) total-times)))
    (let ((summary
           (list
            :result-count result-count
            :iterations iterations
            :garbage-collections
            (- (if (boundp 'gcs-done)
                   (symbol-value 'gcs-done)
                 0)
               gc-before)
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
                    total-times))))))
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

(provide 'org-files-db-benchmark)

;;; org-files-db-benchmark.el ends here
