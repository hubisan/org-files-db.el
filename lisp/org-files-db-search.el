;;; org-files-db-search.el --- Full text search for org-files-db -*- lexical-binding: t; -*-

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

;; Full text search over heading titles and bodies with `orgfdb search
;; --occurrences'.  Each candidate is one matched occurrence; selecting it
;; opens the source at the match, not only at the owning heading.

;;; Code:

(require 'cl-lib)
(require 'char-fold)
(require 'seq)
(require 'subr-x)
(require 'org-files-db-core)
(require 'org-files-db-process)
(require 'org-files-db-actions)

(declare-function ivy-read "ext:ivy")
(defvar ivy-mode)

(defvar org-files-db-search--history nil
  "History for `org-files-db-search' expressions.")

(cl-defstruct (org-files-db-search-occurrence
               (:constructor org-files-db-search--make-occurrence))
  "One matched title or body occurrence of `orgfdb search'.
FIELD is `title' or `body'.  LINE and BYTE-START locate the match in FILE;
they fall back to the owning heading when the match has no source position.
SNIPPET is the matched text line, with the match from SNIPPET-START to
SNIPPET-END in characters.  RANGES lists all matches in SNIPPET as
\(START . END) character offsets, more than one when matches on the same
line are grouped.  MATCHED lists the matched texts of all grouped
matches, also of those whose RANGES are not in SNIPPET."
  ranges
  matched
  heading-id
  title
  file
  field
  line
  byte-start
  snippet
  snippet-start
  snippet-end)

(defun org-files-db-search--field (row key)
  "Return the value of KEY in the JSON object ROW, nil for JSON null."
  (alist-get key row))

(defun org-files-db-search--row-occurrences (row)
  "Return the occurrences of one search result ROW.
A row without occurrences yields one occurrence at its heading."
  (let* ((location (org-files-db-search--field row 'location))
         (file (org-files-db-search--field location 'file_path))
         (title (or (org-files-db-search--field row 'title) ""))
         (heading-line (org-files-db-search--field location 'line))
         (heading-byte (org-files-db-search--field location 'byte_start))
         (occurrences (append (org-files-db-search--field row 'occurrences) nil)))
    (if (null occurrences)
        (list (org-files-db-search--make-occurrence
               :heading-id (org-files-db-search--field row 'id)
               :title title :file file :field 'title
               :line heading-line :byte-start heading-byte
               :snippet title :snippet-start 0 :snippet-end 0 :ranges nil :matched nil))
      (mapcar
       (lambda (occurrence)
         (let* ((line (org-files-db-search--field occurrence 'line))
                (snippet (or (org-files-db-search--field occurrence 'snippet) ""))
                (start (org-files-db-search--field occurrence 'snippet_start))
                (end (org-files-db-search--field occurrence 'snippet_end)))
           (org-files-db-search--make-occurrence
            :heading-id (org-files-db-search--field row 'id)
            :title title
            :file file
            :field (intern (org-files-db-search--field occurrence 'field))
            :line (or line heading-line)
            :byte-start (if line
                            (org-files-db-search--field occurrence 'byte_start)
                          heading-byte)
            :snippet snippet
            :snippet-start start
            :snippet-end end
            :ranges (list (cons start end))
            :matched (and start end (<= start end (length snippet))
                          (list (substring-no-properties snippet start end))))))
       occurrences))))

(defun org-files-db-search-occurrences (expression &optional config substring)
  "Return the occurrences that FTS5 EXPRESSION matches as a list.
Each element is an `org-files-db-search-occurrence', in result rank order
and then in source order.  CONFIG is a name from `org-files-db-configs';
nil uses the default.  Non-nil SUBSTRING searches the substring index."
  (let* ((config-name (org-files-db-process--config-name config))
         (rows (org-files-db-process--call-json
                (append (list "search" "--format" "json" "--occurrences")
                        (and substring (list "--substring"))
                        (org-files-db-process--config-arguments config-name)
                        (list expression)))))
    (org-files-db-search--group-occurrences
     (mapcan #'org-files-db-search--row-occurrences (append rows nil)))))

(defun org-files-db-search--same-line-p (a b)
  "Return non-nil when occurrences A and B are on the same matched line."
  (and (equal (org-files-db-search-occurrence-heading-id a)
              (org-files-db-search-occurrence-heading-id b))
       (eq (org-files-db-search-occurrence-field a)
           (org-files-db-search-occurrence-field b))
       (equal (org-files-db-search-occurrence-line a)
              (org-files-db-search-occurrence-line b))))

(defun org-files-db-search--group-occurrences (occurrences)
  "Merge consecutive OCCURRENCES on the same line into the first one.
The merged occurrence keeps the position and snippet of the first match,
the matched texts of all matches, and the ranges of the matches that share
its snippet.  A long line has a different snippet around each match."
  (let ((result nil))
    (dolist (occurrence occurrences)
      (let ((first (car result)))
        (if (and first (org-files-db-search--same-line-p first occurrence))
            (progn
              (setf (org-files-db-search-occurrence-matched first)
                    (append (org-files-db-search-occurrence-matched first)
                            (org-files-db-search-occurrence-matched occurrence)))
              (when (equal (org-files-db-search-occurrence-snippet first)
                           (org-files-db-search-occurrence-snippet occurrence))
                (setf (org-files-db-search-occurrence-ranges first)
                      (append (org-files-db-search-occurrence-ranges first)
                              (org-files-db-search-occurrence-ranges occurrence)))))
          (push occurrence result))))
    (nreverse result)))

(defun org-files-db-search--score (occurrence words)
  "Return how many of WORDS the matched texts of OCCURRENCE contain.
Case and diacritics are ignored.  Without WORDS, count the distinct
matched texts."
  (let ((matched (org-files-db-search-occurrence-matched occurrence))
        (case-fold-search t))
    (if words
        (seq-count (lambda (word)
                     (let ((regexp (char-fold-to-regexp word)))
                       (seq-some (lambda (text) (string-match-p regexp text))
                                 matched)))
                   words)
      (length (seq-uniq (mapcar #'downcase matched))))))

(defun org-files-db-search--rank (occurrences words)
  "Return the best OCCURRENCES for the search WORDS, best first.
Keep the lines of each heading that match the most WORDS, then sort all
kept lines by that number.  Lines with equal numbers keep the orgfdb rank
and source order."
  (let ((scores (mapcar (lambda (occurrence)
                          (cons occurrence
                                (org-files-db-search--score occurrence words)))
                        occurrences))
        (best (make-hash-table :test #'equal)))
    (dolist (entry scores)
      (let ((id (org-files-db-search-occurrence-heading-id (car entry))))
        (puthash id (max (cdr entry) (gethash id best 0)) best)))
    (mapcar #'car
            (sort (seq-filter
                   (lambda (entry)
                     (= (cdr entry)
                        (gethash (org-files-db-search-occurrence-heading-id
                                  (car entry))
                                 best)))
                   scores)
                  (lambda (a b) (> (cdr a) (cdr b)))))))

(defun org-files-db-search--words (input)
  "Return the search words of INPUT, nil for a raw FTS5 expression."
  (pcase org-files-db-search-input-style
    ('fts nil)
    ('substring (seq-filter (lambda (word) (>= (length word) 3))
                            (split-string input nil t)))
    (_ (split-string input nil t))))

(defun org-files-db-search--candidate (occurrence)
  "Return the completion candidate string for OCCURRENCE."
  (let* ((snippet (copy-sequence (string-trim-right
                                  (org-files-db-search-occurrence-snippet occurrence))))
         (title-p (eq (org-files-db-search-occurrence-field occurrence) 'title))
         (location (format "%s:%s"
                           (file-name-nondirectory
                            (org-files-db-search-occurrence-file occurrence))
                           (or (org-files-db-search-occurrence-line occurrence) "?"))))
    (when title-p
      (add-face-text-property 0 (length snippet) 'org-files-db-heading t snippet))
    (dolist (range (org-files-db-search-occurrence-ranges occurrence))
      (let ((start (car range))
            (end (min (or (cdr range) 0) (length snippet))))
        (when (and start (< start end))
          (add-face-text-property start end 'org-files-db-search-match nil snippet))))
    (concat
     (if title-p
         snippet
       (propertize (org-files-db-search-occurrence-title occurrence)
                   'face 'org-files-db-heading))
     "  "
     (propertize location 'face 'org-files-db-file-path)
     (if title-p "" (concat "  " (string-trim-left snippet))))))

(defun org-files-db-search--candidates (occurrences)
  "Return an alist of unique candidate strings and OCCURRENCES."
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (occurrence occurrences)
      (let* ((candidate (org-files-db-search--candidate occurrence))
             (count (gethash candidate seen 0)))
        (puthash candidate (1+ count) seen)
        (when (> count 0)
          (setq candidate (concat candidate
                                  (propertize (format " [%d]" (1+ count))
                                              'face 'shadow))))
        (push (cons candidate occurrence) result)))
    (nreverse result)))

(defun org-files-db-search--expression (input)
  "Return the FTS5 expression for the user INPUT, or nil for no search.
See `org-files-db-search-input-style'.  The `substring' style drops words
shorter than three characters, which the trigram index cannot match."
  (pcase org-files-db-search-input-style
    ('fts (string-trim input))
    (style
     (let ((words (split-string input nil t)))
       (when (eq style 'substring)
         (setq words (seq-filter (lambda (word) (>= (length word) 3)) words)))
       (when words
         (mapconcat (lambda (word)
                      (format (if (eq style 'substring) "\"%s\"" "\"%s\"*")
                              (string-replace "\"" "\"\"" word)))
                    words
                    " AND "))))))

(defun org-files-db-search--input-occurrences (input config)
  "Return the occurrences for the user INPUT with CONFIG.
Signal a `user-error' when INPUT gives no search expression."
  (let ((expression (org-files-db-search--expression input)))
    (unless expression
      (user-error "Substring search needs a word of at least 3 characters"))
    (org-files-db-search--rank
     (org-files-db-search-occurrences
      expression config (eq org-files-db-search-input-style 'substring))
     (org-files-db-search--words input))))

(defun org-files-db-search--input-long-enough-p (input)
  "Return non-nil when INPUT reaches `org-files-db-search-min-input'."
  (>= (length (string-trim input)) org-files-db-search-min-input))

(defun org-files-db-search--live-candidates (input config)
  "Return the candidate alist for INPUT with CONFIG, nil while typing.
INPUT shorter than `org-files-db-search-min-input' and incomplete FTS5
expressions give no candidates instead of an error."
  (when (org-files-db-search--input-long-enough-p input)
    (condition-case nil
        (org-files-db-search--candidates
         (org-files-db-search--input-occurrences input config))
      ((org-files-db-error user-error) nil))))

(defun org-files-db-search-open-occurrence (occurrence)
  "Open the source of OCCURRENCE at the match and return nil."
  (org-files-db-actions--goto (org-files-db-search-occurrence-file occurrence)
                              (org-files-db-search-occurrence-line occurrence)
                              (org-files-db-search-occurrence-byte-start occurrence))
  nil)

(defun org-files-db-search--read-with-ivy (config)
  "Read one occurrence with a live Ivy search using CONFIG."
  (let* ((candidates nil)
         (selected
          (ivy-read "orgfdb search: "
                    (lambda (input)
                      (setq candidates
                            (org-files-db-search--live-candidates input config))
                      (mapcar #'car candidates))
                    :dynamic-collection t
                    :require-match t
                    :history 'org-files-db-search--history
                    :caller 'org-files-db-search)))
    (cdr (assoc selected candidates))))

(defun org-files-db-search--read-expression ()
  "Read a search expression of at least `org-files-db-search-min-input'."
  (let ((expression (read-string "orgfdb search: " nil
                                 'org-files-db-search--history)))
    (unless (org-files-db-search--input-long-enough-p expression)
      (user-error "Search needs at least %d characters"
                  org-files-db-search-min-input))
    expression))

(defun org-files-db-search--read-with-completion (config)
  "Read an expression, then one of its occurrences, using CONFIG."
  (let* ((expression (org-files-db-search--read-expression))
         (candidates (org-files-db-search--candidates
                      (org-files-db-search--input-occurrences expression config)))
         (strings (mapcar #'car candidates)))
    (unless candidates
      (user-error "No matches for %s" expression))
    (cdr (assoc (completing-read
                 "Occurrence: "
                 (lambda (string predicate action)
                   (if (eq action 'metadata)
                       '(metadata
                         (category . org-files-db-search-occurrence)
                         (display-sort-function . identity)
                         (cycle-sort-function . identity))
                     (complete-with-action action strings string predicate)))
                 nil t)
                candidates))))

;;;###autoload
(defun org-files-db-search (&optional config)
  "Search heading titles and bodies and open the selected occurrence.
By default each input word matches the start of a word, in any order;
see `org-files-db-search-input-style' for raw FTS5 expressions.
Matches appear from `org-files-db-search-min-input' characters on.
With `ivy-mode', results update while you type; otherwise
read the expression first and then select an occurrence.  CONFIG is a name
from `org-files-db-configs'; with a prefix argument, select it first.
Return the selected `org-files-db-search-occurrence'."
  (interactive
   (list (org-files-db-process--interactive-config-name current-prefix-arg)))
  (let ((occurrence
         (if (and (fboundp 'ivy-read) (bound-and-true-p ivy-mode))
             (org-files-db-search--read-with-ivy config)
           (org-files-db-search--read-with-completion config))))
    (unless occurrence
      (user-error "No occurrence selected"))
    (org-files-db-search-open-occurrence occurrence)
    occurrence))

(provide 'org-files-db-search)

;;; org-files-db-search.el ends here
