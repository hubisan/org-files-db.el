;;; org-files-db-presentation.el --- Rust presentation data for org-files-db -*- lexical-binding: t; -*-

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

;; PresentationSpec serialization, presentation-json version 2 decoding, and
;; lightweight completion data.  Rust owns presentation semantics and layout
;; work.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'org-files-db-core)

(defconst org-files-db-presentation--version 2
  "Presentation JSON version supported by this package.")

(defconst org-files-db-presentation--heading-faces
  [org-files-db-heading-1 org-files-db-heading-2 org-files-db-heading-3
                          org-files-db-heading-4 org-files-db-heading-5 org-files-db-heading-6
                          org-files-db-heading-7 org-files-db-heading-8]
  "Heading faces indexed by zero-based heading level.")

(defconst org-files-db-presentation--candidate-identity-base #x1900
  "Number of private-use characters used for candidate identities.")

(cl-defstruct (org-files-db-presentation
               (:constructor org-files-db-presentation--make-presentation))
  "One decoded presentation-json response."
  version
  database-id
  generation
  config
  results
  schemas
  rows)

(cl-defstruct (org-files-db-presentation-row
               (:constructor org-files-db-presentation--make-presentation-row))
  "One decoded presentation row."
  result-index
  row-context
  cells)

(cl-defstruct (org-files-db-presentation-cell
               (:constructor org-files-db-presentation--make-presentation-cell))
  "One decoded presentation cell."
  search-text
  display-text
  role)

(cl-defstruct (org-files-db-presentation--wire-schema
               (:constructor org-files-db-presentation--make-wire-schema))
  "Compiled positional indexes for one presentation wire schema."
  row-result-index
  row-context
  row-cells
  cell-search-text
  cell-display-text
  cell-role
  row-context-shapes
  role-values)

(defun org-files-db-presentation--default-columns (target)
  "Return the configured default columns for TARGET."
  (pcase target
    ('headings org-files-db-heading-columns)
    ('files org-files-db-file-columns)
    ('links org-files-db-link-columns)
    (_ (user-error "Unsupported org-files-db query target: %S" target))))

(defun org-files-db-presentation--default-sort (target)
  "Return the configured default sorting for TARGET."
  (pcase target
    ('headings org-files-db-heading-sort)
    ('files org-files-db-file-sort)
    ('links org-files-db-link-sort)
    (_ (user-error "Unsupported org-files-db query target: %S" target))))

(defun org-files-db-presentation--json-object (&rest entries)
  "Return a JSON object hash table from ENTRIES.
ENTRIES is a sequence of key and value pairs."
  (let ((object (make-hash-table :test #'equal)))
    (while entries
      (let ((key (pop entries)))
        (unless entries
          (error "Missing JSON value for key %S" key))
        (puthash key (pop entries) object)))
    object))

(defun org-files-db-presentation--json-boolean (value)
  "Return boolean VALUE in the representation used by JSON serialization."
  (unless (memq value '(nil t))
    (user-error "Presentation boolean must be nil or t: %S" value))
  (if value t :false))

(defun org-files-db-presentation--name (value description)
  "Return VALUE as a JSON name string for DESCRIPTION."
  (cond
   ((symbolp value) (symbol-name value))
   ((and (stringp value) (not (string-empty-p value))) value)
   (t (user-error "%s must be a symbol or non-empty string: %S"
                  description value))))

(defun org-files-db-presentation--plist (definition description)
  "Return the option plist in DEFINITION for DESCRIPTION."
  (unless (and (consp definition)
               (proper-list-p definition))
    (user-error "Malformed %s: %S" description definition))
  (let ((properties (cdr definition)))
    (unless (zerop (mod (length properties) 2))
      (user-error "Malformed options in %s: %S" description definition))
    properties))

(defun org-files-db-presentation--check-options (properties allowed description)
  "Check PROPERTIES against ALLOWED keys for DESCRIPTION."
  (let ((tail properties))
    (while tail
      (let ((key (pop tail)))
        (pop tail)
        (unless (memq key allowed)
          (user-error "Unsupported option %S in %s" key description))))))

(defun org-files-db-presentation--width-json (width)
  "Return the JSON width object for Emacs WIDTH."
  (pcase width
    ('auto
     (org-files-db-presentation--json-object "mode" "auto"))
    (`(max ,value)
     (org-files-db-presentation--json-object "mode" "max" "value" value))
    (`(fixed ,value)
     (org-files-db-presentation--json-object "mode" "fixed" "value" value))
    (_ (user-error "Malformed presentation width: %S" width))))

(defun org-files-db-presentation--truncate-json (truncate)
  "Return the JSON truncation object for Emacs TRUNCATE."
  (unless (and (listp truncate) (proper-list-p truncate))
    (user-error "Malformed presentation truncation: %S" truncate))
  (org-files-db-presentation--check-options
   truncate '(:position :marker) "presentation truncation")
  (let ((object (make-hash-table :test #'equal)))
    (when (plist-member truncate :position)
      (puthash "position"
               (org-files-db-presentation--name
                (plist-get truncate :position) "Truncation position")
               object))
    (when (plist-member truncate :marker)
      (puthash "marker" (plist-get truncate :marker) object))
    object))

(defun org-files-db-presentation--outline-json (properties)
  "Return the optional outline-path JSON object for PROPERTIES."
  (when (or (plist-member properties :separator)
            (plist-member properties :include-root)
            (plist-member properties :include-match))
    (let ((object (make-hash-table :test #'equal)))
      (when (plist-member properties :separator)
        (puthash "separator" (plist-get properties :separator) object))
      (when (plist-member properties :include-root)
        (puthash "include_root"
                 (org-files-db-presentation--json-boolean
                  (plist-get properties :include-root))
                 object))
      (when (plist-member properties :include-match)
        (puthash "include_match"
                 (org-files-db-presentation--json-boolean
                  (plist-get properties :include-match))
                 object))
      object)))

(defun org-files-db-presentation--column-json (definition)
  "Return one PresentationSpec column object for DEFINITION."
  (unless (and (consp definition) (proper-list-p definition))
    (user-error "Malformed presentation column: %S" definition))
  (let* ((name (org-files-db-presentation--name
                (car definition) "Presentation column name"))
         (properties
          (org-files-db-presentation--plist definition "presentation column")))
    (org-files-db-presentation--check-options
     properties
     '(:width :truncate :separator :include-root :include-match)
     (format "presentation column %s" name))
    (let ((object
           (org-files-db-presentation--json-object
            "name" name
            "width"
            (org-files-db-presentation--width-json
             (if (plist-member properties :width)
                 (plist-get properties :width)
               'auto))))
          (outline (org-files-db-presentation--outline-json properties)))
      (when (and (plist-member properties :truncate)
                 (plist-get properties :truncate))
        (puthash "truncate"
                 (org-files-db-presentation--truncate-json
                  (plist-get properties :truncate))
                 object))
      (when outline
        (puthash "outline_path" outline object))
      object)))

(defun org-files-db-presentation--sort-json (definition)
  "Return one PresentationSpec sort object for DEFINITION."
  (unless (and (consp definition) (proper-list-p definition))
    (user-error "Malformed presentation sort: %S" definition))
  (let* ((name (org-files-db-presentation--name
                (car definition) "Sort column name"))
         (properties
          (org-files-db-presentation--plist definition "presentation sort")))
    (org-files-db-presentation--check-options
     properties '(:direction) (format "presentation sort %s" name))
    (let ((object (org-files-db-presentation--json-object "column" name)))
      (when (plist-member properties :direction)
        (puthash "direction"
                 (org-files-db-presentation--name
                  (plist-get properties :direction) "Sort direction")
                 object))
      object)))

(defun org-files-db-presentation--row-source-json (row-source)
  "Return the PresentationSpec row-source object for ROW-SOURCE."
  (when row-source
    (org-files-db-presentation--json-object
     "kind" (org-files-db-presentation--name row-source "Row source"))))

(defun org-files-db-presentation--spec-json (columns sort row-source)
  "Serialize COLUMNS, SORT, and ROW-SOURCE as PresentationSpec JSON."
  (unless (and (listp columns) (proper-list-p columns) columns)
    (user-error "Presentation columns must contain at least one column"))
  (unless (and (listp sort) (proper-list-p sort))
    (user-error "Presentation sort must be a list: %S" sort))
  (let ((object
         (org-files-db-presentation--json-object
          "columns" (vconcat (mapcar #'org-files-db-presentation--column-json columns))
          "sort" (vconcat (mapcar #'org-files-db-presentation--sort-json sort))
          "row_source" (org-files-db-presentation--row-source-json row-source))))
    (json-serialize object :null-object nil :false-object :false)))

(defun org-files-db-presentation--error (format-string &rest arguments)
  "Signal an org-files-db presentation error.
FORMAT-STRING and ARGUMENTS build the user-facing message."
  (signal 'org-files-db-error
          (list (apply #'format format-string arguments))))

(defun org-files-db-presentation--required (object key)
  "Return required KEY from alist OBJECT."
  (let ((entry (assq key object)))
    (unless entry
      (org-files-db-presentation--error
       "Presentation JSON version 2 is missing %s" key))
    (cdr entry)))

(defun org-files-db-presentation--field-index (fields name section)
  "Return index of NAME in schema FIELDS for SECTION."
  (unless (vectorp fields)
    (org-files-db-presentation--error
     "Presentation JSON schema %s fields are not an array" section))
  (or (cl-position name fields :test #'equal)
      (org-files-db-presentation--error
       "Presentation JSON schema %s has no %s field" section name)))

(defun org-files-db-presentation--vector-value (values index description)
  "Return VALUES element at INDEX for DESCRIPTION."
  (unless (and (vectorp values)
               (integerp index)
               (>= index 0)
               (< index (length values)))
    (org-files-db-presentation--error
     "Invalid %s position %S" description index))
  (aref values index))

(defun org-files-db-presentation--role (encoded role-values)
  "Decode ENCODED with ROLE-VALUES and return a role symbol."
  (cond
   ((null encoded) nil)
   ((not (and (integerp encoded)
              (>= encoded 0)
              (vectorp role-values)
              (< encoded (length role-values))))
    (org-files-db-presentation--error
     "Invalid presentation role index: %S" encoded))
   (t
    (let ((name (aref role-values encoded)))
      (unless (stringp name)
        (org-files-db-presentation--error
         "Invalid presentation role value at index %d" encoded))
      (intern name)))))

(defun org-files-db-presentation--row-context (encoded shapes)
  "Decode row-context ENCODED with schema SHAPES."
  (when encoded
    (unless (vectorp encoded)
      (org-files-db-presentation--error
       "Invalid presentation row_context: %S" encoded))
    (let (matched-shape)
      (dolist (entry shapes)
        (let* ((kind (car entry))
               (fields (cdr entry))
               (kind-index
                (and (vectorp fields)
                     (cl-position "kind" fields :test #'equal))))
          (when (and kind-index
                     (< kind-index (length encoded))
                     (equal (aref encoded kind-index)
                            (if (symbolp kind) (symbol-name kind) kind)))
            (setq matched-shape fields))))
      (unless matched-shape
        (org-files-db-presentation--error
         "Unknown presentation row_context shape: %S" encoded))
      (unless (= (length encoded) (length matched-shape))
        (org-files-db-presentation--error
         "Invalid presentation row_context length: %S" encoded))
      (cl-loop for index from 0 below (length matched-shape)
               for field = (aref matched-shape index)
               unless (stringp field)
               do (org-files-db-presentation--error
                   "Invalid presentation row_context field: %S" field)
               collect (cons (intern field) (aref encoded index))))))

(defun org-files-db-presentation--compile-wire-schema (schemas)
  "Compile positional indexes and lookup data from SCHEMAS."
  (let* ((row-fields
          (org-files-db-presentation--required schemas 'row_fields))
         (cell-fields
          (org-files-db-presentation--required schemas 'cell_fields))
         (shapes
          (org-files-db-presentation--required schemas 'row_context_shapes))
         (display-text-null
          (org-files-db-presentation--required schemas 'display_text_null))
         (role-encoding
          (org-files-db-presentation--required schemas 'role_encoding))
         (role-values
          (org-files-db-presentation--required schemas 'role_values)))
    (unless (equal display-text-null "same-as-search_text")
      (org-files-db-presentation--error
       "Unsupported presentation display_text null encoding: %S"
       display-text-null))
    (unless (equal role-encoding "null-or-index-into-role_values")
      (org-files-db-presentation--error
       "Unsupported presentation role encoding: %S" role-encoding))
    (unless (listp shapes)
      (org-files-db-presentation--error
       "Presentation JSON row_context_shapes are not an object"))
    (unless (vectorp role-values)
      (org-files-db-presentation--error
       "Presentation JSON role_values are not an array"))
    (org-files-db-presentation--make-wire-schema
     :row-result-index
     (org-files-db-presentation--field-index
      row-fields "result_index" "row")
     :row-context
     (org-files-db-presentation--field-index
      row-fields "row_context" "row")
     :row-cells
     (org-files-db-presentation--field-index
      row-fields "cells" "row")
     :cell-search-text
     (org-files-db-presentation--field-index
      cell-fields "search_text" "cell")
     :cell-display-text
     (org-files-db-presentation--field-index
      cell-fields "display_text" "cell")
     :cell-role
     (org-files-db-presentation--field-index
      cell-fields "role" "cell")
     :row-context-shapes shapes
     :role-values role-values)))

(defun org-files-db-presentation--decode-cell (encoded schema)
  "Decode one presentation cell ENCODED with compiled SCHEMA."
  (unless (vectorp encoded)
    (org-files-db-presentation--error
     "Invalid presentation cell: %S" encoded))
  (let* ((search-text
          (org-files-db-presentation--vector-value
           encoded
           (org-files-db-presentation--wire-schema-cell-search-text schema)
           "cell search_text"))
         (display-text
          (org-files-db-presentation--vector-value
           encoded
           (org-files-db-presentation--wire-schema-cell-display-text schema)
           "cell display_text"))
         (role
          (org-files-db-presentation--vector-value
           encoded
           (org-files-db-presentation--wire-schema-cell-role schema)
           "cell role")))
    (unless (stringp search-text)
      (org-files-db-presentation--error
       "Invalid presentation search_text: %S" search-text))
    (unless (or (null display-text) (stringp display-text))
      (org-files-db-presentation--error
       "Invalid presentation display_text: %S" display-text))
    (org-files-db-presentation--make-presentation-cell
     :search-text search-text
     :display-text (or display-text search-text)
     :role
     (org-files-db-presentation--role
      role (org-files-db-presentation--wire-schema-role-values schema)))))

(defun org-files-db-presentation--decode-row (encoded results schema)
  "Decode one presentation row ENCODED with RESULTS and compiled SCHEMA."
  (unless (vectorp encoded)
    (org-files-db-presentation--error
     "Invalid presentation row: %S" encoded))
  (let* ((result-index
          (org-files-db-presentation--vector-value
           encoded
           (org-files-db-presentation--wire-schema-row-result-index schema)
           "row result_index"))
         (row-context
          (org-files-db-presentation--vector-value
           encoded
           (org-files-db-presentation--wire-schema-row-context schema)
           "row row_context"))
         (cells
          (org-files-db-presentation--vector-value
           encoded
           (org-files-db-presentation--wire-schema-row-cells schema)
           "row cells")))
    (unless (and (integerp result-index)
                 (>= result-index 0)
                 (< result-index (length results)))
      (org-files-db-presentation--error
       "Invalid presentation result_index: %S" result-index))
    (unless (vectorp cells)
      (org-files-db-presentation--error
       "Invalid presentation cells array: %S" cells))
    (org-files-db-presentation--make-presentation-row
     :result-index result-index
     :row-context
     (org-files-db-presentation--row-context
      row-context
      (org-files-db-presentation--wire-schema-row-context-shapes schema))
     :cells
     (vconcat
      (mapcar
       (lambda (cell)
         (org-files-db-presentation--decode-cell cell schema))
       cells)))))

(defun org-files-db-presentation--decode (wire)
  "Decode one presentation-json version 2 WIRE alist."
  (unless (listp wire)
    (org-files-db-presentation--error
     "Invalid presentation-json response"))
  (let ((version (org-files-db-presentation--required wire 'presentation_version)))
    (unless (equal version org-files-db-presentation--version)
      (org-files-db-presentation--error
       "Unsupported presentation version: %S (expected %d)"
       version org-files-db-presentation--version)))
  (let* ((database-id
          (org-files-db-presentation--required wire 'database_id))
         (generation
          (org-files-db-presentation--required wire 'generation))
         (results
          (org-files-db-presentation--required wire 'results))
         (schemas
          (org-files-db-presentation--required wire 'schemas))
         (encoded-rows
          (org-files-db-presentation--required wire 'rows)))
    (unless (stringp database-id)
      (org-files-db-presentation--error
       "Presentation JSON database_id is not a string"))
    (unless (integerp generation)
      (org-files-db-presentation--error
       "Presentation JSON generation is not an integer"))
    (unless (vectorp results)
      (org-files-db-presentation--error
       "Presentation JSON results are not an array"))
    (unless (listp schemas)
      (org-files-db-presentation--error
       "Presentation JSON schemas are not an object"))
    (unless (vectorp encoded-rows)
      (org-files-db-presentation--error
       "Presentation JSON rows are not an array"))
    (let* ((schema (org-files-db-presentation--compile-wire-schema schemas))
           (rows
            (vconcat
             (mapcar
              (lambda (row)
                (org-files-db-presentation--decode-row row results schema))
              encoded-rows))))
      (org-files-db-presentation--make-presentation
       :version org-files-db-presentation--version
       :database-id database-id
       :generation generation
       :results results
       :schemas schemas
       :rows rows))))

(defun org-files-db-presentation--result-level (result)
  "Return a normalized heading level for RESULT."
  (let ((level (and (listp result)
                    (or (alist-get 'level result)
                        (alist-get 'heading_level result)))))
    (if (and (integerp level) (> level 0))
        (min 8 level)
      1)))

(defun org-files-db-presentation--todo-face (keyword role)
  "Return the face for TODO KEYWORD with semantic ROLE."
  (or (org-face-from-face-or-color
       'todo 'org-todo (cdr (assoc keyword org-todo-keyword-faces)))
      (pcase role
        ('todo 'org-files-db-todo)
        ('done 'org-files-db-done))))

(defun org-files-db-presentation--role-face (role result &optional cell-text)
  "Return the face for semantic ROLE, original RESULT, and CELL-TEXT."
  (pcase role
    ('heading
     (aref org-files-db-presentation--heading-faces
           (1- (org-files-db-presentation--result-level result))))
    ('title 'org-files-db-title)
    ((or 'todo 'done)
     (org-files-db-presentation--todo-face cell-text role))
    ('priority 'org-files-db-priority)
    ('tag 'org-files-db-tag)
    ('date 'org-files-db-date)
    ('file-name 'org-files-db-file-name)
    ('file-path 'org-files-db-file-path)
    ('keyword-name 'org-files-db-keyword-name)
    ('keyword-value 'org-files-db-keyword-value)
    ('property-name 'org-files-db-property-name)
    ('property-value 'org-files-db-property-value)
    (_ nil)))

(defun org-files-db-presentation--visible-row (row result)
  "Return the Rust-prepared visible string for ROW and RESULT."
  (let ((cells (org-files-db-presentation-row-cells row))
        segments)
    (dotimes (index (length cells))
      (let* ((cell (aref cells index))
             (segment
              (copy-sequence
               (org-files-db-presentation-cell-display-text cell)))
             (face
              (org-files-db-presentation--role-face
               (org-files-db-presentation-cell-role cell)
               result
               (org-files-db-presentation-cell-search-text cell))))
        (when (and face (> (length segment) 0))
          (add-text-properties 0 (length segment) (list 'face face) segment))
        (push segment segments)))
    (string-join (nreverse segments) "  ")))

(defun org-files-db-presentation--search-row (row)
  "Return complete searchable text for presentation ROW."
  (let ((cells (org-files-db-presentation-row-cells row))
        values)
    (dotimes (index (length cells))
      (let ((value
             (org-files-db-presentation-cell-search-text
              (aref cells index))))
        (unless (string-empty-p value)
          (push value values))))
    (string-join (nreverse values) "  ")))

(defun org-files-db-presentation--candidate-identity (index)
  "Return a compact hidden identity suffix for zero-based INDEX."
  (let ((value (1+ index))
        characters)
    (while (> value 0)
      (push (+ #xe000 (% value org-files-db-presentation--candidate-identity-base))
            characters)
      (setq value (/ value org-files-db-presentation--candidate-identity-base)))
    (apply #'string #x2063 characters)))

(defun org-files-db-presentation--candidate-identity-index (candidate)
  "Return the zero-based identity encoded at the end of CANDIDATE."
  (when (stringp candidate)
    (let* ((end (length candidate))
           (start end))
      (while (and (> start 0)
                  (let ((character (aref candidate (1- start))))
                    (and (>= character #xe000)
                         (< character
                            (+ #xe000 org-files-db-presentation--candidate-identity-base)))))
        (setq start (1- start)))
      (when (and (< start end)
                 (> start 0)
                 (= (aref candidate (1- start)) #x2063))
        (let ((value 0)
              (position start))
          (while (< position end)
            (setq value
                  (+ (* value org-files-db-presentation--candidate-identity-base)
                     (- (aref candidate position) #xe000))
                  position (1+ position)))
          (and (> value 0) (1- value)))))))

(defun org-files-db-presentation--candidate (presentation row index)
  "Return one completion candidate for ROW at INDEX in PRESENTATION."
  (let* ((result (org-files-db-presentation--row-result presentation row))
         (visible (org-files-db-presentation--visible-row row result))
         (search (org-files-db-presentation--search-row row))
         (body (if (string-empty-p search) (string #x2060) search))
         (body-length (length body))
         (candidate (concat body (org-files-db-presentation--candidate-identity index)))
         (metadata
          (list 'org-files-db-presentation-row row
                'org-files-db-result result
                'org-files-db-row-context
                (org-files-db-presentation-row-row-context row)
                'org-files-db-config
                (org-files-db-presentation-config presentation)
                'rear-nonsticky t)))
    (add-text-properties 0 (length candidate) metadata candidate)
    (add-text-properties 0 body-length (list 'display visible) candidate)
    (add-text-properties body-length (length candidate) '(display "") candidate)
    candidate))

(defun org-files-db-presentation--candidates (presentation)
  "Return lightweight completion candidates for PRESENTATION."
  (let* ((rows (org-files-db-presentation-rows presentation))
         (count (length rows))
         candidates)
    (dotimes (index count)
      (push
       (org-files-db-presentation--candidate
        presentation (aref rows index) index)
       candidates))
    (nreverse candidates)))

(defun org-files-db-presentation--completion-table (candidates)
  "Return a standard completion table that preserves CANDIDATES order."
  (lambda (string predicate action)
    (cond
     ((eq action 'metadata)
      '(metadata
        (display-sort-function . identity)
        (cycle-sort-function . identity)))
     ((eq action 'org-files-db-presentation--candidates) candidates)
     (t (complete-with-action action candidates string predicate)))))

(defun org-files-db-presentation--completion-candidates (table)
  "Return the candidate list stored in completion TABLE."
  (funcall table "" nil 'org-files-db-presentation--candidates))

(defun org-files-db-presentation--candidate-result (selected presentation)
  "Return the original result for SELECTED from PRESENTATION."
  (or (and (stringp selected)
           (> (length selected) 0)
           (get-text-property 0 'org-files-db-result selected))
      (when-let* ((index (org-files-db-presentation--candidate-identity-index selected)))
        (let ((rows (org-files-db-presentation-rows presentation)))
          (when (< index (length rows))
            (org-files-db-presentation--row-result
             presentation (aref rows index)))))))

(defun org-files-db-presentation--read (presentation &optional prompt)
  "Read one result from PRESENTATION with standard completion and PROMPT."
  (let ((candidates (org-files-db-presentation--candidates presentation)))
    (unless candidates
      (user-error "The query returned no results"))
    (let ((selected
           (completing-read
            (or prompt "Result: ")
            (org-files-db-presentation--completion-table candidates)
            nil t)))
      (or (org-files-db-presentation--candidate-result selected presentation)
          (user-error "Selected result is no longer available")))))

(defun org-files-db-presentation--row-result (presentation row)
  "Return the original result for ROW in PRESENTATION."
  (let* ((results (org-files-db-presentation-results presentation))
         (index (org-files-db-presentation-row-result-index row)))
    (unless (and (vectorp results)
                 (integerp index)
                 (>= index 0)
                 (< index (length results)))
      (org-files-db-presentation--error
       "Invalid presentation result_index: %S" index))
    (aref results index)))

(provide 'org-files-db-presentation)

;;; org-files-db-presentation.el ends here
