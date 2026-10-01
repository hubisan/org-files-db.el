;;; org-files-db-outline.el --- Outline export of matched headings -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Hubmann

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Export matched heading results as an Org outline of links. The outline
;; structure (file roots, matched headings and their merged ancestors) comes
;; from `orgfdb query --output outline'. Headline details, planning, properties
;; and body text are read from the source files in Emacs, after checking that
;; each indexed position still holds the expected headline.
;;
;; Layout: every file becomes a top-level heading with a link to the file, and
;; its headings sit one level deeper than in the source file. Without
;; ancestors the matched headings are listed flat on level 2 under their file.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'ol)
(require 'org-files-db-core)
(require 'org-files-db-presentation)
(require 'org-files-db-process)
(require 'org-files-db-query)

(defconst org-files-db-outline--buffer-name "*org-files-db outline*"
  "Name of the outline export buffer.")

(defconst org-files-db-outline--stale-message "Index changed, run the query again"
  "Message for an index that no longer matches the source files.")

(defun org-files-db-outline--stale ()
  "Signal the user error for a changed index."
  (user-error "%s" org-files-db-outline--stale-message))

(defun org-files-db-outline--fetch (records presentation)
  "Return the outline root nodes for heading RECORDS from PRESENTATION."
  (let ((ids (delete-dups (mapcar #'org-files-db-record-id records))))
    (condition-case nil
        (append (org-files-db-query--guarded-json
                 (format "(headings (id %s))"
                         (mapconcat #'number-to-string ids " "))
                 presentation :output "outline")
                nil)
      (org-files-db-stale-index (org-files-db-outline--stale)))))

(defun org-files-db-outline--flatten (node)
  "Return the heading nodes below outline NODE in document order."
  (mapcan (lambda (child)
            (cons child (org-files-db-outline--flatten child)))
          (append (alist-get 'children node) nil)))

(defun org-files-db-outline--matched-p (node)
  "Return non-nil when outline NODE is a matched heading."
  (eq (alist-get 'matched node) t))

(defun org-files-db-outline--locate (node)
  "Return the position of heading NODE in the current buffer.
Signal a user error when the headline there is not the indexed one."
  (let* ((byte (alist-get 'byte_start (alist-get 'location node)))
         (pos (and (integerp byte) (byte-to-position (1+ byte)))))
    (unless pos
      (org-files-db-outline--stale))
    (save-excursion
      (goto-char pos)
      (let ((components (and (= pos (line-beginning-position))
                             (org-at-heading-p)
                             (org-heading-components))))
        (unless (and components
                     (equal (nth 0 components) (alist-get 'level node))
                     (let ((title (or (nth 4 components) "")))
                       (or (equal title (alist-get 'title node))
                           (equal title (alist-get 'title_raw node))
                           (equal (string-trim (concat (nth 2 components) " " title))
                                  (alist-get 'title_raw node)))))
          (org-files-db-outline--stale))))
    pos))

(defun org-files-db-outline--link (file pos)
  "Return the link target for the heading at POS in the current buffer of FILE."
  (let ((custom-id (org-entry-get pos "CUSTOM_ID"))
        (id (org-entry-get pos "ID"))
        (path (abbreviate-file-name file)))
    (cond
     ((org-string-nw-p custom-id) (format "file:%s::#%s" path custom-id))
     ((org-string-nw-p id) (format "id:%s" id))
     (t (format "file:%s::%d" path (line-number-at-pos pos))))))

(defun org-files-db-outline--headline (file pos level)
  "Return the exported headline of the heading at POS in FILE on LEVEL."
  (save-excursion
    (goto-char pos)
    (pcase-let* ((`(,_ ,_ ,todo ,priority ,title ,tags) (org-heading-components)))
      (concat
       (string-join
        (delq nil (list (make-string level ?*)
                        todo
                        (and priority (format "[#%c]" priority))
                        (org-link-make-string
                         (org-files-db-outline--link file pos)
                         (org-link-display-format (or title "")))))
        " ")
       (and tags (concat " " tags))))))

(defun org-files-db-outline--entry-lines (file pos level matched)
  "Return the exported text blocks of the heading at POS in FILE on LEVEL.
MATCHED is non-nil for a matched heading, which may also keep its
properties and body text."
  (let ((blocks (list (org-files-db-outline--headline file pos level))))
    (save-excursion
      (goto-char pos)
      (forward-line 1)
      (when (looking-at org-planning-line-re)
        (when org-files-db-outline-export-planning
          (push (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))
                blocks))
        (forward-line 1))
      (when (looking-at org-property-drawer-re)
        (when (and matched org-files-db-outline-export-properties)
          (push (match-string-no-properties 0) blocks))
        (goto-char (match-end 0))
        (forward-line 1))
      (when (and matched org-files-db-outline-export-body)
        (let* ((start (point))
               (end (save-excursion
                      (or (outline-next-heading) (goto-char (point-max)))
                      (point)))
               (body (and (< start end)
                          (string-trim-right
                           (buffer-substring-no-properties start end)))))
          (when (org-string-nw-p body)
            (push body blocks)))))
    (nreverse blocks)))

(defun org-files-db-outline--descendants (file pos level claimed)
  "Return link items for the descendants of the heading at POS in FILE.
LEVEL is the exported level of that heading. CLAIMED maps positions of
headings listed elsewhere to t; they are skipped. Without ancestors a
claimed heading is a matched one and its whole subtree is skipped."
  (when org-files-db-outline-export-children
    (save-excursion
      (goto-char pos)
      (let* ((source-level (org-current-level))
             (limit (if (eq org-files-db-outline-export-children 'children)
                        (1+ source-level)
                      most-positive-fixnum))
             (end (save-excursion (org-end-of-subtree t) (point)))
             items)
        (while (and (outline-next-heading) (< (point) end))
          (let ((here (point))
                (here-level (org-current-level)))
            (cond
             ((gethash here claimed)
              (unless org-files-db-outline-export-ancestors
                (org-end-of-subtree t)))
             ((<= here-level limit)
              (puthash here t claimed)
              (push (list :pos here
                          :matched nil
                          :blocks (list (org-files-db-outline--headline
                                         file here
                                         (+ level (- here-level source-level)))))
                    items)))))
        (nreverse items)))))

(defun org-files-db-outline--file-items (root)
  "Return the exported items for the outline ROOT node.
Each item is a plist with :pos, :matched and :blocks, preceded by the item
of the file heading."
  (let* ((file (alist-get 'file_path (alist-get 'location root)))
         (title (or (org-string-nw-p (alist-get 'title root))
                    (file-name-nondirectory file)))
         (nodes (org-files-db-outline--flatten root))
         (ancestors org-files-db-outline-export-ancestors))
    (unless (file-readable-p file)
      (org-files-db-outline--stale))
    (let* ((existing (get-file-buffer file))
           (buffer (or existing (find-file-noselect file))))
      (unwind-protect
          (with-current-buffer buffer
            (unless (derived-mode-p 'org-mode)
              (org-files-db-outline--stale))
            (org-with-wide-buffer
             (let* ((located (mapcar (lambda (node)
                                       (cons node (org-files-db-outline--locate node)))
                                     nodes))
                    (claimed (make-hash-table))
                    (shown (if ancestors
                               located
                             (cl-remove-if-not
                              (lambda (entry)
                                (org-files-db-outline--matched-p (car entry)))
                              located)))
                    items)
               (dolist (entry shown)
                 (puthash (cdr entry) t claimed))
               (dolist (entry shown)
                 (let* ((node (car entry))
                        (pos (cdr entry))
                        (matched (org-files-db-outline--matched-p node))
                        (level (if ancestors (1+ (alist-get 'level node)) 2))
                        (item (list :pos pos
                                    :matched matched
                                    :blocks (org-files-db-outline--entry-lines
                                             file pos level matched)))
                        (extra (and matched
                                    (org-files-db-outline--descendants
                                     file pos level claimed))))
                   (push (cons item extra) items)))
               (setq items (nreverse items))
               (cons (list :pos 0
                           :matched nil
                           :blocks (list (concat "* " (org-link-make-string
                                                       (format "file:%s"
                                                               (abbreviate-file-name file))
                                                       title))))
                     (if ancestors
                         (sort (mapcan (lambda (group) (cons (car group) (cdr group)))
                                       items)
                               (lambda (a b) (< (plist-get a :pos) (plist-get b :pos))))
                       (mapcan (lambda (group) (cons (car group) (cdr group)))
                               items))))))
        (unless (or existing (buffer-modified-p buffer))
          (kill-buffer buffer))))))

(defvar-keymap org-files-db-outline-export-mode-map
  :doc "Keymap for `org-files-db-outline-export-mode'."
  "C-x C-s" #'org-files-db-outline-export-save)

(define-minor-mode org-files-db-outline-export-mode
  "Minor mode for the org-files-db outline export buffer.
\\{org-files-db-outline-export-mode-map}"
  :lighter " Outline-export"
  :keymap org-files-db-outline-export-mode-map)

(defun org-files-db-outline-export-save (file)
  "Save the text of the outline export buffer to FILE without its faces."
  (interactive (list (read-file-name "Save outline to: ")))
  (setq file (expand-file-name file))
  (when (and (file-exists-p file)
             (not (y-or-n-p (format "Overwrite %s? " file))))
    (user-error "Outline not saved"))
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-file file
      (insert text)))
  (message "Outline saved to %s" file)
  file)

(defun org-files-db-outline--render (items)
  "Show the exported ITEMS in the outline export buffer and return it."
  (let ((buffer (get-buffer-create org-files-db-outline--buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (setq buffer-read-only nil)
        (erase-buffer)
        (delete-all-overlays)
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (org-files-db-outline-export-mode 1)
        (dolist (item items)
          (let ((start (point)))
            (insert (string-join (plist-get item :blocks) "\n") "\n")
            (when (plist-get item :matched)
              (let ((overlay (save-excursion
                               (goto-char start)
                               (make-overlay start (line-end-position)))))
                (overlay-put overlay 'face 'org-files-db-outline-match)
                (overlay-put overlay 'org-files-db-outline-match t)))))
        (goto-char (point-min))
        (set-buffer-modified-p nil))
      (setq buffer-read-only t))
    buffer))

;;;###autoload
(defun org-files-db-outline-export (records presentation)
  "Export heading RECORDS of PRESENTATION as an Org outline of links.
Return the read-only outline buffer after showing it. The outline keeps the
hierarchy of each file, see `org-files-db-outline-export-ancestors' and the
related options. Signal a user error when a record is not a heading or when
the index changed since PRESENTATION; then nothing is exported."
  (unless records
    (user-error "No results to export"))
  (unless (cl-every (lambda (record) (eq (org-files-db-record-kind record) 'heading))
                    records)
    (user-error "Outline export needs heading results"))
  (let ((items (mapcan #'org-files-db-outline--file-items
                       (org-files-db-outline--fetch records presentation))))
    (pop-to-buffer-same-window (org-files-db-outline--render items))))

(provide 'org-files-db-outline)

;;; org-files-db-outline.el ends here
