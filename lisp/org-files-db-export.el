;;; org-files-db-export.el --- Org export for org-files-db -*- lexical-binding: t; -*-

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

;; Org rendering and optional Embark export integration.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'org-files-db-query)

(declare-function embark-export "embark")
(defvar embark-exporters-alist)

(defun org-files-db--heading-at-result (result function)
  "Call FUNCTION at RESULT's heading and return its value."
  (let ((file (org-files-db--result-file result)))
    (when (and file (file-readable-p file))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (org-files-db--goto-result-location result)
          (when (derived-mode-p 'org-mode)
            (condition-case nil
                (progn
                  (org-back-to-heading t)
                  (funcall function))
              (error nil))))))))

(defun org-files-db--heading-link-info (result)
  "Return information about the first Org link in RESULT's title."
  (org-files-db--heading-at-result
   result
   (lambda ()
     (let* ((title (org-get-heading t t t t))
            (tree (org-element-parse-secondary-string title '(link)))
            (link (org-element-map tree 'link #'identity nil t)))
       (when (and link
                  (member (org-element-property :type link) '("file" "id")))
         (let* ((rendered (org-element-interpret-data link))
                (begin (string-search rendered title))
                (end (and begin (+ begin (length rendered))))
                (description
                 (when-let* ((contents (org-element-contents link)))
                   (org-element-interpret-data contents))))
           (list :title title
                 :source-file buffer-file-name
                 :type (org-element-property :type link)
                 :path (org-element-property :path link)
                 :search-option (org-element-property :search-option link)
                 :description description
                 :rendered rendered
                 :prefix (and begin (substring title 0 begin))
                 :suffix (and end (substring title end)))))))))

(defun org-files-db--split-file-link-path (path search-option)
  "Return a pair of file PATH and SEARCH-OPTION.
The fallback split is used for Org versions which leave the search option in
PATH."
  (if search-option
      (cons path search-option)
    (if (string-match "\\`\\(.*\\)::\\(.*\\)\\'" path)
        (cons (match-string 1 path) (match-string 2 path))
      (cons path nil))))

(defun org-files-db--absolute-linked-file (info)
  "Return the absolute file target described by INFO."
  (pcase-let* ((`(,path . ,_) (org-files-db--split-file-link-path
                               (plist-get info :path)
                               (plist-get info :search-option)))
               (path (org-link-unescape path)))
    (expand-file-name path
                      (file-name-directory (plist-get info :source-file)))))

(defun org-files-db--rebased-heading-link (info)
  "Return INFO's link with an absolute file target when needed."
  (let* ((type (plist-get info :type))
         (description (plist-get info :description))
         (target
          (pcase type
            ("file"
             (pcase-let* ((`(,_ . ,search)
                           (org-files-db--split-file-link-path
                            (plist-get info :path)
                            (plist-get info :search-option)))
                          (file (org-files-db--absolute-linked-file info)))
               (concat "file:" (org-link-escape file)
                       (when search (concat "::" search)))))
            ("id" (concat "id:" (plist-get info :path)))
            (_ nil))))
    (when target
      (org-link-make-string target description))))

(defun org-files-db--goto-linked-target (info)
  "Open and visit the link target described by INFO.
Return a result-like alist for the target."
  (pcase (plist-get info :type)
    ("id"
     (let* ((id (plist-get info :path))
            (response
             (org-files-db--execute-query
              `(headings (property "ID" ,id :inherit nil))))
            (results (org-files-db--normalize-results response)))
       (pcase (length results)
         (0 (user-error "Cannot resolve indexed Org ID %s" id))
         (1
          (let ((result (car results)))
            (org-files-db-open-result result)
            result))
         (_ (user-error "Org ID %s resolves to multiple indexed headings" id)))))
    ("file"
     (pcase-let* ((`(,_ . ,search)
                   (org-files-db--split-file-link-path
                    (plist-get info :path)
                    (plist-get info :search-option)))
                  (file (org-files-db--absolute-linked-file info)))
       (unless (file-readable-p file)
         (user-error "Linked file is missing: %s" file))
       (find-file file)
       (goto-char (point-min))
       (when search
         (org-link-search search))
       (let ((title (if (org-at-heading-p)
                        (org-get-heading t t t t)
                      (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))
                          (file-name-base file)))))
         (when (listp title)
           (setq title (car title)))
         `((kind . ,(if (org-at-heading-p) "heading" "file"))
           (title . ,title)
           (location . ((file_path . ,file)
                        (line . ,(line-number-at-pos))
                        (byte_start . nil)))))))
    (_ (user-error "Unsupported heading link type"))))

(defun org-files-db--preserved-linked-heading (info)
  "Return linked heading text using INFO."
  (when-let* ((link (org-files-db--rebased-heading-link info)))
    (concat (or (plist-get info :prefix) "")
            link
            (or (plist-get info :suffix) ""))))

(defun org-files-db--resolved-linked-heading (info)
  "Return an Org link to the effective target represented by INFO."
  (save-current-buffer
    (save-window-excursion
      (let ((target (org-files-db--goto-linked-target info)))
        (org-files-db--result-org-link target
                                       (org-files-db--result-title target))))))

(defun org-files-db--export-result-text (result)
  "Return Org text representing RESULT."
  (if (not (org-files-db--result-file result))
      (org-files-db--node-title result)
    (if (eq (org-files-db--kind result) 'heading)
        (if-let* ((info (org-files-db--heading-link-info result)))
            (condition-case nil
                (pcase org-files-db-export-linked-heading-style
                  ('resolve (org-files-db--resolved-linked-heading info))
                  (_ (org-files-db--preserved-linked-heading info)))
              (error
               (or (org-files-db--preserved-linked-heading info)
                   (org-files-db--result-org-link result))))
          (org-files-db--result-org-link result))
      (org-files-db--result-org-link result))))

(defun org-files-db--path-node-key (node)
  "Return a stable key for outline path NODE."
  (format "%s:%s:%s:%s:%s:%s:%s"
          (or (org-files-db--get node 'kind) "")
          (or (org-files-db--get node 'id) "")
          (or (org-files-db--get node 'file_id) "")
          (or (org-files-db--get node 'location 'file_path) "")
          (or (org-files-db--get node 'location 'line) "")
          (or (org-files-db--get node 'level) "")
          (org-files-db--node-title node)))

(defun org-files-db--same-node-p (node result)
  "Return non-nil when path NODE denotes RESULT."
  (let ((node-id (org-files-db--get node 'id))
        (result-id (org-files-db--get result 'id)))
    (if (and node-id result-id)
        (equal node-id result-id)
      (and (equal (org-files-db--node-title node)
                  (org-files-db--result-title result))
           (equal (org-files-db--get node 'level)
                  (org-files-db--get result 'level))))))

(defun org-files-db--source-outline-nodes (result)
  "Build outline context for RESULT from its current source file."
  (let ((file (org-files-db--result-file result)))
    (when (and file
               (file-readable-p file)
               (eq (org-files-db--kind result) 'heading))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (save-restriction
            (widen)
            (org-files-db--goto-result-location result)
            (org-back-to-heading t)
            (let ((continue t)
                  nodes)
              (while continue
                (push `((kind . "heading")
                        (title . ,(org-get-heading t t t t))
                        (level . ,(org-outline-level))
                        (location . ((file_path . ,file)
                                     (line . ,(line-number-at-pos))
                                     (byte_start . nil))))
                      nodes)
                (setq continue (org-up-heading-safe)))
              (let* ((keywords (org-collect-keywords '("TITLE")))
                     (titles (cdr (assoc "TITLE" keywords)))
                     (file-title (or (car-safe titles)
                                     (file-name-base file))))
                (cons `((kind . "root")
                        (title . ,file-title)
                        (level . 0)
                        (location . ((file_path . ,file)
                                     (line . 1)
                                     (byte_start . 0))))
                      nodes)))))))))

(defun org-files-db--result-outline-nodes (result)
  "Return complete outline path nodes for RESULT."
  (let ((nodes (copy-sequence
                (or (org-files-db--result-path-nodes result)
                    (org-files-db--source-outline-nodes result)
                    nil))))
    (if (and nodes (org-files-db--same-node-p (car (last nodes)) result))
        (setcar (last nodes) result)
      (setq nodes (append nodes (list result))))
    nodes))

(defun org-files-db--tree-add (tree nodes)
  "Add NODES to outline TREE and return TREE."
  (if (null nodes)
      tree
    (let* ((node (car nodes))
           (key (org-files-db--path-node-key node))
           (entry (seq-find (lambda (item)
                              (equal (plist-get item :key) key))
                            tree)))
      (unless entry
        (setq entry (list :key key :data node :children nil)
              tree (append tree (list entry))))
      (when (cdr nodes)
        (setf (plist-get entry :children)
              (org-files-db--tree-add
               (plist-get entry :children)
               (cdr nodes))))
      tree)))

(defun org-files-db--outline-tree (results)
  "Build an ordered outline tree for RESULTS."
  (let (tree)
    (dolist (result results tree)
      (setq tree (org-files-db--tree-add
                  tree (org-files-db--result-outline-nodes result))))))

(defun org-files-db--render-tree (tree level)
  "Render outline TREE beginning at Org LEVEL."
  (mapconcat
   (lambda (entry)
     (concat (make-string level ?*) " "
             (org-files-db--export-result-text (plist-get entry :data)) "\n"
             (org-files-db--render-tree (plist-get entry :children)
                                        (1+ level))))
   tree
   ""))

(defun org-files-db--render-org-results (results layout &optional base-level)
  "Render RESULTS as Org using LAYOUT below BASE-LEVEL."
  (let ((level (1+ (or base-level 0))))
    (pcase layout
      ('flat
       (mapconcat
        (lambda (result)
          (format "%s %s\n"
                  (make-string level ?*)
                  (org-files-db--export-result-text result)))
        results
        ""))
      ('outline
       (org-files-db--render-tree (org-files-db--outline-tree results) level))
      (_ (user-error "Unsupported Org result layout: %s" layout)))))

(defun org-files-db-embark-export-org (candidates)
  "Export org-files-db CANDIDATES to a new Org buffer."
  (let ((results
         (delq nil
               (mapcar (lambda (candidate)
                         (get-text-property 0 'org-files-db-result candidate))
                       candidates))))
    (unless results
      (user-error "No org-files-db results to export"))
    (let ((buffer (generate-new-buffer "*org-files-db export*")))
      (with-current-buffer buffer
        (org-mode)
        (insert "#+TITLE: org-files-db results\n\n")
        (insert (org-files-db--render-org-results
                 results org-files-db-export-layout 0))
        (goto-char (point-min))
        (set-buffer-modified-p nil))
      (pop-to-buffer buffer))))

(defun org-files-db--register-embark-exporter ()
  "Register the org-files-db exporter with Embark."
  (setf (alist-get org-files-db--completion-category
                   embark-exporters-alist)
        #'org-files-db-embark-export-org))

(defun org-files-db--register-embark-exporter-after-load (_file)
  "Register the Embark exporter once Embark has loaded."
  (when (featurep 'embark)
    (org-files-db--register-embark-exporter)
    (remove-hook 'after-load-functions
                 #'org-files-db--register-embark-exporter-after-load)))

(if (featurep 'embark)
    (org-files-db--register-embark-exporter)
  (add-hook 'after-load-functions
            #'org-files-db--register-embark-exporter-after-load))

(provide 'org-files-db-export)

;;; org-files-db-export.el ends here
