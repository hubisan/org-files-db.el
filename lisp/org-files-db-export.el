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
(require 'org-files-db-core)

(declare-function embark-export "embark")
(defvar embark-exporters-alist)

(defun org-files-db-export--rebased-heading-link (info)
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

(defun org-files-db-export--preserved-linked-heading (info)
  "Return linked heading text using INFO."
  (when-let* ((link (org-files-db-export--rebased-heading-link info)))
    (concat (or (plist-get info :prefix) "")
            link
            (or (plist-get info :suffix) ""))))

(defun org-files-db-export--resolved-linked-heading (info)
  "Return an Org link to the effective target represented by INFO."
  (save-current-buffer
    (save-window-excursion
      (let ((target (org-files-db--goto-linked-target info)))
        (org-files-db--result-org-link target
                                       (org-files-db--result-title target))))))

(defun org-files-db-export--result-text (result)
  "Return Org text representing RESULT."
  (if (not (org-files-db--result-file result))
      (org-files-db--node-title result)
    (if (eq (org-files-db--kind result) 'heading)
        (if-let* ((info (org-files-db--heading-link-info result)))
            (condition-case nil
                (pcase org-files-db-export-linked-heading-style
                  ('resolve (org-files-db-export--resolved-linked-heading info))
                  (_ (org-files-db-export--preserved-linked-heading info)))
              (error
               (or (org-files-db-export--preserved-linked-heading info)
                   (org-files-db--result-org-link result))))
          (org-files-db--result-org-link result))
      (org-files-db--result-org-link result))))

(defun org-files-db-export--path-node-key (node)
  "Return a stable key for outline path NODE."
  (format "%s:%s:%s:%s:%s:%s:%s"
          (or (org-files-db--get node 'kind) "")
          (or (org-files-db--get node 'id) "")
          (or (org-files-db--get node 'file_id) "")
          (or (org-files-db--get node 'location 'file_path) "")
          (or (org-files-db--get node 'location 'line) "")
          (or (org-files-db--get node 'level) "")
          (org-files-db--node-title node)))

(defun org-files-db-export--same-node-p (node result)
  "Return non-nil when path NODE denotes RESULT."
  (let ((node-id (org-files-db--get node 'id))
        (result-id (org-files-db--get result 'id)))
    (if (and node-id result-id)
        (equal node-id result-id)
      (and (equal (org-files-db--node-title node)
                  (org-files-db--result-title result))
           (equal (org-files-db--get node 'level)
                  (org-files-db--get result 'level))))))

(defun org-files-db-export--source-outline-nodes (result)
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

(defun org-files-db-export--result-outline-nodes (result)
  "Return complete outline path nodes for RESULT."
  (let ((nodes (copy-sequence
                (or (org-files-db--result-path-nodes result)
                    (org-files-db-export--source-outline-nodes result)
                    nil))))
    (if (and nodes (org-files-db-export--same-node-p (car (last nodes)) result))
        (setcar (last nodes) result)
      (setq nodes (append nodes (list result))))
    nodes))

(defun org-files-db-export--tree-add (tree nodes)
  "Add NODES to outline TREE and return TREE."
  (if (null nodes)
      tree
    (let* ((node (car nodes))
           (key (org-files-db-export--path-node-key node))
           (entry (seq-find (lambda (item)
                              (equal (plist-get item :key) key))
                            tree)))
      (unless entry
        (setq entry (list :key key :data node :children nil)
              tree (append tree (list entry))))
      (when (cdr nodes)
        (setf (plist-get entry :children)
              (org-files-db-export--tree-add
               (plist-get entry :children)
               (cdr nodes))))
      tree)))

(defun org-files-db-export--outline-tree (results)
  "Build an ordered outline tree for RESULTS."
  (let (tree)
    (dolist (result results tree)
      (setq tree (org-files-db-export--tree-add
                  tree (org-files-db-export--result-outline-nodes result))))))

(defun org-files-db-export--render-tree (tree level)
  "Render outline TREE beginning at Org LEVEL."
  (mapconcat
   (lambda (entry)
     (concat (make-string level ?*) " "
             (org-files-db-export--result-text (plist-get entry :data)) "\n"
             (org-files-db-export--render-tree
              (plist-get entry :children)
              (1+ level))))
   tree
   ""))

(defun org-files-db-export-render-results (results layout &optional base-level)
  "Render RESULTS as Org using LAYOUT below BASE-LEVEL."
  (let ((level (1+ (or base-level 0))))
    (pcase layout
      ('flat
       (mapconcat
        (lambda (result)
          (format "%s %s\n"
                  (make-string level ?*)
                  (org-files-db-export--result-text result)))
        results
        ""))
      ('outline
       (org-files-db-export--render-tree
        (org-files-db-export--outline-tree results) level))
      (_ (user-error "Unsupported Org result layout: %s" layout)))))

(defun org-files-db-export-embark-org (candidates)
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
        (insert (org-files-db-export-render-results
                 results org-files-db-export-layout 0))
        (goto-char (point-min))
        (set-buffer-modified-p nil))
      (pop-to-buffer buffer))))

(defun org-files-db-export--register-embark-exporter ()
  "Register the org-files-db exporter with Embark."
  (setf (alist-get org-files-db--completion-category
                   embark-exporters-alist)
        #'org-files-db-export-embark-org))

(defun org-files-db-export--register-embark-exporter-after-load (_file)
  "Register the Embark exporter once Embark has loaded."
  (when (featurep 'embark)
    (org-files-db-export--register-embark-exporter)
    (remove-hook 'after-load-functions
                 #'org-files-db-export--register-embark-exporter-after-load)))

(if (featurep 'embark)
    (org-files-db-export--register-embark-exporter)
  (add-hook 'after-load-functions
            #'org-files-db-export--register-embark-exporter-after-load))

(provide 'org-files-db-export)

;;; org-files-db-export.el ends here
