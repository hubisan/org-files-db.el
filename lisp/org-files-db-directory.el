;;; org-files-db-directory.el --- Directory actions -*- lexical-binding: t; -*-

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

;; Rename or move a directory and update the indexed file links: incoming
;; links to files inside it and outgoing relative links from files inside it.
;; The change is planned from guarded orgfdb queries, shown for confirmation
;; and rolled back when a step fails.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-files-db-core)
(require 'org-files-db-process)
(require 'org-files-db-actions)
(require 'org-files-db-query)

(defconst org-files-db-directory--plan-buffer "*org-files-db rename directory*"
  "Name of the buffer showing the planned directory rename or move.")

(defun org-files-db-directory--inside-p (file directory)
  "Return non-nil when FILE is inside DIRECTORY, which has no trailing slash."
  (string-prefix-p (file-name-as-directory directory) file))

(defun org-files-db-directory--quote (string)
  "Return STRING escaped for a double-quoted orgfdb query string."
  (replace-regexp-in-string "[\\\"]" "\\\\\\&" string t))

(defun org-files-db-directory--incoming-query (old)
  "Return the query for links to files inside directory OLD."
  (format "(links (target (files (file-path \"%s\" :regexp t))))"
          (org-files-db-directory--quote
           (concat "^" (regexp-quote (file-name-as-directory old))))))

(defun org-files-db-directory--outgoing-query (old)
  "Return the query for links from files inside directory OLD."
  (format "(links (source (files (file-path \"%s\" :regexp t))))"
          (org-files-db-directory--quote
           (concat "^" (regexp-quote (file-name-as-directory old))))))

(defun org-files-db-directory--affected-links (old config)
  "Return the indexed file links that must change for moving OLD.
CONFIG is the configuration name. Keep incoming links from files outside OLD
and from files inside OLD with an absolute path, and outgoing relative links
to targets outside OLD. Signal a user error when the index changed between the
status check and a query."
  (condition-case nil
      (let* ((status (org-files-db-process--call-json
                      (append '("status" "--format" "json")
                              (org-files-db-process--config-arguments config))))
             (query (lambda (string)
                      (org-files-db-query--guarded-json
                       string nil
                       :config config
                       :database-id (alist-get 'database_id status)
                       :generation (alist-get 'generation status))))
             (incoming (funcall query (org-files-db-directory--incoming-query old)))
             (outgoing (funcall query (org-files-db-directory--outgoing-query old))))
        (append
         (seq-filter
          (lambda (link)
            (and (equal (alist-get 'link_type link) "file")
                 (alist-get 'path_absolute link)
                 (or (not (org-files-db-directory--inside-p
                           (org-files-db-directory--link-file link) old))
                     (file-name-absolute-p (alist-get 'link_path link)))))
          incoming)
         (seq-filter
          (lambda (link)
            (and (equal (alist-get 'link_type link) "file")
                 (alist-get 'path_absolute link)
                 (not (file-name-absolute-p (alist-get 'link_path link)))
                 (not (string-prefix-p "~" (alist-get 'link_path link)))
                 (not (org-files-db-directory--inside-p
                       (alist-get 'path_absolute link) old))))
          outgoing)))
    (org-files-db-stale-index
     (user-error "Index changed, run the query again"))))

(defun org-files-db-directory--moved (path old new)
  "Return PATH, inside directory OLD, with the prefix OLD replaced by NEW."
  (concat new (substring path (length old))))

(defun org-files-db-directory--replacement (link old new)
  "Return the new text of LINK when directory OLD becomes NEW.
An incoming link follows its moved target. An outgoing link keeps its target
and is made relative to the moved source file."
  (let ((file (alist-get 'file_path (alist-get 'location link)))
        (target (alist-get 'path_absolute link)))
    (if (org-files-db-directory--inside-p target old)
        (org-files-db-actions--link-replacement
         link file (org-files-db-directory--moved target old new))
      (org-files-db-actions--link-replacement
       link (org-files-db-directory--moved file old new) target))))

(defun org-files-db-directory--link-start (link)
  "Return the byte start of LINK."
  (alist-get 'byte_start (alist-get 'location link)))

(defun org-files-db-directory--link-file (link)
  "Return the path of the file holding LINK."
  (alist-get 'file_path (alist-get 'location link)))

(defun org-files-db-directory--file-text (file)
  "Return the current text of FILE, from its buffer when one visits it."
  (if-let* ((buffer (find-buffer-visiting file)))
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun org-files-db-directory--plan (links old new)
  "Return the rename plan for LINKS when renaming OLD to NEW.
Links whose text stays the same are left out. The plan is an alist.
`changes' and `skipped' are lists of entries
\(FILE LINK TEXT) in file and position order, TEXT being the new link text
and nil when skipped. `files' maps each file to change to its links, last
position first. Links whose text differs from the index are skipped."
  (let ((by-file nil) changes skipped files)
    (dolist (link links)
      (push link (alist-get (org-files-db-directory--link-file link)
                            by-file nil nil #'equal)))
    (dolist (entry (sort by-file (lambda (a b) (string< (car a) (car b)))))
      (let* ((file (car entry))
             (sorted (sort (cdr entry)
                           (lambda (a b)
                             (< (org-files-db-directory--link-start a)
                                (org-files-db-directory--link-start b)))))
             (content (and (file-readable-p file)
                           (org-files-db-directory--file-text file)))
             (applied nil))
        (with-temp-buffer
          (when content (insert content))
          (dolist (link sorted)
            (let ((text (org-files-db-directory--replacement link old new)))
              (cond
               ((equal text (alist-get 'raw link)))
               ((and content (org-files-db-actions--link-region link))
                (push (list file link text) changes)
                (push link applied))
               (t (push (list file link nil) skipped))))))
        (when applied
          (push (cons file applied) files))))
    `((changes . ,(nreverse changes))
      (skipped . ,(nreverse skipped))
      (files . ,(nreverse files)))))

(defun org-files-db-directory--modified-buffers (old files)
  "Return the modified buffers visiting a file of FILES or inside OLD."
  (seq-filter
   (lambda (buffer)
     (when-let* ((name (buffer-file-name buffer)))
       (and (buffer-modified-p buffer)
            (or (member name files)
                (org-files-db-directory--inside-p name old)))))
   (buffer-list)))

(defun org-files-db-directory--line (entry)
  "Return the FILE:LINE prefix for plan ENTRY."
  (format "%s:%s" (abbreviate-file-name (car entry))
          (alist-get 'line (alist-get 'location (nth 1 entry)))))

(defun org-files-db-directory--moving-p (old new)
  "Return non-nil when renaming OLD to NEW changes the parent directory."
  (not (equal (file-name-directory old) (file-name-directory new))))

(defun org-files-db-directory--show-plan (old new plan)
  "Display the PLAN to rename or move directory OLD to NEW."
  (with-current-buffer (get-buffer-create org-files-db-directory--plan-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%s %s → %s\n\n"
                      (if (org-files-db-directory--moving-p old new)
                          "Move" "Rename")
                      (abbreviate-file-name old)
                      (abbreviate-file-name new)))
      (dolist (entry (alist-get 'changes plan))
        (insert (format "%s  %s → %s\n" (org-files-db-directory--line entry)
                        (alist-get 'raw (nth 1 entry)) (nth 2 entry))))
      (when-let* ((skipped (alist-get 'skipped plan)))
        (insert "\nSkipped:\n")
        (dolist (entry skipped)
          (insert (format "%s  %s\n" (org-files-db-directory--line entry)
                          (alist-get 'raw (nth 1 entry))))))
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer))))

(defun org-files-db-directory--read-bytes (file)
  "Return the exact bytes of FILE as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun org-files-db-directory--write-bytes (bytes file)
  "Write the unibyte string BYTES to FILE."
  (let ((coding-system-for-write 'no-conversion))
    (write-region bytes nil file nil 'silent)))

(defun org-files-db-directory--edit-file (file links old new)
  "Rewrite LINKS in FILE, which are sorted by descending position, and save it.
OLD and NEW are the moved directory before and after."
  (let ((buffer (find-buffer-visiting file)))
    (with-current-buffer (or buffer (generate-new-buffer " *org-files-db edit*"))
      (unwind-protect
          (progn
            (unless buffer (insert-file-contents file))
            (save-excursion
              (save-restriction
                (widen)
                (dolist (link links)
                  (when-let* ((region (org-files-db-actions--link-region link)))
                    (goto-char (car region))
                    (delete-region (car region) (cdr region))
                    (insert (org-files-db-directory--replacement
                             link old new))))))
            (if buffer
                (save-buffer)
              (write-region nil nil file nil 'silent)))
        (unless buffer (kill-buffer))))))

(defun org-files-db-directory--restore (originals old new renamed retargeted)
  "Undo a failed rename of OLD to NEW and return (RESTORED . NOT-RESTORED).
ORIGINALS maps edited files to their original bytes. RENAMED is non-nil when
the directory was moved. RETARGETED maps buffers to their original file."
  (let (restored failed)
    (cl-flet ((attempt (what fn)
                (condition-case err
                    (progn (funcall fn) (push what restored))
                  (error (push (format "%s (%s)" what
                                       (error-message-string err))
                               failed)))))
      (when renamed
        (attempt (abbreviate-file-name old) (lambda () (rename-file new old))))
      (dolist (entry retargeted)
        (when (buffer-live-p (car entry))
          (with-current-buffer (car entry)
            (unless (equal buffer-file-name (cdr entry))
              (attempt (buffer-name) (lambda ()
                                       (set-visited-file-name (cdr entry) t)
                                       (set-buffer-modified-p nil)))))))
      (dolist (entry originals)
        (attempt (abbreviate-file-name (car entry))
                 (lambda ()
                   (org-files-db-directory--write-bytes (cdr entry) (car entry))
                   (when-let* ((buffer (find-buffer-visiting (car entry))))
                     (with-current-buffer buffer
                       (revert-buffer t t t)))))))
    (cons (nreverse restored) (nreverse failed))))

(defun org-files-db-directory--apply (old new plan)
  "Edit the links of PLAN, then rename directory OLD to NEW.
Restore the previous state and signal a user error when a step fails."
  (let* ((files (alist-get 'files plan))
         (outside (seq-remove (lambda (entry)
                                (org-files-db-directory--inside-p (car entry) old))
                              files))
         (inside (seq-filter (lambda (entry)
                               (org-files-db-directory--inside-p (car entry) old))
                             files))
         (visiting (seq-filter
                    (lambda (buffer)
                      (when-let* ((name (buffer-file-name buffer)))
                        (org-files-db-directory--inside-p name old)))
                    (buffer-list)))
         (retargeted (mapcar (lambda (buffer)
                               (cons buffer (buffer-file-name buffer)))
                             visiting))
         originals renamed)
    (condition-case err
        (progn
          (dolist (entry (append outside inside))
            (push (cons (car entry)
                        (org-files-db-directory--read-bytes (car entry)))
                  originals))
          (dolist (entry (append outside inside))
            (org-files-db-directory--edit-file
             (car entry) (cdr entry) old new))
          (rename-file old new)
          (setq renamed t)
          (dolist (entry retargeted)
            (org-files-db-actions--rename-visiting-buffer
             (cdr entry) (org-files-db-directory--moved (cdr entry) old new))))
      (error
       (let ((result (org-files-db-directory--restore
                      (nreverse originals) old new renamed retargeted)))
         (user-error "Directory rename failed: %s; restored: %s; not restored: %s"
                     (error-message-string err)
                     (if (car result) (string-join (car result) ", ") "nothing")
                     (if (cdr result) (string-join (cdr result) ", ")
                       "nothing")))))))

;;;###autoload
(defun org-files-db-rename-directory (directory new-name &optional config)
  "Rename or move DIRECTORY to NEW-NAME and update file links.
NEW-NAME is a path, relative to the parent of DIRECTORY when not absolute. Its
parent directory must exist. Update the indexed incoming links to files inside
DIRECTORY and the outgoing relative links of files inside it that point
outside, using the configuration CONFIG, the default one when nil. Show the
planned changes and ask for confirmation before anything changes. When a step
fails, restore the previous state. Return nil.

With an interactive prefix argument, select the configuration."
  (interactive
   (let* ((directory (read-directory-name "Rename or move directory: "
                                          nil nil t))
          (parent (file-name-directory (directory-file-name
                                        (expand-file-name directory)))))
     (list directory
           (read-directory-name "Rename or move to: " parent nil nil
                                (file-name-nondirectory
                                 (directory-file-name directory)))
           (org-files-db-process--interactive-config-name current-prefix-arg))))
  (let* ((old (directory-file-name (expand-file-name directory)))
         (new (directory-file-name
               (expand-file-name new-name (file-name-directory old))))
         (config-name (org-files-db-process--config-name config))
         (moving (org-files-db-directory--moving-p old new)))
    (unless (file-directory-p old)
      (user-error "Not a directory: %s" old))
    (when (org-files-db-directory--inside-p new old)
      (user-error "Destination is inside the directory: %s" new))
    (when (file-exists-p new)
      (user-error "Destination already exists: %s" new))
    (unless (file-directory-p (file-name-directory new))
      (user-error "Destination parent does not exist: %s"
                  (file-name-directory new)))
    (let* ((links (org-files-db-directory--affected-links old config-name))
           (plan (org-files-db-directory--plan links old new))
           (changes (alist-get 'changes plan))
           (skipped (length (alist-get 'skipped plan)))
           (files (mapcar #'car (alist-get 'files plan)))
           (modified (org-files-db-directory--modified-buffers old files)))
      (when modified
        (user-error "Save or discard changes first: %s"
                    (mapconcat #'buffer-name modified ", ")))
      (org-files-db-directory--show-plan old new plan)
      (if (not (yes-or-no-p
                (format "%s directory and update %d links in %d files? "
                        (if moving "Move" "Rename")
                        (length changes) (length files))))
          (message (if moving "Directory not moved" "Directory not renamed"))
        (org-files-db-directory--apply old new plan)
        (message "Directory %s, %d links updated%s"
                 (if moving "moved" "renamed") (length changes)
                 (if (> skipped 0) (format ", %d skipped" skipped) "")))
      nil)))

(provide 'org-files-db-directory)

;;; org-files-db-directory.el ends here
