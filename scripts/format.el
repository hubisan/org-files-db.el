;;; format.el --- Indent Emacs Lisp files with the package loaded -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage: emacs --batch -L lisp -l scripts/format.el FILE...
;;
;; Loads every package module (and Buttercup when it is available) so that
;; `declare indent' specs of macros apply, evaluates the `defmacro' forms
;; of each file before indenting it, and saves the file when it changed.

;;; Code:

(require 'cl-lib)

(dolist (file (directory-files "lisp" t "\\`org-files-db.*\\.el\\'"))
  (require (intern (file-name-base file))))
(require 'buttercup nil t)

(defun org-files-db-format--eval-macros ()
  "Evaluate the top-level `defmacro' forms of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (when (and (consp form) (eq (car form) 'defmacro))
              (eval form t))))
      (end-of-file nil))))

(dolist (file command-line-args-left)
  (with-current-buffer (find-file-noselect file)
    (emacs-lisp-mode)
    (org-files-db-format--eval-macros)
    (indent-region (point-min) (point-max))
    (when (buffer-modified-p)
      (save-buffer)
      (message "Formatted %s" file))
    (kill-buffer)))

(setq command-line-args-left nil)

;;; format.el ends here
