;;; check-offline.el --- Helpers for scripts/check-offline.sh -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch entry points used by scripts/check-offline.sh.

;;; Code:

(require 'checkdoc)

(defun org-files-db-check-offline-checkdoc ()
  "Run checkdoc on lisp/*.el and exit non-zero when it reports anything."
  (setq sentence-end-double-space nil
        checkdoc-verb-check-experimental-flag nil)
  (let ((failed nil))
    (dolist (file (directory-files "lisp" t "\\.el\\'"))
      (with-current-buffer (find-file-noselect file)
        (checkdoc-current-buffer t)
        (when-let* ((buffer (get-buffer checkdoc-diagnostic-buffer)))
          (with-current-buffer buffer
            (when (string-match-p "\\.el:[0-9]+: " (buffer-string))
              (princ (buffer-string))
              (setq failed t))
            (let ((inhibit-read-only t))
              (erase-buffer))))))
    (kill-emacs (if failed 1 0))))

(defun org-files-db-check-offline-abort-on-error ()
  "Make `error' abort the run, as Eask does for Buttercup tests."
  (advice-add 'error :around
              (lambda (_ &rest args)
                (message "%s" (apply #'format-message args))
                (kill-emacs 1))))

;;; check-offline.el ends here
