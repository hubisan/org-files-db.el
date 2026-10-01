;;; org-files-db-cache.el --- Rust view cache integration -*- lexical-binding: t; -*-

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

;; Lifecycle management for Rust materialized presentation views.
;; Emacs stores registration metadata only and never caches result payloads.

;;; Code:

(require 'cl-lib)
(require 'org-files-db-core)
(require 'org-files-db-presentation)
(require 'org-files-db-process)
(require 'org-files-db-query)
(require 'org-files-db-views)
(require 'org-files-db-watch)
(require 'seq)
(require 'subr-x)

(defvar org-files-db-cache-mode)

(defvar org-files-db-cache--session-id
  (substring
   (secure-hash
    'sha256
    (format "%s:%s:%s:%s"
            (emacs-pid) (float-time) (random) system-name))
   0 16)
  "Private identifier for Rust view names created by this Emacs session.")

(cl-defstruct (org-files-db-cache--entry
               (:constructor org-files-db-cache--entry-create))
  "One Rust registration owned by the current cache activation."
  resolved
  rust-name)

(cl-defstruct (org-files-db-cache--activation
               (:constructor org-files-db-cache--activation-create))
  "Snapshot of Rust registrations created by one cache activation."
  entries)

(defvar org-files-db-cache--activation nil
  "Snapshot owned by the current `org-files-db-cache-mode' activation.")

(defun org-files-db-cache--rust-name (name)
  "Return a private Rust view name for user view NAME."
  (let* ((safe (replace-regexp-in-string "[^[:alnum:]_.-]" "_" name))
         (safe (if (> (length safe) 32) (substring safe 0 32) safe))
         (digest (substring (secure-hash 'sha256 name) 0 10)))
    (format "__org-files-db-emacs-%s-%s-%s"
            org-files-db-cache--session-id safe digest)))

(defun org-files-db-cache--presentation-spec-json (resolved)
  "Return PresentationSpec JSON for RESOLVED view data."
  (org-files-db-presentation--spec-json
   (org-files-db-views--resolved-columns resolved)
   (org-files-db-views--resolved-sort resolved)
   (org-files-db-views--resolved-row-source resolved)))

(defun org-files-db-cache--include-arguments (resolved)
  "Return explicit orgfdb include arguments for RESOLVED."
  (let ((includes (org-files-db-views--resolved-action-includes resolved)))
    (when includes
      (list "--include" (string-join includes ",")))))

(defun org-files-db-cache--register-entry (entry)
  "Register the Rust view represented by cache ENTRY."
  (let* ((resolved (org-files-db-cache--entry-resolved entry))
         (arguments
          (append
           (list "view" "register"
                 "--config" (org-files-db-views--resolved-config-file resolved)
                 "--output" "flat")
           (org-files-db-cache--include-arguments resolved)
           (list "--presentation-spec-json"
                 (org-files-db-cache--presentation-spec-json resolved)
                 (org-files-db-cache--entry-rust-name entry)
                 (org-files-db-views--resolved-query-string resolved)))))
    (org-files-db-process--call-json arguments)))

(defun org-files-db-cache--remove-entry (entry)
  "Remove the Rust registration represented by cache ENTRY."
  (let ((resolved (org-files-db-cache--entry-resolved entry)))
    (org-files-db-process--call-json
     (list "view" "remove"
           "--config" (org-files-db-views--resolved-config-file resolved)
           (org-files-db-cache--entry-rust-name entry)))))

(defun org-files-db-cache--resolved-views ()
  "Return resolved views that opt in to Rust caching."
  (seq-filter #'org-files-db-views--resolved-cache
              (org-files-db-views--resolved-views)))

(defun org-files-db-cache--ensure-watchers (resolved-views)
  "Require active watchers for all RESOLVED-VIEWS."
  (let ((checked (make-hash-table :test #'equal)))
    (dolist (resolved resolved-views)
      (let ((config-file (org-files-db-views--resolved-config-file resolved)))
        (unless (gethash config-file checked)
          (puthash config-file t checked)
          (unless (org-files-db-watch--probe-active-p config-file)
            (user-error
             "Watcher for configuration `%s' is not active. Restart org-files-db-watch-mode"
             (org-files-db-views--resolved-config resolved))))))))

(defun org-files-db-cache--rollback-entries (entries)
  "Best-effort remove Rust registrations in ENTRIES."
  (dolist (entry entries)
    (condition-case err
        (org-files-db-cache--remove-entry entry)
      (error
       (display-warning
        'org-files-db
        (format "Failed to roll back cached view `%s': %s"
                (org-files-db-views--resolved-name
                 (org-files-db-cache--entry-resolved entry))
                (error-message-string err))
        :warning)))))

(defun org-files-db-cache--activate ()
  "Activate Rust caching for the current view-definition snapshot."
  (let ((watch-was-active org-files-db-watch-mode)
        resolved-views
        registered
        success)
    (unwind-protect
        (progn
          (unless watch-was-active
            (org-files-db-watch-start))
          (setq resolved-views (org-files-db-cache--resolved-views))
          (org-files-db-cache--ensure-watchers resolved-views)
          (dolist (resolved resolved-views)
            (let ((entry
                   (org-files-db-cache--entry-create
                    :resolved resolved
                    :rust-name
                    (org-files-db-cache--rust-name
                     (org-files-db-views--resolved-name resolved)))))
              (push entry registered)
              (org-files-db-cache--register-entry entry)))
          (setq registered (nreverse registered)
                org-files-db-cache--activation
                (org-files-db-cache--activation-create
                 :entries registered)
                success t)
          org-files-db-cache--activation)
      (unless success
        (org-files-db-cache--rollback-entries registered)
        (setq org-files-db-cache--activation nil)
        (when (and (not watch-was-active) org-files-db-watch-mode)
          (let ((org-files-db-cache-mode nil))
            (org-files-db-watch-stop)))))))

(defun org-files-db-cache--remove-activation (&optional quiet)
  "Remove registrations from the active snapshot.
When QUIET is non-nil, report cleanup failures as warnings."
  (let* ((activation org-files-db-cache--activation)
         (entries
          (and activation
               (org-files-db-cache--activation-entries activation)))
         first-error)
    (dolist (entry entries)
      (condition-case err
          (org-files-db-cache--remove-entry entry)
        (error
         (if quiet
             (display-warning
              'org-files-db
              (format "Failed to remove cached view `%s': %s"
                      (org-files-db-views--resolved-name
                       (org-files-db-cache--entry-resolved entry))
                      (error-message-string err))
              :warning)
           (unless first-error
             (setq first-error err))))))
    (setq org-files-db-cache--activation nil)
    (when first-error
      (signal (car first-error) (cdr first-error)))))

(defun org-files-db-cache--entry (name)
  "Return the active cache entry for user view NAME, or nil."
  (let ((activation org-files-db-cache--activation))
    (when activation
      (seq-find
       (lambda (entry)
         (equal name
                (org-files-db-views--resolved-name
                 (org-files-db-cache--entry-resolved entry))))
       (org-files-db-cache--activation-entries activation)))))

(defun org-files-db-cache--resolved-signature (resolved)
  "Return the cache-relevant definition signature for RESOLVED."
  (list
   (org-files-db-views--resolved-name resolved)
   (org-files-db-views--resolved-config resolved)
   (org-files-db-views--resolved-config-file resolved)
   (org-files-db-views--resolved-query resolved)
   (org-files-db-views--resolved-target resolved)
   (org-files-db-views--resolved-columns resolved)
   (org-files-db-views--resolved-sort resolved)
   (org-files-db-views--resolved-row-source resolved)
   (org-files-db-views--resolved-cache resolved)
   (org-files-db-views--resolved-action resolved)
   (org-files-db-views--resolved-action-includes resolved)))

(defun org-files-db-cache--raw-view-by-name (name)
  "Return the only current raw view named NAME, or nil."
  (let ((matches
         (seq-filter
          (lambda (view)
            (and (consp view) (equal (car view) name)))
          org-files-db-views)))
    (and (= (length matches) 1) (car matches))))

(defun org-files-db-cache--definition-error (name)
  "Signal the cache restart error for user view NAME."
  (user-error
   "Cached view `%s' changed. Restart org-files-db-cache-mode to apply the new definition"
   name))

(defun org-files-db-cache--assert-definition-current (entry)
  "Require current Lisp definition to match cached ENTRY."
  (let* ((stored (org-files-db-cache--entry-resolved entry))
         (name (org-files-db-views--resolved-name stored))
         current)
    (condition-case nil
        (let ((view (org-files-db-cache--raw-view-by-name name)))
          (unless view
            (org-files-db-cache--definition-error name))
          (setq current (org-files-db-views--resolve view)))
      (error (org-files-db-cache--definition-error name)))
    (unless (equal (org-files-db-cache--resolved-signature stored)
                   (org-files-db-cache--resolved-signature current))
      (org-files-db-cache--definition-error name))))

(defun org-files-db-cache--view-read-result (entry)
  "Run one Rust view read for cache ENTRY and return process data."
  (let ((resolved (org-files-db-cache--entry-resolved entry)))
    (org-files-db-process--run-process
     (list "view" "read"
           "--config" (org-files-db-views--resolved-config-file resolved)
           (org-files-db-cache--entry-rust-name entry)))))

(defun org-files-db-cache--decode-read-result (result resolved)
  "Decode successful Rust view read RESULT for RESOLVED."
  (let ((presentation
         (org-files-db-presentation--decode
          (org-files-db-process--parse-json (plist-get result :stdout)))))
    (setf (org-files-db-presentation-config presentation)
          (org-files-db-views--resolved-config resolved))
    presentation))

(defun org-files-db-cache--read-entry (entry &optional retried)
  "Read cache ENTRY and return its decoded presentation.
When RETRIED is non-nil, do not attempt another missing-view recovery."
  (let* ((resolved (org-files-db-cache--entry-resolved entry))
         (result (org-files-db-cache--view-read-result entry))
         (status (plist-get result :status))
         (stderr (or (plist-get result :stderr) "")))
    (cond
     ((zerop status)
      (org-files-db-cache--decode-read-result result resolved))
     ((and (not retried)
           (org-files-db-watch--view-not-found-p stderr))
      (org-files-db-cache--register-entry entry)
      (org-files-db-cache--read-entry entry t))
     (t
      (org-files-db-process--signal-cli-error status stderr)))))

(defun org-files-db-cache--run-cached (entry)
  "Read cache ENTRY and run its configured result action."
  (org-files-db-cache--assert-definition-current entry)
  (let* ((resolved (org-files-db-cache--entry-resolved entry))
         (presentation (org-files-db-cache--read-entry entry)))
    (org-files-db-query--run-presentation-action
     presentation
     (org-files-db-views--resolved-target resolved)
     (org-files-db-views--resolved-action resolved))))

(defun org-files-db-cache--run-view (name)
  "Run predefined view NAME while cache mode is active."
  (let ((entry (org-files-db-cache--entry name)))
    (if entry
        (org-files-db-cache--run-cached entry)
      (let ((resolved
             (org-files-db-views--resolve
              (org-files-db-views--get name))))
        (when (org-files-db-views--resolved-cache resolved)
          (org-files-db-cache--definition-error name))
        (org-files-db-views--run-one-shot resolved)))))

;;;###autoload
(define-minor-mode org-files-db-cache-mode
  "Use Rust materialized views for predefined views with :cache t."
  :global t
  :group 'org-files-db
  :lighter nil
  (if org-files-db-cache-mode
      (unless org-files-db-cache--activation
        (let (success)
          (unwind-protect
              (progn
                (org-files-db-cache--activate)
                (setq success t))
            (unless success
              (setq org-files-db-cache-mode nil)))))
    (org-files-db-cache--remove-activation)))

;;;###autoload
(defun org-files-db-cache-start ()
  "Enable `org-files-db-cache-mode'."
  (interactive)
  (org-files-db-cache-mode 1))

;;;###autoload
(defun org-files-db-cache-stop (&optional stop-watch)
  "Disable `org-files-db-cache-mode'.
When STOP-WATCH is non-nil, also disable `org-files-db-watch-mode'."
  (interactive "P")
  (let (cache-error)
    (condition-case err
        (org-files-db-cache-mode -1)
      (error (setq cache-error err)))
    (when stop-watch
      (org-files-db-watch-stop))
    (when cache-error
      (signal (car cache-error) (cdr cache-error))))
  nil)

(defun org-files-db-cache--cleanup-at-exit ()
  "Remove active Emacs-owned Rust view registrations at normal exit."
  (when org-files-db-cache--activation
    (org-files-db-cache--remove-activation t))
  (setq org-files-db-cache-mode nil))

(add-hook 'org-files-db-watch--before-exit-hook
          #'org-files-db-cache--cleanup-at-exit)

(provide 'org-files-db-cache)

;;; org-files-db-cache.el ends here
