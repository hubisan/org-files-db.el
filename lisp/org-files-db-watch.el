;;; org-files-db-watch.el --- Watcher lifecycle for org-files-db -*- lexical-binding: t; -*-

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

;; Lifecycle management for long-running orgfdb watcher processes.
;; Existing external watchers are reused and never become Emacs-owned.

;;; Code:

(require 'cl-lib)
(require 'org-files-db-core)
(require 'org-files-db-process)
(require 'subr-x)

(defconst org-files-db-watch--probe-view-name
  "__org-files-db-emacs-watcher-probe__"
  "Reserved view name used for the read-only watcher availability probe.")

(defconst org-files-db-watch--view-not-found-marker
  "presentation view request failed (view_not_found):"
  "Error marker returned when a watcher is reachable but the probe view is absent.")

(defconst org-files-db-watch--connection-error-marker
  "failed to connect to the active watcher presentation view registry"
  "Error marker returned when no watcher control endpoint is reachable.")

(cl-defstruct (org-files-db-watch--entry
               (:constructor org-files-db-watch--entry-create))
  config
  config-file
  ownership
  state
  process
  failure)

(defvar org-files-db-watch-mode)

(defvar org-files-db-watch--activation nil
  "Watcher state captured by the current watch-mode activation.")

(defvar org-files-db-watch--before-exit-hook nil
  "Functions that run before Emacs stops owned watchers at exit.")

(defun org-files-db-watch--configured-targets ()
  "Return validated configuration names and files for one activation."
  (let ((configs (org-files-db-process--validated-configs)))
    (unless configs
      (user-error "No org-files-db configurations are configured"))
    (mapcar
     (lambda (entry)
       (cons (car entry)
             (org-files-db-process--config-file (car entry))))
     configs)))

(defun org-files-db-watch--probe-active-p (config-file)
  "Return non-nil when CONFIG-FILE has a reachable active watcher."
  (let* ((result
          (org-files-db-process--run-process
           (list "view" "show" "--config" config-file
                 org-files-db-watch--probe-view-name)))
         (status (plist-get result :status))
         (stderr (or (plist-get result :stderr) "")))
    (cond
     ((zerop status) t)
     ((org-files-db-watch--view-not-found-p stderr) t)
     ((string-match-p
       (regexp-quote org-files-db-watch--connection-error-marker)
       stderr)
      nil)
     (t
      (org-files-db-process--signal-cli-error status stderr)))))

(defun org-files-db-watch--view-not-found-p (stderr)
  "Return non-nil when STDERR reports a missing registered view."
  (string-match-p
   (regexp-quote org-files-db-watch--view-not-found-marker)
   (or stderr "")))

(defun org-files-db-watch--buffer-string (buffer)
  "Return BUFFER contents without text properties."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-files-db-watch--ready-p (buffer)
  "Return non-nil when BUFFER contains the watcher readiness marker."
  (let ((text (or (org-files-db-watch--buffer-string buffer) "")))
    (member "watcher ready" (split-string text "\r?\n" t))))

(defun org-files-db-watch--cleanup-process-buffers (process)
  "Kill internal output buffers associated with PROCESS."
  (dolist (key '(org-files-db-watch--stdout-buffer
                 org-files-db-watch--stderr-buffer))
    (let ((buffer (process-get process key)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun org-files-db-watch--entry-failure-text (event stderr)
  "Return concise watcher failure text from EVENT and STDERR."
  (let ((stderr (string-trim (or stderr "")))
        (event (string-trim (or event ""))))
    (cond
     ((not (string-empty-p stderr)) stderr)
     ((not (string-empty-p event)) event)
     (t "Watcher process exited"))))

(defun org-files-db-watch--record-unexpected-exit (entry event stderr)
  "Record an unexpected watcher exit for ENTRY from EVENT and STDERR."
  (let ((failure (org-files-db-watch--entry-failure-text event stderr)))
    (setf (org-files-db-watch--entry-state entry) 'failed
          (org-files-db-watch--entry-failure entry) failure)
    (when org-files-db-watch-mode
      (display-warning
       'org-files-db
       (format "Watcher for configuration `%s' exited unexpectedly: %s"
               (org-files-db-watch--entry-config entry)
               failure)
       :error))))

(defun org-files-db-watch--sentinel (process event)
  "Record terminal state changes for watcher PROCESS described by EVENT."
  (when (memq (process-status process) '(exit signal failed closed))
    (let* ((entry (process-get process 'org-files-db-watch--entry))
           (expected (process-get process 'org-files-db-watch--expected-stop))
           (starting (process-get process 'org-files-db-watch--starting))
           (stderr-buffer
            (process-get process 'org-files-db-watch--stderr-buffer))
           (stderr (org-files-db-watch--buffer-string stderr-buffer)))
      (cond
       (expected
        (when entry
          (setf (org-files-db-watch--entry-state entry) 'stopped)))
       (starting
        (when entry
          (setf (org-files-db-watch--entry-state entry) 'failed
                (org-files-db-watch--entry-failure entry)
                (org-files-db-watch--entry-failure-text event stderr))))
       (entry
        (org-files-db-watch--record-unexpected-exit entry event stderr)))
      (unless starting
        (org-files-db-watch--cleanup-process-buffers process)))))

(defun org-files-db-watch--start-owned-watcher (config-name config-file)
  "Start an owned watcher for CONFIG-NAME using CONFIG-FILE.
Wait until orgfdb reports its readiness marker."
  (let* ((program (org-files-db-process--resolve-executable))
         (stdout (generate-new-buffer
                  (format " *org-files-db-watch-%s-stdout*" config-name)))
         (stderr (generate-new-buffer
                  (format " *org-files-db-watch-%s-stderr*" config-name)))
         (entry
          (org-files-db-watch--entry-create
           :config config-name
           :config-file config-file
           :ownership 'owned
           :state 'starting
           :failure nil))
         process
         success)
    (unwind-protect
        (progn
          (setq process
                (make-process
                 :name (format "org-files-db-watch-%s" config-name)
                 :command (list program "watch" "--config" config-file)
                 :buffer stdout
                 :stderr stderr
                 :connection-type 'pipe
                 :coding '(utf-8-unix . utf-8-unix)
                 :noquery t
                 :sentinel #'org-files-db-watch--sentinel))
          (setf (org-files-db-watch--entry-process entry) process)
          (process-put process 'org-files-db-watch--entry entry)
          (process-put process 'org-files-db-watch--stdout-buffer stdout)
          (process-put process 'org-files-db-watch--stderr-buffer stderr)
          (process-put process 'org-files-db-watch--expected-stop nil)
          (process-put process 'org-files-db-watch--starting t)
          (let ((started-at (float-time)))
            (while (and (process-live-p process)
                        (not (org-files-db-watch--ready-p stderr))
                        (not (and org-files-db-watch-startup-timeout
                                  (>= (- (float-time) started-at)
                                      org-files-db-watch-startup-timeout))))
              (accept-process-output nil 0.05))
            (accept-process-output nil 0)
            (cond
             ((org-files-db-watch--ready-p stderr)
              (setf (org-files-db-watch--entry-state entry) 'ready)
              (process-put process 'org-files-db-watch--starting nil)
              (setq success t)
              entry)
             ((and org-files-db-watch-startup-timeout
                   (>= (- (float-time) started-at)
                       org-files-db-watch-startup-timeout))
              (signal 'org-files-db-error
                      (list
                       (format
                        "Watcher for configuration `%s' did not report watcher ready within %s seconds%s"
                        config-name
                        org-files-db-watch-startup-timeout
                        (let ((diagnostic
                               (string-trim
                                (or (org-files-db-watch--buffer-string stderr) ""))))
                          (if (string-empty-p diagnostic)
                              ""
                            (format ": %s" diagnostic)))))))
             (t
              (let ((status (process-exit-status process))
                    (diagnostic (org-files-db-watch--buffer-string stderr)))
                (if (zerop status)
                    (signal 'org-files-db-error
                            (list
                             (format
                              "Watcher for configuration `%s' exited before reporting watcher ready"
                              config-name)))
                  (org-files-db-process--signal-cli-error status diagnostic)))))))
      (unless success
        (when (and process (process-live-p process))
          (process-put process 'org-files-db-watch--expected-stop t)
          (condition-case nil
              (interrupt-process process)
            (error (delete-process process)))
          (while (process-live-p process)
            (accept-process-output process 0.05)))
        (when process
          (org-files-db-watch--cleanup-process-buffers process))
        (when (buffer-live-p stdout)
          (kill-buffer stdout))
        (when (buffer-live-p stderr)
          (kill-buffer stderr))))))

(defun org-files-db-watch--external-entry (config-name config-file)
  "Return an external watcher entry for CONFIG-NAME and CONFIG-FILE."
  (org-files-db-watch--entry-create
   :config config-name
   :config-file config-file
   :ownership 'external
   :state 'ready
   :process nil
   :failure nil))

(defun org-files-db-watch--stop-owned-entry (entry)
  "Stop the owned watcher represented by ENTRY."
  (when (eq (org-files-db-watch--entry-ownership entry) 'owned)
    (let ((process (org-files-db-watch--entry-process entry)))
      (when process
        (process-put process 'org-files-db-watch--expected-stop t)
        (when (process-live-p process)
          (setf (org-files-db-watch--entry-state entry) 'stopping)
          (condition-case err
              (interrupt-process process)
            (error
             (display-warning
              'org-files-db
              (format "Failed to stop watcher for configuration `%s' gracefully: %s"
                      (org-files-db-watch--entry-config entry)
                      (error-message-string err))
              :warning)
             (delete-process process)))
          (while (process-live-p process)
            (accept-process-output process 0.05)))
        (setf (org-files-db-watch--entry-state entry) 'stopped)
        (org-files-db-watch--cleanup-process-buffers process)))))

(defun org-files-db-watch--stop-owned-entries (entries)
  "Stop every Emacs-owned watcher in ENTRIES."
  (dolist (entry entries)
    (when (eq (org-files-db-watch--entry-ownership entry) 'owned)
      (condition-case err
          (org-files-db-watch--stop-owned-entry entry)
        (error
         (display-warning
          'org-files-db
          (format "Failed to stop watcher for configuration `%s': %s"
                  (org-files-db-watch--entry-config entry)
                  (error-message-string err))
          :warning))))))

(defun org-files-db-watch--activate ()
  "Activate watchers for the current configuration snapshot."
  (let ((targets (org-files-db-watch--configured-targets))
        entries
        started
        success)
    (unwind-protect
        (progn
          (dolist (target targets)
            (let ((name (car target))
                  (file (cdr target)))
              (if (org-files-db-watch--probe-active-p file)
                  (push (org-files-db-watch--external-entry name file) entries)
                (let ((entry
                       (org-files-db-watch--start-owned-watcher name file)))
                  (push entry entries)
                  (push entry started)))))
          (setq entries (nreverse entries)
                org-files-db-watch--activation entries
                success t)
          entries)
      (unless success
        (org-files-db-watch--stop-owned-entries started)
        (setq org-files-db-watch--activation nil)))))

(defun org-files-db-watch--deactivate ()
  "Stop watchers owned by the current activation snapshot."
  (let ((snapshot org-files-db-watch--activation))
    (org-files-db-watch--stop-owned-entries snapshot)
    (setq org-files-db-watch--activation nil)))

(defun org-files-db-watch--cleanup-at-exit ()
  "Run package exit cleanup and then stop Emacs-owned watchers."
  (unwind-protect
      (condition-case err
          (run-hooks 'org-files-db-watch--before-exit-hook)
        (error
         (display-warning
          'org-files-db
          (format "Org-files-db exit cleanup failed before watcher shutdown: %s"
                  (error-message-string err))
          :warning)))
    (org-files-db-watch--deactivate)
    (setq org-files-db-watch-mode nil)))

;;;###autoload
(define-minor-mode org-files-db-watch-mode
  "Keep configured orgfdb databases covered by active watchers."
  :global t
  :group 'org-files-db
  :lighter nil
  (if org-files-db-watch-mode
      (unless org-files-db-watch--activation
        (let (success)
          (unwind-protect
              (progn
                (org-files-db-watch--activate)
                (setq success t))
            (unless success
              (setq org-files-db-watch-mode nil)))))
    (if (bound-and-true-p org-files-db-cache-mode)
        (progn
          (setq org-files-db-watch-mode t)
          (user-error
           "Disable org-files-db-cache-mode before stopping org-files-db-watch-mode"))
      (org-files-db-watch--deactivate))))

;;;###autoload
(defun org-files-db-watch-start ()
  "Enable `org-files-db-watch-mode'."
  (interactive)
  (org-files-db-watch-mode 1))

;;;###autoload
(defun org-files-db-watch-stop ()
  "Disable `org-files-db-watch-mode'."
  (interactive)
  (org-files-db-watch-mode -1))

(add-hook 'kill-emacs-hook #'org-files-db-watch--cleanup-at-exit)

(provide 'org-files-db-watch)

;;; org-files-db-watch.el ends here
