;;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(eval-when-compile
  (defvar sentence-end-double-space)
  (defvar checkdoc-verb-check-experimental-flag))

(package "org-files-db"
         "0.1.0"
         "Emacs interface for org-files-db")

(website-url "https://github.com/hubisan/org-files-db.el")
(keywords "outlines" "tools" "convenience")
(license "GPLv3")

(package-file "lisp/org-files-db.el")
(files "lisp/*.el")
(load-paths "./lisp" "./tests")

(source "gnu")
(source "nongnu")
(source "melpa")

(depends-on "emacs" "29.4")

(development
 (depends-on "buttercup")
 (depends-on "consult")
 (depends-on "embark")
 (depends-on "embark")
 (depends-on "package-lint")
 (depends-on "relint"))

(setq sentence-end-double-space nil)
(setq checkdoc-verb-check-experimental-flag nil)
(setq enable-local-variables :safe)
