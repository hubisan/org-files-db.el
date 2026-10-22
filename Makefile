EASK ?= eask
EMACS ?= emacs

ELISP_FILES := $(shell find lisp tests -type f -name '*.el' | sort)

.DEFAULT_GOAL := help

.PHONY: help all setup ci check-offline compile fmt lint test bench package install clean emacs

help:
	@printf '%s\n' \
		'make setup    Install development dependencies' \
		'make ci       Compile, lint and test' \
		'make check-offline  Same checks without Eask (dependencies from GitHub)' \
		'make compile  Byte-compile the package' \
		'make fmt      Format all Emacs Lisp files' \
		'make lint     Run package and source linters' \
		'make test     Run Buttercup tests' \
		'make bench    Run the on-demand benchmark (needs orgfdb)' \
		'make package  Build the package archive' \
		'make install  Install the package' \
		'make clean    Remove generated files' \
		'make emacs    Start Emacs in the Eask environment'

all: ci

setup:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> SETUP'
	$(EASK) install-deps --dev

ci: clean setup package install compile lint test

check-offline:
	EMACS=$(EMACS) scripts/check-offline.sh

compile:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> COMPILE'
	$(EASK) recompile

fmt:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> FORMAT'
	$(EASK) emacs --batch -L lisp -L tests -l scripts/format.el $(ELISP_FILES)

lint:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> LINT'
	$(EASK) clean autoloads --verbose 0
	@printf '\e[1;34m%-10s\e[0m\n\n' '>>> package-lint'
	$(EASK) lint package --verbose 0
	@printf '\e[1;34m%-10s\e[0m\n\n' '>>> checkdoc'
	$(EASK) lint checkdoc --verbose 0
	@printf '\e[1;34m%-10s\e[0m\n\n' '>>> indent-lint'
	$(EASK) lint indent --verbose 0
	@printf '\e[1;34m%-10s\e[0m\n\n' '>>> relint'
	$(EASK) lint regexps --verbose 0

install:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> INSTALL'
	@$(EASK) install

test:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> TEST'
	$(EASK) test buttercup

bench:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> BENCH'
	dir=$$(mktemp -d) && trap 'rm -rf "$$dir"' EXIT && \
	cp lisp/*.el "$$dir" && \
	$(EMACS) --batch -Q -L "$$dir" -f batch-byte-compile "$$dir"/*.el && \
	$(EMACS) --batch -Q -L "$$dir" -l bench/org-files-db-bench.el \
		--eval '(setq gc-cons-percentage 0.1)' \
		--eval '(org-files-db-bench-run)'

package:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> PACKAGING'
	$(EASK) package

clean:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> CLEAN ALL'
	$(EASK) clean all

emacs:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> RUN EMACS'
	$(EASK) install
	$(EASK) emacs &
