EASK ?= eask
EMACS ?= emacs

ELISP_FILES := $(shell find lisp tests -type f -name '*.el' | sort)

.DEFAULT_GOAL := help

.PHONY: help all setup ci compile fmt lint test package install clean emacs

help:
	@printf '%s\n' \
		'make setup    Install development dependencies' \
		'make ci       Compile, lint and test' \
		'make compile  Byte-compile the package' \
		'make fmt      Format all Emacs Lisp files' \
		'make lint     Run package and source linters' \
		'make test     Run Buttercup tests' \
		'make package  Build the package archive' \
		'make install  Install the package' \
		'make clean    Remove generated files' \
		'make emacs    Start Emacs in the Eask environment'

all: ci

setup:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> SETUP'
	$(EASK) install-deps --dev

ci: clean setup package install compile lint test

compile:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> COMPILE'
	$(EASK) recompile

fmt:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> FORMAT'
	@for file in $(ELISP_FILES); do \
		printf '%s\n' "Formatting $$file"; \
		$(EMACS) --batch -Q "$$file" \
			--eval '(progn (emacs-lisp-mode) (indent-region (point-min) (point-max)) (save-buffer))' \
			>/dev/null; \
	done

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
