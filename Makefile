EASK ?= eask

.DEFAULT_GOAL := help

.PHONY: help all setup ci compile lint test package clean emacs

help:
	@printf '%s\n' \
		'make setup    Install development dependencies' \
		'make ci       Compile, lint and test' \
		'make compile  Byte-compile the package' \
		'make lint     Run package and source linters' \
		'make test     Run Buttercup tests' \
		'make package  Build the package archive' \
		'make clean    Remove generated files' \
		'make emacs    Start Emacs in the Eask environment'

all: ci

setup:
	$(EASK) install-deps --dev

ci: setup compile lint test

compile:
	$(EASK) recompile

lint:
	$(EASK) clean autoloads --verbose 0
	$(EASK) lint package --verbose 0
	$(EASK) lint checkdoc --verbose 0
	$(EASK) lint indent --verbose 0
	$(EASK) lint regexps --verbose 0

test:
	$(EASK) test buttercup

package:
	$(EASK) package

clean:
	$(EASK) clean all

emacs: setup
	$(EASK) emacs
