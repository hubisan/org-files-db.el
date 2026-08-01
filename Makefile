EASK ?= eask

.DEFAULT_GOAL := help

.PHONY: help all setup ci compile lint test package install clean emacs

help:
	@printf '%s\n' \
		'make setup    Install development dependencies' \
		'make ci       Compile, lint and test' \
		'make compile  Byte-compile the package' \
		'make lint     Run package and source linters' \
		'make test     Run Buttercup tests' \
		'make package  Build the package archive' \
		'make install  Install the pacakge' \
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

lint:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> LINT'
	$(EASK) clean autoloads --verbose 0
	@printf '\e[1;34m%-10s\e[0m\n\n' '>>> package-lint'
	$(EASK) lint package --verbose 0
	@printf '\e[1;34m%-10s\e[0m\n\n' '>>> checkdoc'
	$(EASK) lint checkdoc --verbose 0
	@printf '\e[1;34m%-10s\e[0m\n' '>>> indent-lint'
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
