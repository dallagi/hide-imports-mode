# Makefile for hide-imports-mode

EMACS ?= emacs
PACKAGE_NAME = hide-imports-mode

.PHONY: test clean byte-compile lint

# Run tests
test:
	$(EMACS) --batch --script run-tests.el

# Run tests with verbose output
test-verbose:
	$(EMACS) --batch --eval "(progn (add-to-list 'load-path default-directory) (require 'ert) (require 'hide-imports-mode) (require 'test-hide-imports-mode) (ert-run-tests-batch 'hide-imports-))"

# Byte compile the package
byte-compile:
	$(EMACS) --batch --eval "(progn (add-to-list 'load-path default-directory) (byte-compile-file \"$(PACKAGE_NAME).el\"))"

# Clean compiled files
clean:
	rm -f *.elc

# Check for common issues
lint:
	$(EMACS) --batch 
		--eval "(require 'package)" 
		--eval "(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))" 
		--eval "(package-initialize)" 
		--eval "(unless (package-installed-p 'package-lint) (package-refresh-contents) (package-install 'package-lint))" 
		--eval "(require 'package-lint)" 
		--eval "(package-lint-batch-and-exit)" 
		$(PACKAGE_NAME).el

# Install package locally for testing
install:
	$(EMACS) --batch --eval "(progn (require 'package) (package-initialize) (package-install-file \"$(PACKAGE_NAME).el\"))"

# Run all checks
check: byte-compile test lint

help:
	@echo "Available targets:"
	@echo "  test         - Run all tests"
	@echo "  test-verbose - Run tests with verbose output"
	@echo "  byte-compile - Byte compile the package"
	@echo "  lint         - Run checkdoc linting"
	@echo "  clean        - Remove compiled files"
	@echo "  install      - Install package locally"
	@echo "  check        - Run all checks (compile, test, lint)"
	@echo "  help         - Show this help message"