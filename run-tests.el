#!/usr/bin/env emacs --script
;;; run-tests.el --- Test runner for hide-imports-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Test runner script for hide-imports-mode.
;; Usage: emacs --script run-tests.el

;;; Code:

(add-to-list 'load-path default-directory)

(require 'ert)
(require 'hide-imports-mode)
(require 'test-hide-imports-mode)

(defun run-hide-imports-tests ()
  "Run all hide-imports-mode tests."
  (interactive)
  (ert-run-tests-batch-and-exit "hide-imports-"))

(run-hide-imports-tests)

;;; run-tests.el ends here