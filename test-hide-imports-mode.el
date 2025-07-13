;;; test-hide-imports-mode.el --- Tests for hide-imports-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Test Suite
;; Keywords: test

;;; Commentary:

;; Unit tests for hide-imports-mode using ERT (Emacs Lisp Regression Testing).

;;; Code:

(require 'ert)
(require 'hide-imports-mode)

(defmacro hide-imports-test-with-temp-buffer (content &rest body)
  "Create a temporary buffer with CONTENT and execute BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (let ((inhibit-message t))  ; Suppress python-indent-offset messages during tests
       (python-mode)
       (goto-char (point-min))
       ,@body)))

(defmacro hide-imports-test-with-treesit-buffer (content &rest body)
  "Create a temporary buffer with CONTENT, enable treesit if available, and execute BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (set-window-buffer (selected-window) (current-buffer))
     (insert ,content)
     (let ((inhibit-message t))  ; Suppress python-indent-offset messages during tests
       (if (and (treesit-available-p) (treesit-language-available-p 'python))
           (progn
             (python-ts-mode)
             ;; Force tree-sitter parsing
             (treesit-parser-create 'python))
         (python-mode))
       (goto-char (point-min))
       ,@body)))

(defmacro hide-imports-test-with-min-rows (min-rows &rest body)
  "Execute BODY with hide-imports-minimum-rows temporarily set to MIN-ROWS."
  (declare (indent 1))
  `(let ((original-min-rows hide-imports-minimum-rows))
     (unwind-protect
         (progn
           (setq hide-imports-minimum-rows ,min-rows)
           ,@body)
       (setq hide-imports-minimum-rows original-min-rows))))

;;; Basic Mode Tests

(ert-deftest hide-imports-mode-activation ()
  "Test basic mode activation and deactivation."
  (hide-imports-test-with-temp-buffer "import os\n\nprint('hello')"
    (should-not hide-imports-mode)
    (hide-imports-mode 1)
    (should hide-imports-mode)
    (hide-imports-mode -1)
    (should-not hide-imports-mode)))

(ert-deftest hide-imports-mode-python-mode-check ()
  "Test that mode only works in Python buffers."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "import os\n\nprint('hello')")
    (hide-imports-mode 1)
    (should-not (hide-imports--supported-mode-p))
    (should-not hide-imports--overlays)))

;;; Import Detection Tests

(ert-deftest hide-imports-python-mode-detection ()
  "Test Python mode detection."
  (hide-imports-test-with-temp-buffer "import os"
    (should (or (eq major-mode 'python-mode) (eq major-mode 'python-ts-mode)))
    (when (and (treesit-available-p) (treesit-language-available-p 'python))
      (should (hide-imports--supported-mode-p)))))

(ert-deftest hide-imports-import-region-simple ()
  "Test detection of simple import statements."
  (hide-imports-test-with-min-rows 1
    (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
      (when (hide-imports--supported-mode-p)
        (let ((region (hide-imports--get-imports-region)))
          (should region)
          (should (= (car region) 1))
          (should (> (cdr region) 1)))))))

(ert-deftest hide-imports-import-region-with-from ()
  "Test detection of 'from...import' statements."
  (hide-imports-test-with-min-rows 1
    (hide-imports-test-with-treesit-buffer "from os import path\nfrom sys import argv\n\nprint('hello')"
      (when (hide-imports--supported-mode-p)
        (let ((region (hide-imports--get-imports-region)))
          (should region)
          (should (= (car region) 1)))))))

(ert-deftest hide-imports-import-region-with-comments ()
  "Test detection of imports with comments."
  (hide-imports-test-with-treesit-buffer "import os\n# This is a comment\nimport sys\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      (let ((region (hide-imports--get-imports-region)))
        (should region)
        (should (= (car region) 1))
        ;; Should include the comment in the region
        (let ((region-text (buffer-substring (car region) (cdr region))))
          (should (string-match-p "# This is a comment" region-text)))))))

(ert-deftest hide-imports-no-imports ()
  "Test behavior when no imports are present."
  (hide-imports-test-with-treesit-buffer "print('hello world')\n\ndef main():\n    pass"
    (when (hide-imports--supported-mode-p)
      (let ((region (hide-imports--get-imports-region)))
        (should-not region)))))

;;; Overlay Tests

(ert-deftest hide-imports-overlay-creation ()
  "Test overlay creation and properties."
  (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      (let ((region (hide-imports--get-imports-region)))
        (when region
          (let ((overlay (hide-imports--create-overlay (car region) (cdr region))))
            (should overlay)
            (should (overlay-get overlay 'display))
            (should (overlay-get overlay 'hide-imports))
            (should (string-match-p "\\[.*hidden import" (overlay-get overlay 'display)))
            (should (member overlay hide-imports--overlays))))))))

(ert-deftest hide-imports-overlay-removal ()
  "Test overlay removal."
  (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      (let ((region (hide-imports--get-imports-region)))
        (when region
          (hide-imports--create-overlay (car region) (cdr region))
          (should hide-imports--overlays)
          (hide-imports--remove-overlays)
          (should-not hide-imports--overlays))))))

(ert-deftest hide-imports-hide-show-cycle ()
  "Test hiding and showing imports."
  (hide-imports-test-with-min-rows 1
    (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
      (when (hide-imports--supported-mode-p)
        ;; Move cursor away from imports region to ensure overlays are created
        (goto-char (point-max))
        (hide-imports--hide-imports)
        ;; With window-local overlays, we need to check for window-specific overlays
        (should (hide-imports--window-has-overlays-p (selected-window)))
        (hide-imports--show-imports)
        (should-not hide-imports--overlays)))))

;;; Cursor Movement Tests

(ert-deftest hide-imports-cursor-detection ()
  "Test cursor position detection in imports region."
  (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      (let ((region (hide-imports--get-imports-region)))
        (when region
          (setq hide-imports--imports-regions (list region))
          (goto-char (car region))
          (should (hide-imports--cursor-in-imports-p))
          (goto-char (cdr region))
          (should (hide-imports--cursor-in-imports-p))
          (goto-char (1+ (cdr region)))
          (should-not (hide-imports--cursor-in-imports-p)))))))

(ert-deftest hide-imports-post-command-hook ()
  "Test post-command hook behavior."
  (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      (hide-imports-mode 1)
      (let ((region (hide-imports--get-imports-region)))
        (when region
          (setq hide-imports--imports-regions (list region))

          ;; Move cursor to imports region
          (goto-char (car region))
          (hide-imports--post-command-hook)
          (should-not hide-imports--overlays)

          ;; Move cursor away from imports
          (goto-char (cdr region))
          (forward-char 10)
          (hide-imports--post-command-hook)
          (should hide-imports--overlays))))))

;;; Toggle Tests

(ert-deftest hide-imports-toggle-function ()
  "Test manual toggle functionality."
  (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      (hide-imports-mode 1)
      (let ((region (hide-imports--get-imports-region)))
        (when region
          ;; Move cursor away from imports region
          (goto-char (1+ (cdr region)))
          (hide-imports--post-command-hook)
          (should (hide-imports--window-has-overlays-p (selected-window)))
          (hide-imports-toggle)
          (should-not (hide-imports--window-has-overlays-p (selected-window)))
          (hide-imports-toggle)
          ;; After toggle, imports should be hidden again
          (should (hide-imports--window-has-overlays-p (selected-window))))))))

;;; Edge Cases

(ert-deftest hide-imports-empty-buffer ()
  "Test behavior with empty buffer."
  (let ((original-overlays hide-imports--overlays))
    (unwind-protect
        (progn
          (setq hide-imports--overlays nil)  ; Start with clean state
          (hide-imports-test-with-treesit-buffer ""
            (when (hide-imports--supported-mode-p)
              (should-not (hide-imports--get-imports-region))
              (hide-imports--hide-imports)
              (should-not hide-imports--overlays))))
      (setq hide-imports--overlays original-overlays))))

(ert-deftest hide-imports-only-comments ()
  "Test behavior with only comments at top."
  (hide-imports-test-with-treesit-buffer "# This is a comment\n# Another comment\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      (should-not (hide-imports--get-imports-region)))))

(ert-deftest hide-imports-mixed-content ()
  "Test behavior with mixed content before imports."
  (hide-imports-test-with-treesit-buffer "#!/usr/bin/env python3\n\"\"\"Module docstring\"\"\"\n\nimport os\nimport sys\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      (let ((region (hide-imports--get-imports-region)))
        (when region
          ;; Should start from the first import, not the shebang or docstring
          (let ((region-text (buffer-substring (car region) (cdr region))))
            (should (string-match-p "^import os" region-text))))))))

(ert-deftest hide-imports-customization ()
  "Test customization variables."
  (should (functionp hide-imports-overlay-text-function))
  (let ((original-func hide-imports-overlay-text-function))
    (setq hide-imports-overlay-text-function (lambda (start end) "Custom text"))
    (should (string= (funcall hide-imports-overlay-text-function 1 10) "Custom text"))
    (setq hide-imports-overlay-text-function original-func)))

;;; Cleanup

(ert-deftest hide-imports-cleanup-on-disable ()
  "Test proper cleanup when mode is disabled."
  (hide-imports-test-with-min-rows 1
    (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
      (when (hide-imports--supported-mode-p)
        ;; Move cursor away from imports region
        (goto-char (point-max))
        (hide-imports-mode 1)
        ;; Need to trigger post-command hook to create overlays
        (hide-imports--post-command-hook)
        (should hide-imports--overlays)
        (should hide-imports--imports-regions)
        (hide-imports-mode -1)
        (should-not hide-imports--overlays)
        (should-not hide-imports--imports-regions)
        (should-not hide-imports--cursor-in-imports)))))

;;; Global Mode Tests

(ert-deftest hide-imports-global-mode-activation ()
  "Test hide-imports-global-mode activation and deactivation."
  (let ((original-global-modes hide-imports-global-modes))
    (unwind-protect
        (progn
          (setq hide-imports-global-modes '(python-mode python-ts-mode))
          (hide-imports-global-mode 1)
          (should hide-imports-global-mode)
          (hide-imports-global-mode -1)
          (should-not hide-imports-global-mode))
      (setq hide-imports-global-modes original-global-modes))))

(ert-deftest hide-imports-global-mode-python-auto-enable ()
  "Test that global mode automatically enables hide-imports-mode in Python buffers."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'python)))
  (let ((original-global-modes hide-imports-global-modes))
    (unwind-protect
        (progn
          (setq hide-imports-global-modes '(python-mode python-ts-mode))
          (hide-imports-global-mode 1)
          (hide-imports-test-with-min-rows 1
            (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
              ;; Move cursor away from imports region
              (goto-char (point-max))
              ;; Manually trigger the function since the test doesn't go through normal buffer creation
              (hide-imports--maybe-turn-on)
              (should hide-imports-mode)
              ;; Need to trigger post-command hook to create overlays
              (hide-imports--post-command-hook)
              (should hide-imports--overlays))))
      (hide-imports-global-mode -1)
      (setq hide-imports-global-modes original-global-modes))))

(ert-deftest hide-imports-global-mode-rust-auto-enable ()
  "Test that global mode automatically enables hide-imports-mode in Rust buffers."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'rust)))
  (let ((original-global-modes hide-imports-global-modes))
    (unwind-protect
        (progn
          (setq hide-imports-global-modes '(rust-ts-mode rust-mode))
          (hide-imports-global-mode 1)
          (hide-imports-test-with-min-rows 1
            (hide-imports-test-with-rust-buffer "use std::collections::HashMap;\nuse std::fs;\n\nfn main() {\n    println!(\"Hello\");\n}"
              ;; Move cursor away from imports region
              (goto-char (point-max))
              ;; Manually trigger the function since the test doesn't go through normal buffer creation
              (hide-imports--maybe-turn-on)
              (should hide-imports-mode)
              ;; Need to trigger post-command hook to create overlays
              (hide-imports--post-command-hook)
              (should hide-imports--overlays))))
      (hide-imports-global-mode -1)
      (setq hide-imports-global-modes original-global-modes))))

(ert-deftest hide-imports-global-mode-unsupported-mode ()
  "Test that global mode doesn't activate in unsupported modes."
  (let ((original-global-modes hide-imports-global-modes))
    (unwind-protect
        (progn
          (setq hide-imports-global-modes '(python-mode python-ts-mode))
          (hide-imports-global-mode 1)
          (with-temp-buffer
            (text-mode)
            (insert "Some text content")
            (should-not hide-imports-mode)))
      (hide-imports-global-mode -1)
      (setq hide-imports-global-modes original-global-modes))))

(ert-deftest hide-imports-global-mode-excluded-mode ()
  "Test that global mode doesn't activate in modes not in the list."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'python)))
  (let ((original-global-modes hide-imports-global-modes))
    (unwind-protect
        (progn
          (setq hide-imports-global-modes '(rust-ts-mode))  ; Only Rust modes
          (hide-imports-global-mode 1)
          (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
            (should-not hide-imports-mode)))
      (hide-imports-global-mode -1)
      (setq hide-imports-global-modes original-global-modes))))

(ert-deftest hide-imports-global-mode-customization ()
  "Test customization of hide-imports-global-modes."
  (let ((original-global-modes hide-imports-global-modes))
    (unwind-protect
        (progn
          (should (listp hide-imports-global-modes))
          (should (memq 'python-mode hide-imports-global-modes))
          (should (memq 'rust-ts-mode hide-imports-global-modes))
          (setq hide-imports-global-modes '(python-mode))
          (should (equal hide-imports-global-modes '(python-mode))))
      (setq hide-imports-global-modes original-global-modes))))

(ert-deftest hide-imports-maybe-turn-on-function ()
  "Test the hide-imports--maybe-turn-on function directly."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'python)))
  (let ((original-global-modes hide-imports-global-modes))
    (unwind-protect
        (progn
          (setq hide-imports-global-modes '(python-mode python-ts-mode))
          (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
            (should-not hide-imports-mode)
            (hide-imports--maybe-turn-on)
            (should hide-imports-mode)))
      (setq hide-imports-global-modes original-global-modes))))

(ert-deftest hide-imports-maybe-turn-on-already-active ()
  "Test that hide-imports--maybe-turn-on doesn't activate if already active."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'python)))
  (let ((original-global-modes hide-imports-global-modes))
    (unwind-protect
        (progn
          (setq hide-imports-global-modes '(python-mode python-ts-mode))
          (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
            (hide-imports-mode 1)
            (should hide-imports-mode)
            ;; Should not change anything since mode is already active
            (hide-imports--maybe-turn-on)
            (should hide-imports-mode)))
      (setq hide-imports-global-modes original-global-modes))))

;;; Multi-Window Tests

(ert-deftest hide-imports-multi-window-cursor-tracking ()
  "Test cursor tracking across multiple windows showing the same buffer."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'python)))
  (hide-imports-test-with-treesit-buffer "import os
import sys

print('hello')"
    (when (hide-imports--supported-mode-p)
      (let ((region (hide-imports--get-imports-region)))
        (when region
          (setq hide-imports--imports-regions (list region))
          (let ((window1 (selected-window)))
            (goto-char (car region))
            (set-window-point window1 (point))
            (should (hide-imports--cursor-in-imports-p window1))
            (goto-char (cdr region))
            (set-window-point window1 (point))
            (should (hide-imports--cursor-in-imports-p window1))
            (goto-char (1+ (cdr region)))
            (set-window-point window1 (point))
            (should-not (hide-imports--cursor-in-imports-p window1))
            (hide-imports--set-window-state window1 t)
            (should (hide-imports--get-window-state window1))
            (hide-imports--set-window-state window1 nil)
            (should-not (hide-imports--get-window-state window1))))))))

(ert-deftest hide-imports-multi-window-any-cursor-check ()
  "Test the any-window-in-imports-p function."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'python)))
  (hide-imports-test-with-treesit-buffer "import os\nsys\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      (hide-imports-mode 1)
      (let ((region (hide-imports--get-imports-region)))
        (when region
          (setq hide-imports--imports-regions (list region))
          (goto-char (car region))
          (hide-imports--post-command-hook)
          (should (hide-imports--any-window-in-imports-p))
          (goto-char (point-max))
          (hide-imports--post-command-hook)
          (should-not (hide-imports--any-window-in-imports-p)))))))

(ert-deftest hide-imports-window-state-cleanup ()
  "Test cleanup of window states for dead windows."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'python)))
  (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      (let ((region (hide-imports--get-imports-region)))
        (when region
          (setq hide-imports--imports-regions (list region))

          ;; Create a fake window entry
          (let ((fake-window (make-symbol "fake-window")))
            (hide-imports--set-window-state fake-window t)
            (should (hide-imports--get-window-state fake-window))

            ;; Cleanup should remove the fake window
            (hide-imports--cleanup-window-states)
            (should-not (alist-get fake-window hide-imports--window-states))))))))

(ert-deftest hide-imports-multi-window-overlay-management ()
  "Test that overlays are managed correctly across multiple windows."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'python)))
  (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      (let ((region (hide-imports--get-imports-region)))
        (when region
          (setq hide-imports--imports-regions (list region))
          (hide-imports-mode 1)

          ;; Start with cursor outside imports (should hide)
          (goto-char (+ (cdr region) 10))
          (hide-imports--post-command-hook)
          (should (hide-imports--window-has-overlays-p (selected-window)))

          ;; Move cursor to imports (should show for this window)
          (goto-char (car region))
          (hide-imports--post-command-hook)
          (should-not (hide-imports--window-has-overlays-p (selected-window)))

          ;; Move cursor back outside (should hide again)
          (goto-char (+ (cdr region) 10))
          (hide-imports--post-command-hook)
          (should (hide-imports--window-has-overlays-p (selected-window))))))))

(ert-deftest hide-imports-buffer-local-window-cleanup ()
  "Test that window states are cleaned up when mode is disabled."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'python)))
  (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      (hide-imports-mode 1)
      (let ((window1 (selected-window))
            (test-buffer (current-buffer)))
        ;; Set some window state
        (hide-imports--set-window-state window1 t)
        (should (hide-imports--get-window-state window1))

        ;; Disable mode should clean up window states for this buffer
        (hide-imports-mode -1)

        ;; The window state should be cleaned up for this buffer
        ;; The cleanup removes entries where window shows current buffer
        ;; Since window1 is still showing test-buffer, it should be removed
        (should-not (seq-find (lambda (entry)
                                (and (eq (car entry) window1)
                                     (eq (window-buffer (car entry)) test-buffer)))
                              hide-imports--window-states))))))

;;; Rust Language Tests

(defmacro hide-imports-test-with-rust-buffer (content &rest body)
  "Create a temporary buffer with CONTENT for Rust testing and execute BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (let ((inhibit-message t))
       (if (and (treesit-available-p) (treesit-language-available-p 'rust))
           (progn
             (setq major-mode 'rust-ts-mode)
             (treesit-parser-create 'rust))
         (setq major-mode 'rust-ts-mode))
       (goto-char (point-min))
       ,@body)))

(ert-deftest hide-imports-rust-basic-imports ()
  "Test basic Rust import detection."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'rust)))
  (hide-imports-test-with-rust-buffer "use std::collections::HashMap;\nuse std::fs;\nextern crate serde;\n\nfn main() {\n    println!(\"Hello\");\n}"
    (let ((region (hide-imports--get-imports-region)))
      (should region)
      (let ((imports-text (buffer-substring (car region) (cdr region))))
        (should (string-match-p "use std::collections::HashMap" imports-text))
        (should (string-match-p "use std::fs" imports-text))
        (should (string-match-p "extern crate serde" imports-text))
        (should-not (string-match-p "fn main" imports-text))))))

(ert-deftest hide-imports-rust-complex-use-declarations ()
  "Test Rust imports with complex use declarations and paths."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'rust)))
  (hide-imports-test-with-rust-buffer "use std::collections::{HashMap, HashSet};\nuse std::io::{self, Read, Write};\nuse crate::module::*;\nuse super::parent_module::Item;\n\nfn main() {\n    println!(\"Hello\");\n}"
    (let ((region (hide-imports--get-imports-region)))
      (should region)
      (let ((imports-text (buffer-substring (car region) (cdr region))))
        (should (string-match-p "HashMap, HashSet" imports-text))
        (should (string-match-p "self, Read, Write" imports-text))
        (should (string-match-p "crate::module::\\*" imports-text))
        (should (string-match-p "super::parent_module::Item" imports-text))
        (should-not (string-match-p "fn main" imports-text))))))

(ert-deftest hide-imports-rust-mixed-import-types ()
  "Test Rust imports mixing use declarations and extern crate."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'rust)))
  (hide-imports-test-with-rust-buffer "extern crate serde;\nextern crate tokio;\nuse serde::{Serialize, Deserialize};\nuse tokio::runtime::Runtime;\n\nfn main() {\n    println!(\"Hello\");\n}"
    (let ((region (hide-imports--get-imports-region)))
      (should region)
      (let ((imports-text (buffer-substring (car region) (cdr region))))
        (should (string-match-p "extern crate serde" imports-text))
        (should (string-match-p "extern crate tokio" imports-text))
        (should (string-match-p "use serde::{Serialize, Deserialize}" imports-text))
        (should (string-match-p "use tokio::runtime::Runtime" imports-text))
        (should-not (string-match-p "fn main" imports-text))))))

(ert-deftest hide-imports-rust-with-comments ()
  "Test Rust imports with comments within and after imports."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'rust)))
  (hide-imports-test-with-rust-buffer "use std::collections::HashMap;\n// Comment within imports\nuse std::fs;\n\n// Comment after imports\nfn main() {\n    println!(\"Hello\");\n}"
    (let ((region (hide-imports--get-imports-region)))
      (should region)
      (let ((imports-text (buffer-substring (car region) (cdr region))))
        ;; Should include the comment within imports but not after
        (should (string-match-p "Comment within imports" imports-text))
        (should-not (string-match-p "Comment after imports" imports-text))
        (should-not (string-match-p "fn main" imports-text))))))

(ert-deftest hide-imports-rust-comments-between-imports ()
  "Test Rust imports with multiple comments interspersed."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'rust)))
  (hide-imports-test-with-rust-buffer "use std::collections::HashMap;\n// First comment within imports\nuse std::fs;\n// Second comment within imports\nuse std::io;\n\n// Comment after imports - should NOT be hidden\nfn main() {\n    println!(\"Hello\");\n}"
    (let ((region (hide-imports--get-imports-region)))
      (should region)
      (let ((imports-text (buffer-substring (car region) (cdr region))))
        (should (string-match-p "First comment within imports" imports-text))
        (should (string-match-p "Second comment within imports" imports-text))
        (should-not (string-match-p "Comment after imports" imports-text))
        (should-not (string-match-p "fn main" imports-text))))))

(ert-deftest hide-imports-rust-single-use-declaration ()
  "Test Rust with only a single use declaration."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'rust)))
  (hide-imports-test-with-min-rows 1
    (hide-imports-test-with-rust-buffer "use std::collections::HashMap;\n\nfn main() {\n    let map = HashMap::new();\n}"
      (let ((region (hide-imports--get-imports-region)))
        (should region)
        (should (= (car region) 1))
        (let ((imports-text (buffer-substring (car region) (cdr region))))
          (should (string= imports-text "use std::collections::HashMap;")))))))

(ert-deftest hide-imports-rust-extern-crate-only ()
  "Test Rust with only extern crate declarations."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'rust)))
  (hide-imports-test-with-min-rows 1
    (hide-imports-test-with-rust-buffer "extern crate serde;\nextern crate tokio;\n\nfn main() {\n    println!(\"Hello\");\n}"
      (let ((region (hide-imports--get-imports-region)))
        (should region)
        (let ((imports-text (buffer-substring (car region) (cdr region))))
          (should (string-match-p "extern crate serde" imports-text))
          (should (string-match-p "extern crate tokio" imports-text))
          (should-not (string-match-p "fn main" imports-text)))))))

(ert-deftest hide-imports-rust-mode-support ()
  "Test that Rust mode is properly supported."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'rust)))
  (hide-imports-test-with-rust-buffer "use std::collections::HashMap;"
    (should (hide-imports--supported-mode-p))
    (let ((config (hide-imports--get-language-config)))
      (should config)
      (should (eq (car config) 'rust))
      (should (equal (alist-get 'language (cdr config)) 'rust))
      (should (member "use_declaration" (alist-get 'import-types (cdr config))))
      (should (member "extern_crate_declaration" (alist-get 'import-types (cdr config)))))))

(ert-deftest hide-imports-rust-no-imports ()
  "Test Rust behavior when there are no imports."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'rust)))
  (hide-imports-test-with-rust-buffer "fn main() {\n    println!(\"Hello world\");\n}\n\nstruct MyStruct {\n    field: i32,\n}"
    (let ((region (hide-imports--get-imports-region)))
      (should-not region))))

;;; Elixir Language Tests

(defmacro hide-imports-test-with-elixir-buffer (content &rest body)
  "Create a temporary buffer with CONTENT for Elixir testing and execute BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (let ((inhibit-message t))
       (if (and (treesit-available-p) (treesit-language-available-p 'elixir))
           (progn
             (setq major-mode 'elixir-ts-mode)
             (treesit-parser-create 'elixir))
         (setq major-mode 'elixir-ts-mode))
       (goto-char (point-min))
       ,@body)))

(ert-deftest hide-imports-elixir-basic-imports ()
  "Test basic Elixir import detection."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'elixir)))
  (hide-imports-test-with-elixir-buffer "defmodule MyApp.User do\n  use Ecto.Schema\n  alias MyApp.{Repo, Auth}\n  import Ecto.Query\n  require Logger\n\n  def create_user(attrs) do\n    # implementation\n  end\nend"
    (let ((region (hide-imports--get-imports-region)))
      (should region)
      (let ((imports-text (buffer-substring (car region) (cdr region))))
        (should (string-match-p "use Ecto.Schema" imports-text))
        (should (string-match-p "alias MyApp.{Repo, Auth}" imports-text))
        (should (string-match-p "import Ecto.Query" imports-text))
        (should (string-match-p "require Logger" imports-text))
        (should-not (string-match-p "def create_user" imports-text))))))

(ert-deftest hide-imports-elixir-alias-variations ()
  "Test Elixir alias variations."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'elixir)))
  (hide-imports-test-with-elixir-buffer "defmodule MyApp.Controller do\n  alias MyApp.User\n  alias MyApp.Authentication.Guardian, as: Auth\n  alias MyApp.{Post, Comment}\n\n  def index do\n    # implementation\n  end\nend"
    (let ((region (hide-imports--get-imports-region)))
      (should region)
      (let ((imports-text (buffer-substring (car region) (cdr region))))
        (should (string-match-p "alias MyApp.User" imports-text))
        (should (string-match-p "alias MyApp.Authentication.Guardian, as: Auth" imports-text))
        (should (string-match-p "alias MyApp.{Post, Comment}" imports-text))
        (should-not (string-match-p "def index" imports-text))))))

(ert-deftest hide-imports-elixir-import-with-options ()
  "Test Elixir import with options."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'elixir)))
  (hide-imports-test-with-min-rows 1
    (hide-imports-test-with-elixir-buffer "defmodule MyApp.Helper do\n  import Enum, only: [map: 2, filter: 2]\n  import String, except: [split: 2]\n\n  def process_data(data) do\n    # implementation\n  end\nend"
      (let ((region (hide-imports--get-imports-region)))
        (should region)
        (let ((imports-text (buffer-substring (car region) (cdr region))))
          (should (string-match-p "import Enum, only: \\[map: 2, filter: 2\\]" imports-text))
          (should (string-match-p "import String, except: \\[split: 2\\]" imports-text))
          (should-not (string-match-p "def process_data" imports-text)))))))

(ert-deftest hide-imports-elixir-mixed-directives ()
  "Test Elixir mixed import directives."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'elixir)))
  (hide-imports-test-with-elixir-buffer "defmodule MyApp.Worker do\n  use GenServer\n  alias MyApp.{Cache, Database}\n  import MyApp.Helpers\n  require Logger\n\n  def start_link(opts) do\n    GenServer.start_link(__MODULE__, opts)\n  end\nend"
    (let ((region (hide-imports--get-imports-region)))
      (should region)
      (let ((imports-text (buffer-substring (car region) (cdr region))))
        (should (string-match-p "use GenServer" imports-text))
        (should (string-match-p "alias MyApp.{Cache, Database}" imports-text))
        (should (string-match-p "import MyApp.Helpers" imports-text))
        (should (string-match-p "require Logger" imports-text))
        (should-not (string-match-p "def start_link" imports-text))))))

(ert-deftest hide-imports-elixir-with-comments ()
  "Test Elixir imports with comments."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'elixir)))
  (hide-imports-test-with-elixir-buffer "defmodule MyApp.Controller do\n  use Phoenix.Controller\n  # Authentication modules\n  alias MyApp.Auth\n  # Database modules\n  alias MyApp.Repo\n\n  # This comment is after imports\n  def index(conn, _params) do\n    # implementation\n  end\nend"
    (let ((region (hide-imports--get-imports-region)))
      (should region)
      (let ((imports-text (buffer-substring (car region) (cdr region))))
        (should (string-match-p "use Phoenix.Controller" imports-text))
        (should (string-match-p "# Authentication modules" imports-text))
        (should (string-match-p "alias MyApp.Auth" imports-text))
        (should (string-match-p "# Database modules" imports-text))
        (should (string-match-p "alias MyApp.Repo" imports-text))
        (should-not (string-match-p "# This comment is after imports" imports-text))
        (should-not (string-match-p "def index" imports-text))))))

(ert-deftest hide-imports-elixir-no-imports ()
  "Test Elixir behavior when there are no imports."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'elixir)))
  (hide-imports-test-with-elixir-buffer "defmodule MyApp.Utils do\n  def format_string(str) do\n    String.upcase(str)\n  end\n\n  def calculate(x, y) do\n    x + y\n  end\nend"
    (let ((region (hide-imports--get-imports-region)))
      (should-not region))))

(ert-deftest hide-imports-elixir-imports-with-function-between ()
  "Test that Elixir only hides the first block of imports, not imports after functions."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'elixir)))
  (hide-imports-test-with-min-rows 1
    (hide-imports-test-with-elixir-buffer "defmodule MyApp.Controller do\n  use Phoenix.Controller\n  alias MyApp.User\n\n  def index(conn, _params) do\n    # function implementation\n  end\n\n  # This import should NOT be hidden\n  import Ecto.Query\n\n  def show(conn, %{\"id\" => id}) do\n    # another function\n  end\nend"
      (let ((region (hide-imports--get-imports-region)))
        (should region)
        (let ((imports-text (buffer-substring (car region) (cdr region))))
          (should (string-match-p "use Phoenix.Controller" imports-text))
          (should (string-match-p "alias MyApp.User" imports-text))
          (should-not (string-match-p "def index" imports-text))
          (should-not (string-match-p "import Ecto.Query" imports-text))
          (should-not (string-match-p "def show" imports-text)))))))

(ert-deftest hide-imports-minimum-rows-configuration ()
  "Test minimum rows configuration."
  (let ((original-min-rows hide-imports-minimum-rows))
    (unwind-protect
        (progn
          (setq hide-imports-minimum-rows 2)
          (should (= hide-imports-minimum-rows 2))
          (setq hide-imports-minimum-rows 5)
          (should (= hide-imports-minimum-rows 5)))
      (setq hide-imports-minimum-rows original-min-rows))))

(ert-deftest hide-imports-minimum-rows-python ()
  "Test minimum rows functionality with Python imports."
  (let ((original-min-rows hide-imports-minimum-rows))
    (unwind-protect
        (progn
          ;; Test with minimum rows = 3 (default)
          (setq hide-imports-minimum-rows 3)

          ;; Short import block (2 lines) should not be hidden
          (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
            (when (hide-imports--supported-mode-p)
              (let ((region (hide-imports--get-imports-region)))
                (should-not region))))

          ;; Longer import block (3 lines) should be hidden
          (hide-imports-test-with-treesit-buffer "import os\nimport sys\nimport json\n\nprint('hello')"
            (when (hide-imports--supported-mode-p)
              (let ((region (hide-imports--get-imports-region)))
                (should region))))

          ;; Test with minimum rows = 1
          (setq hide-imports-minimum-rows 1)

          ;; Even single import should now be hidden
          (hide-imports-test-with-treesit-buffer "import os\n\nprint('hello')"
            (when (hide-imports--supported-mode-p)
              (let ((region (hide-imports--get-imports-region)))
                (should region)))))
      (setq hide-imports-minimum-rows original-min-rows))))

(ert-deftest hide-imports-minimum-rows-elixir ()
  "Test minimum rows functionality with Elixir imports."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'elixir)))
  (let ((original-min-rows hide-imports-minimum-rows))
    (unwind-protect
        (progn
          ;; Test with minimum rows = 3
          (setq hide-imports-minimum-rows 3)

          ;; Short import block (2 lines) should not be hidden
          (hide-imports-test-with-elixir-buffer "defmodule Test do\n  use GenServer\n  alias MyApp.User\n\n  def start_link do\n    # implementation\n  end\nend"
            (let ((region (hide-imports--get-imports-region)))
              (should-not region)))

          ;; Longer import block (3 lines) should be hidden
          (hide-imports-test-with-elixir-buffer "defmodule Test do\n  use GenServer\n  alias MyApp.User\n  import Ecto.Query\n\n  def start_link do\n    # implementation\n  end\nend"
            (let ((region (hide-imports--get-imports-region)))
              (should region)))

          ;; Test with minimum rows = 1
          (setq hide-imports-minimum-rows 1)

          ;; Even single import should now be hidden
          (hide-imports-test-with-elixir-buffer "defmodule Test do\n  use GenServer\n\n  def start_link do\n    # implementation\n  end\nend"
            (let ((region (hide-imports--get-imports-region)))
              (should region))))
      (setq hide-imports-minimum-rows original-min-rows))))

(ert-deftest hide-imports-count-rows-helper ()
  "Test the row counting helper function."
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3\nline 4")
    (should (= (hide-imports--count-rows 1 7) 1))  ; "line 1" only
    (should (= (hide-imports--count-rows 1 14) 2)) ; "line 1\nline 2"
    (should (= (hide-imports--count-rows 1 21) 3)) ; "line 1\nline 2\nline 3"
    (should (= (hide-imports--count-rows 8 14) 1)) ; "line 2" only
    (should (= (hide-imports--count-rows 8 21) 2)) ; "line 2\nline 3"
    ))

(ert-deftest hide-imports-elixir-mode-support ()
  "Test that Elixir mode is properly supported."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'elixir)))
  (hide-imports-test-with-elixir-buffer "defmodule Test do\n  alias MyApp.User\nend"
    (should (hide-imports--supported-mode-p))
    (let ((config (hide-imports--get-language-config)))
      (should config)
      (should (eq (car config) 'elixir))
      (should (equal (alist-get 'language (cdr config)) 'elixir))
      (should (equal (alist-get 'import-types (cdr config)) '("call"))))))

;;; Multi-block Tests

(ert-deftest hide-imports-hide-all-blocks-python ()
  "Test hiding all import blocks in Python when hide-imports-hide-all-blocks is enabled."
  (hide-imports-test-with-min-rows 1
    (let ((hide-imports-hide-all-blocks t))
      (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\ndef func1():\n    pass\n\nimport json\nimport re\n\ndef func2():\n    pass"
        (when (hide-imports--supported-mode-p)
          (let ((regions (hide-imports--get-imports-regions)))
            (should regions)
            (should (= (length regions) 2))
            ;; First block: import os, import sys
            (let ((first-block-text (buffer-substring (caar regions) (cdar regions))))
              (should (string-match-p "import os" first-block-text))
              (should (string-match-p "import sys" first-block-text)))
            ;; Second block: import json, import re
            (let ((second-block-text (buffer-substring (car (nth 1 regions)) (cdr (nth 1 regions)))))
              (should (string-match-p "import json" second-block-text))
              (should (string-match-p "import re" second-block-text)))))))))

(ert-deftest hide-imports-hide-all-blocks-disabled ()
  "Test that only first block is hidden when hide-imports-hide-all-blocks is disabled."
  (hide-imports-test-with-min-rows 1
    (let ((hide-imports-hide-all-blocks nil))
      (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\ndef func1():\n    pass\n\nimport json\nimport re\n\ndef func2():\n    pass"
        (when (hide-imports--supported-mode-p)
          (let ((regions (hide-imports--get-imports-regions)))
            (should regions)
            (should (= (length regions) 1))
            ;; Should only get first block
            (let ((first-block-text (buffer-substring (caar regions) (cdar regions))))
              (should (string-match-p "import os" first-block-text))
              (should (string-match-p "import sys" first-block-text))
              (should-not (string-match-p "import json" first-block-text)))))))))

(ert-deftest hide-imports-hide-all-blocks-minimum-rows ()
  "Test that multi-block mode respects minimum rows setting."
  (hide-imports-test-with-min-rows 3
    (let ((hide-imports-hide-all-blocks t))
      (hide-imports-test-with-treesit-buffer "import os\n\ndef func1():\n    pass\n\nimport json\nimport re\nimport math\n\ndef func2():\n    pass"
        (when (hide-imports--supported-mode-p)
          (let ((regions (hide-imports--get-imports-regions)))
            ;; First block has only 1 row, second block has 3 rows
            ;; Only second block should be returned
            (should regions)
            (should (= (length regions) 1))
            (let ((block-text (buffer-substring (caar regions) (cdar regions))))
              (should-not (string-match-p "import os" block-text))
              (should (string-match-p "import json" block-text))
              (should (string-match-p "import re" block-text))
              (should (string-match-p "import math" block-text)))))))))

(ert-deftest hide-imports-hide-all-blocks-elixir ()
  "Test hiding all import blocks in Elixir when hide-imports-hide-all-blocks is enabled."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'elixir)))
  (hide-imports-test-with-min-rows 1
    (let ((hide-imports-hide-all-blocks t))
      (hide-imports-test-with-elixir-buffer "defmodule MyModule do\n  import Foo\n  import Bar\n\n  def some_function do\n    :ok\n  end\n\n  import Baz\n  import Qux\n\n  def another_function do\n    :ok\n  end\nend"
        (let ((regions (hide-imports--get-imports-regions)))
          (should regions)
          (should (= (length regions) 2))
          ;; First block: import Foo, import Bar
          (let ((first-block-text (buffer-substring (caar regions) (cdar regions))))
            (should (string-match-p "import Foo" first-block-text))
            (should (string-match-p "import Bar" first-block-text)))
          ;; Second block: import Baz, import Qux
          (let ((second-block-text (buffer-substring (car (nth 1 regions)) (cdr (nth 1 regions)))))
            (should (string-match-p "import Baz" second-block-text))
            (should (string-match-p "import Qux" second-block-text))))))))

(ert-deftest hide-imports-elixir-multiple-import-blocks-updated ()
  "Test that Elixir gets all blocks when hide-all-blocks is enabled, only first when disabled."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'elixir)))
  (hide-imports-test-with-min-rows 1
    ;; Test with hide-all-blocks disabled (should only get first block)
    (let ((hide-imports-hide-all-blocks nil))
      (hide-imports-test-with-elixir-buffer "defmodule MyModule do\n  import Foo\n  import Bar\n\n  def some_function do\n    :ok\n  end\n\n  import Baz\n  import Qux\nend"
        (let ((regions (hide-imports--get-imports-regions)))
          (should regions)
          (should (= (length regions) 1))
          ;; Check that the region doesn't include the later imports (Baz/Qux)
          (let ((region-text (buffer-substring (caar regions) (cdar regions))))
            (should (string-match-p "import Foo" region-text))
            (should (string-match-p "import Bar" region-text))
            (should-not (string-match-p "import Baz" region-text))
            (should-not (string-match-p "import Qux" region-text))))))
    ;; Test with hide-all-blocks enabled (should get both blocks)
    (let ((hide-imports-hide-all-blocks t))
      (hide-imports-test-with-elixir-buffer "defmodule MyModule do\n  import Foo\n  import Bar\n\n  def some_function do\n    :ok\n  end\n\n  import Baz\n  import Qux\nend"
        (let ((regions (hide-imports--get-imports-regions)))
          (should regions)
          (should (= (length regions) 2)))))))

;;; Nested Scope Tests

(ert-deftest hide-imports-python-nested-in-function ()
  "Test Python imports nested within a function."
  (hide-imports-test-with-min-rows 1
    (hide-imports-test-with-treesit-buffer "def my_function():\n    import os\n    import sys\n    # Some code\n    return 'hello'"
      (when (hide-imports--supported-mode-p)
        (let ((regions (hide-imports--get-imports-regions)))
          (should regions)
          (should (= (length regions) 1))
          (let ((region-text (buffer-substring (caar regions) (cdar regions))))
            (should (string-match-p "import os" region-text))
            (should (string-match-p "import sys" region-text))
            (should-not (string-match-p "def my_function" region-text))
            (should-not (string-match-p "return" region-text))))))))

(ert-deftest hide-imports-python-nested-in-class ()
  "Test Python imports nested within a class."
  (hide-imports-test-with-min-rows 1
    (hide-imports-test-with-treesit-buffer "class MyClass:\n    def method(self):\n        import json\n        import re\n        return json.dumps({})"
      (when (hide-imports--supported-mode-p)
        (let ((regions (hide-imports--get-imports-regions)))
          (should regions)
          (should (= (length regions) 1))
          (let ((region-text (buffer-substring (caar regions) (cdar regions))))
            (should (string-match-p "import json" region-text))
            (should (string-match-p "import re" region-text))
            (should-not (string-match-p "class MyClass" region-text))
            (should-not (string-match-p "def method" region-text))))))))

(ert-deftest hide-imports-python-mixed-nested-and-toplevel ()
  "Test Python with both top-level and nested imports."
  (hide-imports-test-with-min-rows 1
    (let ((hide-imports-hide-all-blocks t))
      (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\ndef func():\n    import json\n    import re\n    return 'test'\n\nprint('hello')"
        (when (hide-imports--supported-mode-p)
          (let ((regions (hide-imports--get-imports-regions)))
            (should regions)
            (should (= (length regions) 2))
            ;; First block: top-level imports
            (let ((first-block-text (buffer-substring (caar regions) (cdar regions))))
              (should (string-match-p "import os" first-block-text))
              (should (string-match-p "import sys" first-block-text)))
            ;; Second block: nested imports
            (let ((second-block-text (buffer-substring (car (nth 1 regions)) (cdr (nth 1 regions)))))
              (should (string-match-p "import json" second-block-text))
              (should (string-match-p "import re" second-block-text)))))))))

(ert-deftest hide-imports-python-deeply-nested ()
  "Test Python imports deeply nested in multiple levels."
  (hide-imports-test-with-min-rows 1
    (hide-imports-test-with-treesit-buffer "class Outer:\n    class Inner:\n        def method(self):\n            if True:\n                import datetime\n                import time\n                return 'nested'"
      (when (hide-imports--supported-mode-p)
        (let ((regions (hide-imports--get-imports-regions)))
          (should regions)
          (should (= (length regions) 1))
          (let ((region-text (buffer-substring (caar regions) (cdar regions))))
            (should (string-match-p "import datetime" region-text))
            (should (string-match-p "import time" region-text))
            (should-not (string-match-p "class Outer" region-text))
            (should-not (string-match-p "if True" region-text))))))))

(ert-deftest hide-imports-python-nested-with-comments ()
  "Test Python nested imports with comments."
  (hide-imports-test-with-min-rows 1
    (hide-imports-test-with-treesit-buffer "def process_data():\n    # Need these for data processing\n    import pandas as pd\n    import numpy as np\n    # Process the data\n    return pd.DataFrame()"
      (when (hide-imports--supported-mode-p)
        (let ((regions (hide-imports--get-imports-regions)))
          (should regions)
          (should (= (length regions) 1))
          (let ((region-text (buffer-substring (caar regions) (cdar regions))))
            (should (string-match-p "import pandas" region-text))
            (should (string-match-p "import numpy" region-text))
            ;; Comments between imports should be included
            (should (string-match-p "Need these for data processing" region-text))
            (should-not (string-match-p "def process_data" region-text))
            (should-not (string-match-p "Process the data" region-text))))))))

(ert-deftest hide-imports-elixir-nested-in-function ()
  "Test Elixir imports nested within a function."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'elixir)))
  (hide-imports-test-with-min-rows 1
    (hide-imports-test-with-elixir-buffer "defmodule MyModule do\n  def my_function do\n    alias MyApp.Helper\n    import Ecto.Query\n    # Some logic here\n    :ok\n  end\nend"
      (let ((regions (hide-imports--get-imports-regions)))
        (should regions)
        (should (= (length regions) 1))
        (let ((region-text (buffer-substring (caar regions) (cdar regions))))
          (should (string-match-p "alias MyApp.Helper" region-text))
          (should (string-match-p "import Ecto.Query" region-text))
          (should-not (string-match-p "def my_function" region-text))
          (should-not (string-match-p "Some logic" region-text)))))))

;;; Auto-hide Timer Tests

(ert-deftest hide-imports-auto-hide-timer ()
  "Test that imports auto-hide after the configured delay."
  (hide-imports-test-with-min-rows 1
    (let ((hide-imports-auto-hide-delay 0.1)) ;; Very short delay for testing
      (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\ndef func():\n    pass"
        (when (hide-imports--supported-mode-p)
          (let ((regions (hide-imports--get-imports-regions)))
            (should regions)
            (should (= (length regions) 1))

            ;; Enable hide-imports-mode
            (hide-imports-mode 1)

            ;; Move cursor to imports
            (goto-char (car (nth 0 regions)))
            (hide-imports--post-command-hook)

            ;; Imports should be visible (no overlay)
            (should-not (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))

            ;; Move cursor outside imports
            (goto-char (point-max))
            (hide-imports--post-command-hook)

            ;; Should not be hidden immediately (timer is scheduled)
            (should-not (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))

            ;; Wait for timer to fire
            (sleep-for 0.2)

            ;; Now imports should be hidden
            (should (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))))))))

(ert-deftest hide-imports-auto-hide-disabled ()
  "Test that auto-hide is disabled when delay is 0."
  (hide-imports-test-with-min-rows 1
    (let ((hide-imports-auto-hide-delay 0)) ;; Disabled
      (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\ndef func():\n    pass"
        (when (hide-imports--supported-mode-p)
          (let ((regions (hide-imports--get-imports-regions)))
            (should regions)
            (should (= (length regions) 1))

            ;; Enable hide-imports-mode
            (hide-imports-mode 1)

            ;; Move cursor to imports
            (goto-char (car (nth 0 regions)))
            (hide-imports--post-command-hook)

            ;; Imports should be visible
            (should-not (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))

            ;; Move cursor outside imports
            (goto-char (point-max))
            (hide-imports--post-command-hook)

            ;; Should be hidden immediately since delay is 0
            (should (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))))))))

(ert-deftest hide-imports-auto-hide-not-canceled-by-movement-outside ()
  "Test that moving cursor outside imports doesn't cancel the auto-hide timer."
  (hide-imports-test-with-min-rows 1
    (let ((hide-imports-auto-hide-delay 0.1)) ;; Short delay for testing
      (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\ndef func():\n    pass\n\ndef other():\n    pass"
        (when (hide-imports--supported-mode-p)
          (let ((regions (hide-imports--get-imports-regions)))
            (should regions)
            (should (= (length regions) 1))

            ;; Enable hide-imports-mode
            (hide-imports-mode 1)

            ;; Move cursor to imports
            (goto-char (car (nth 0 regions)))
            (hide-imports--post-command-hook)

            ;; Imports should be visible
            (should-not (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))

            ;; Move cursor outside imports to start timer
            (goto-char (point-max))
            (hide-imports--post-command-hook)

            ;; Should not be hidden immediately (timer is scheduled)
            (should-not (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))

            ;; Move cursor to another position outside imports (should not cancel timer)
            (goto-char (- (point-max) 5))
            (hide-imports--post-command-hook)

            ;; Should still not be hidden immediately
            (should-not (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))

            ;; Wait for timer to fire
            (sleep-for 0.2)

            ;; Now imports should be hidden (timer wasn't canceled by movement outside)
            (should (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))))))))

(ert-deftest hide-imports-multiple-regions-independent-timers ()
  "Test that multiple regions can have independent auto-hide timers."
  (hide-imports-test-with-min-rows 1
    (let ((hide-imports-auto-hide-delay 0.1)
          (hide-imports-hide-all-blocks t)) ;; Enable multiple regions
      (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\ndef func1():\n    pass\n\nimport json\nimport re\n\ndef func2():\n    pass"
        (when (hide-imports--supported-mode-p)
          (let ((regions (hide-imports--get-imports-regions)))
            (should regions)
            (should (= (length regions) 2))

            ;; Enable hide-imports-mode
            (hide-imports-mode 1)

            ;; Move cursor to first region
            (goto-char (car (nth 0 regions)))
            (hide-imports--post-command-hook)

            ;; First region should be visible, second hidden
            (should-not (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))
            (should (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 1 regions)))

            ;; Move cursor to second region
            (goto-char (car (nth 1 regions)))
            (hide-imports--post-command-hook)

            ;; First region should be scheduled for auto-hide, second should be visible
            (should-not (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))
            (should-not (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 1 regions)))

            ;; Move cursor outside all regions
            (goto-char (point-max))
            (hide-imports--post-command-hook)

            ;; Both regions should still be visible (timers are running)
            (should-not (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))
            (should-not (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 1 regions)))

            ;; Wait for timers to fire
            (sleep-for 0.2)

            ;; Now both regions should be hidden (independent timers fired)
            (should (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))
            (should (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 1 regions)))))))))

;;; Independent Region Unhiding Tests

(ert-deftest hide-imports-independent-region-unhiding ()
  "Test that import regions are shown/hidden independently."
  (hide-imports-test-with-min-rows 1
    (let ((hide-imports-hide-all-blocks t)
          (hide-imports-auto-hide-delay 0)) ;; Disable auto-hide timer for this test
      (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\ndef func1():\n    pass\n\nimport json\nimport re\n\ndef func2():\n    pass"
        (when (hide-imports--supported-mode-p)
          (let ((regions (hide-imports--get-imports-regions)))
            (should regions)
            (should (= (length regions) 2))

            ;; Enable hide-imports-mode
            (hide-imports-mode 1)

            ;; Move cursor outside imports first to ensure overlays are created
            (goto-char (point-max))
            (hide-imports--post-command-hook)

            ;; Initially, both regions should be hidden (cursor not in imports)
            (should (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))
            (should (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 1 regions)))

            ;; Move cursor to first region
            (goto-char (car (nth 0 regions)))
            (hide-imports--post-command-hook)

            ;; First region should be shown, second should remain hidden
            (should-not (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))
            (should (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 1 regions)))

            ;; Move cursor to second region
            (goto-char (car (nth 1 regions)))
            (hide-imports--post-command-hook)

            ;; First region should be hidden again, second should be shown
            (should (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))
            (should-not (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 1 regions)))

            ;; Move cursor outside imports
            (goto-char (point-max))
            (hide-imports--post-command-hook)

            ;; Both regions should be hidden again
            (should (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 0 regions)))
            (should (hide-imports--window-has-overlay-for-region-p (selected-window) (nth 1 regions)))))))))

(ert-deftest hide-imports-region-specific-cursor-detection ()
  "Test that cursor region detection works correctly."
  (hide-imports-test-with-min-rows 1
    (let ((hide-imports-hide-all-blocks t))
      (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\ndef func1():\n    pass\n\nimport json\nimport re\n\ndef func2():\n    pass"
        (when (hide-imports--supported-mode-p)
          (let ((regions (hide-imports--get-imports-regions)))
            (should regions)
            (should (= (length regions) 2))

            ;; Set the regions variable for cursor detection
            (setq hide-imports--imports-regions regions)

            ;; Test cursor outside imports
            (goto-char (point-max))
            (should-not (hide-imports--get-cursor-region))

            ;; Test cursor in first region
            (goto-char (car (nth 0 regions)))
            (should (equal (hide-imports--get-cursor-region) (nth 0 regions)))

            ;; Test cursor in second region
            (goto-char (car (nth 1 regions)))
            (should (equal (hide-imports--get-cursor-region) (nth 1 regions)))

            ;; Test cursor between regions
            (goto-char (+ (cdr (nth 0 regions)) 5))
            (should-not (hide-imports--get-cursor-region))))))))

(ert-deftest hide-imports-overlay-management-per-region ()
  "Test overlay creation and removal for specific regions."
  (hide-imports-test-with-min-rows 1
    (let ((hide-imports-hide-all-blocks t))
      (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\ndef func1():\n    pass\n\nimport json\nimport re\n\ndef func2():\n    pass"
        (when (hide-imports--supported-mode-p)
          (let ((regions (hide-imports--get-imports-regions))
                (window (selected-window)))
            (should regions)
            (should (= (length regions) 2))

            ;; Create overlay for first region only
            (hide-imports--create-overlay-for-region window (nth 0 regions))
            (should (hide-imports--window-has-overlay-for-region-p window (nth 0 regions)))
            (should-not (hide-imports--window-has-overlay-for-region-p window (nth 1 regions)))

            ;; Create overlay for second region
            (hide-imports--create-overlay-for-region window (nth 1 regions))
            (should (hide-imports--window-has-overlay-for-region-p window (nth 0 regions)))
            (should (hide-imports--window-has-overlay-for-region-p window (nth 1 regions)))

            ;; Remove overlay for first region only
            (hide-imports--remove-overlay-for-region window (nth 0 regions))
            (should-not (hide-imports--window-has-overlay-for-region-p window (nth 0 regions)))
            (should (hide-imports--window-has-overlay-for-region-p window (nth 1 regions)))))))))

(provide 'test-hide-imports-mode)

;;; test-hide-imports-mode.el ends here