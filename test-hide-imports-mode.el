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

;;; Basic Mode Tests

(ert-deftest hide-imports-mode-activation ()
  "Test basic mode activation and deactivation."
  (hide-imports-test-with-temp-buffer "import os\n\nprint('hello')"
    (should-not hide-imports-mode)
    (hide-imports-mode 1)
    (should hide-imports-mode)
    (should (member '(hide-imports . t) buffer-invisibility-spec))
    (hide-imports-mode -1)
    (should-not hide-imports-mode)
    (should-not (member '(hide-imports . t) buffer-invisibility-spec))))

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
  (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      (let ((region (hide-imports--get-imports-region)))
        (should region)
        (should (= (car region) 1))
        (should (> (cdr region) 1))))))

(ert-deftest hide-imports-import-region-with-from ()
  "Test detection of 'from...import' statements."
  (hide-imports-test-with-treesit-buffer "from os import path\nfrom sys import argv\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      (let ((region (hide-imports--get-imports-region)))
        (should region)
        (should (= (car region) 1))))))

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
            (should (overlay-get overlay 'invisible))
            (should (eq (overlay-get overlay 'invisible) 'hide-imports))
            (should (overlay-get overlay 'hide-imports))
            (should (overlay-get overlay 'before-string))
            (should (string= (overlay-get overlay 'before-string) 
                           (propertize hide-imports-replacement-text 
                                     'face 'font-lock-comment-face)))
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
  (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      ;; Move cursor away from imports region to ensure overlays are created
      (goto-char (point-max))
      (hide-imports--hide-imports)
      ;; With window-local overlays, we need to check for window-specific overlays
      (should (hide-imports--window-has-overlays-p (selected-window)))
      (hide-imports--show-imports)
      (should-not hide-imports--overlays))))

;;; Cursor Movement Tests

(ert-deftest hide-imports-cursor-detection ()
  "Test cursor position detection in imports region."
  (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      (let ((region (hide-imports--get-imports-region)))
        (when region
          (setq hide-imports--imports-region region)
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
          (setq hide-imports--imports-region region)
          
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
  (hide-imports-test-with-treesit-buffer ""
    (when (hide-imports--supported-mode-p)
      (should-not (hide-imports--get-imports-region))
      (hide-imports--hide-imports)
      (should-not hide-imports--overlays))))

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
  (should (stringp hide-imports-replacement-text))
  (let ((original-text hide-imports-replacement-text))
    (setq hide-imports-replacement-text "Hidden imports...")
    (should (string= hide-imports-replacement-text "Hidden imports..."))
    (setq hide-imports-replacement-text original-text)))

;;; Cleanup

(ert-deftest hide-imports-cleanup-on-disable ()
  "Test proper cleanup when mode is disabled."
  (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
    (when (hide-imports--supported-mode-p)
      ;; Move cursor away from imports region
      (goto-char (point-max))
      (hide-imports-mode 1)
      ;; Need to trigger post-command hook to create overlays
      (hide-imports--post-command-hook)
      (should hide-imports--overlays)
      (should hide-imports--imports-region)
      (hide-imports-mode -1)
      (should-not hide-imports--overlays)
      (should-not hide-imports--imports-region)
      (should-not hide-imports--cursor-in-imports))))

;;; Global Mode Tests

(ert-deftest hide-imports-global-mode-activation ()
  "Test global-hide-imports-mode activation and deactivation."
  (let ((original-global-modes global-hide-imports-modes))
    (unwind-protect
        (progn
          (setq global-hide-imports-modes '(python-mode python-ts-mode))
          (global-hide-imports-mode 1)
          (should global-hide-imports-mode)
          (global-hide-imports-mode -1)
          (should-not global-hide-imports-mode))
      (setq global-hide-imports-modes original-global-modes))))

(ert-deftest hide-imports-global-mode-python-auto-enable ()
  "Test that global mode automatically enables hide-imports-mode in Python buffers."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'python)))
  (let ((original-global-modes global-hide-imports-modes))
    (unwind-protect
        (progn
          (setq global-hide-imports-modes '(python-mode python-ts-mode))
          (global-hide-imports-mode 1)
          (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
            ;; Move cursor away from imports region
            (goto-char (point-max))
            ;; Manually trigger the function since the test doesn't go through normal buffer creation
            (hide-imports--maybe-turn-on)
            (should hide-imports-mode)
            ;; Need to trigger post-command hook to create overlays
            (hide-imports--post-command-hook)
            (should hide-imports--overlays)))
      (global-hide-imports-mode -1)
      (setq global-hide-imports-modes original-global-modes))))

(ert-deftest hide-imports-global-mode-rust-auto-enable ()
  "Test that global mode automatically enables hide-imports-mode in Rust buffers."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'rust)))
  (let ((original-global-modes global-hide-imports-modes))
    (unwind-protect
        (progn
          (setq global-hide-imports-modes '(rust-ts-mode rust-mode))
          (global-hide-imports-mode 1)
          (hide-imports-test-with-rust-buffer "use std::collections::HashMap;\nuse std::fs;\n\nfn main() {\n    println!(\"Hello\");\n}"
            ;; Move cursor away from imports region
            (goto-char (point-max))
            ;; Manually trigger the function since the test doesn't go through normal buffer creation
            (hide-imports--maybe-turn-on)
            (should hide-imports-mode)
            ;; Need to trigger post-command hook to create overlays
            (hide-imports--post-command-hook)
            (should hide-imports--overlays)))
      (global-hide-imports-mode -1)
      (setq global-hide-imports-modes original-global-modes))))

(ert-deftest hide-imports-global-mode-unsupported-mode ()
  "Test that global mode doesn't activate in unsupported modes."
  (let ((original-global-modes global-hide-imports-modes))
    (unwind-protect
        (progn
          (setq global-hide-imports-modes '(python-mode python-ts-mode))
          (global-hide-imports-mode 1)
          (with-temp-buffer
            (text-mode)
            (insert "Some text content")
            (should-not hide-imports-mode)))
      (global-hide-imports-mode -1)
      (setq global-hide-imports-modes original-global-modes))))

(ert-deftest hide-imports-global-mode-excluded-mode ()
  "Test that global mode doesn't activate in modes not in the list."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'python)))
  (let ((original-global-modes global-hide-imports-modes))
    (unwind-protect
        (progn
          (setq global-hide-imports-modes '(rust-ts-mode))  ; Only Rust modes
          (global-hide-imports-mode 1)
          (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
            (should-not hide-imports-mode)))
      (global-hide-imports-mode -1)
      (setq global-hide-imports-modes original-global-modes))))

(ert-deftest hide-imports-global-mode-customization ()
  "Test customization of global-hide-imports-modes."
  (let ((original-global-modes global-hide-imports-modes))
    (unwind-protect
        (progn
          (should (listp global-hide-imports-modes))
          (should (memq 'python-mode global-hide-imports-modes))
          (should (memq 'rust-ts-mode global-hide-imports-modes))
          (setq global-hide-imports-modes '(python-mode))
          (should (equal global-hide-imports-modes '(python-mode))))
      (setq global-hide-imports-modes original-global-modes))))

(ert-deftest hide-imports-maybe-turn-on-function ()
  "Test the hide-imports--maybe-turn-on function directly."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'python)))
  (let ((original-global-modes global-hide-imports-modes))
    (unwind-protect
        (progn
          (setq global-hide-imports-modes '(python-mode python-ts-mode))
          (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
            (should-not hide-imports-mode)
            (hide-imports--maybe-turn-on)
            (should hide-imports-mode)))
      (setq global-hide-imports-modes original-global-modes))))

(ert-deftest hide-imports-maybe-turn-on-already-active ()
  "Test that hide-imports--maybe-turn-on doesn't activate if already active."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'python)))
  (let ((original-global-modes global-hide-imports-modes))
    (unwind-protect
        (progn
          (setq global-hide-imports-modes '(python-mode python-ts-mode))
          (hide-imports-test-with-treesit-buffer "import os\nimport sys\n\nprint('hello')"
            (hide-imports-mode 1)
            (should hide-imports-mode)
            ;; Should not change anything since mode is already active
            (hide-imports--maybe-turn-on)
            (should hide-imports-mode)))
      (setq global-hide-imports-modes original-global-modes))))

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
          (setq hide-imports--imports-region region)
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
          (setq hide-imports--imports-region region)
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
          (setq hide-imports--imports-region region)
          
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
          (setq hide-imports--imports-region region)
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
  (hide-imports-test-with-rust-buffer "use std::collections::HashMap;\n\nfn main() {\n    let map = HashMap::new();\n}"
    (let ((region (hide-imports--get-imports-region)))
      (should region)
      (should (= (car region) 1))
      (let ((imports-text (buffer-substring (car region) (cdr region))))
        (should (string= imports-text "use std::collections::HashMap;"))))))

(ert-deftest hide-imports-rust-extern-crate-only ()
  "Test Rust with only extern crate declarations."
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'rust)))
  (hide-imports-test-with-rust-buffer "extern crate serde;\nextern crate tokio;\n\nfn main() {\n    println!(\"Hello\");\n}"
    (let ((region (hide-imports--get-imports-region)))
      (should region)
      (let ((imports-text (buffer-substring (car region) (cdr region))))
        (should (string-match-p "extern crate serde" imports-text))
        (should (string-match-p "extern crate tokio" imports-text))
        (should-not (string-match-p "fn main" imports-text))))))

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

(provide 'test-hide-imports-mode)

;;; test-hide-imports-mode.el ends here