;;; hide-imports-mode.el --- Hide import statements in code -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/youruser/hide-imports-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Hide-imports-mode is a minor mode that allows you to hide import
;; statements in your code to reduce visual clutter and focus on the
;; actual implementation.

;;; Code:

(require 'treesit)
(require 'seq)
(require 'subr-x)

(defgroup hide-imports nil
  "Hide import statements in code."
  :group 'convenience
  :prefix "hide-imports-")

(defcustom hide-imports-replacement-text "Imports"
  "Text to display when imports are hidden."
  :type 'string
  :group 'hide-imports)

(defcustom global-hide-imports-modes
  '(python-mode python-ts-mode rust-mode rust-ts-mode rustic-mode)
  "List of major modes where global-hide-imports-mode should be enabled."
  :type '(repeat (symbol :tag "Major mode"))
  :group 'hide-imports)

(defvar hide-imports--overlays nil
  "List of overlays created by hide-imports-mode.")

(defvar hide-imports--imports-region nil
  "Cached region containing imports for auto-unhide functionality.")

(defvar hide-imports--cursor-in-imports nil
  "Track if cursor is currently in the imports region.")

(defvar hide-imports--window-states nil
  "Alist mapping windows to their cursor-in-imports state.")

(defvar hide-imports--language-configs
  '((python . ((modes . (python-ts-mode python-mode))
               (language . python)
               (import-types . ("import_statement" "import_from_statement"))))
    (rust . ((modes . (rust-ts-mode rust-mode rustic-mode))
             (language . rust)
             (import-types . ("use_declaration" "extern_crate_declaration")))))
  "Configuration for different languages.")

(defun hide-imports--get-language-config ()
  "Get the language configuration for the current buffer."
  (seq-find (lambda (config)
              (let ((modes (alist-get 'modes (cdr config))))
                (memq major-mode modes)))
            hide-imports--language-configs))

(defun hide-imports--supported-mode-p ()
  "Check if current buffer is supported and has tree-sitter support."
  (when-let ((config (hide-imports--get-language-config)))
    (let ((language (alist-get 'language (cdr config))))
      (and (treesit-available-p)
           (treesit-language-available-p language)))))

(defun hide-imports--get-imports-region ()
  "Get the region containing imports at the top of the file using treesit."
  (when (hide-imports--supported-mode-p)
    (when-let ((config (hide-imports--get-language-config)))
      (let ((language (alist-get 'language (cdr config)))
            (import-types (alist-get 'import-types (cdr config))))
        (let ((root (treesit-buffer-root-node language))
              (start-pos nil)
              (end-pos nil)
              (has-imports nil))
          (when root
            (let ((children (treesit-node-children root))
                  (found-non-import nil))
              (dolist (child children)
                (let ((node-type (treesit-node-type child)))
                  (cond
                   ((member node-type import-types)
                    (setq has-imports t)
                    (unless start-pos
                      (setq start-pos (treesit-node-start child)))
                    (setq end-pos (treesit-node-end child)))
                   ((and (string= node-type "comment")
                         has-imports
                         (not found-non-import))
                    ;; Only include comments that are interspersed with imports
                    ;; Check if there are more imports after this comment
                    (let ((remaining-children (cdr (memq child children)))
                          (has-more-imports nil))
                      (dolist (remaining-child remaining-children)
                        (when (member (treesit-node-type remaining-child) import-types)
                          (setq has-more-imports t)))
                      (when has-more-imports
                        (setq end-pos (treesit-node-end child)))))
                   ((not (string= node-type "comment"))
                    (setq found-non-import t)))))
              (when (and start-pos end-pos has-imports)
                (cons start-pos end-pos)))))))))

(defun hide-imports--create-overlay (start end &optional window)
  "Create an overlay to hide imports from START to END, visible only in WINDOW.
If WINDOW is nil, the overlay is visible in all windows."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'invisible 'hide-imports)
    (overlay-put overlay 'before-string 
                 (propertize hide-imports-replacement-text 
                             'face 'font-lock-comment-face))
    (overlay-put overlay 'hide-imports t)
    (when window
      (overlay-put overlay 'window window))
    (push overlay hide-imports--overlays)
    overlay))

(defun hide-imports--remove-overlays (&optional window)
  "Remove hide-imports overlays for WINDOW.
If WINDOW is nil, remove all overlays."
  (if window
      ;; Remove only overlays for specific window
      (let ((remaining-overlays nil))
        (dolist (overlay hide-imports--overlays)
          (if (eq (overlay-get overlay 'window) window)
              (when (overlay-buffer overlay)
                (delete-overlay overlay))
            (push overlay remaining-overlays)))
        (setq hide-imports--overlays remaining-overlays))
    ;; Remove all overlays
    (dolist (overlay hide-imports--overlays)
      (when (overlay-buffer overlay)
        (delete-overlay overlay)))
    (setq hide-imports--overlays nil)))

(defun hide-imports--get-window-overlays (window)
  "Get overlays for a specific WINDOW."
  (seq-filter (lambda (overlay)
                (eq (overlay-get overlay 'window) window))
              hide-imports--overlays))

(defun hide-imports--window-has-overlays-p (window)
  "Check if WINDOW has any hide-imports overlays."
  (seq-some (lambda (overlay)
              (eq (overlay-get overlay 'window) window))
            hide-imports--overlays))

(defun hide-imports--create-window-overlay (window)
  "Create overlay for WINDOW to hide imports."
  (when (hide-imports--supported-mode-p)
    (let ((region (hide-imports--get-imports-region)))
      (when region
        (setq hide-imports--imports-region region)
        (hide-imports--create-overlay (car region) (cdr region) window)))))

(defun hide-imports--cursor-in-imports-p (&optional window)
  "Check if cursor is currently in the imports region for WINDOW.
If WINDOW is nil, use the selected window."
  (when hide-imports--imports-region
    (let ((pos (if window
                   (window-point window)
                 (point))))
      (and (>= pos (car hide-imports--imports-region))
           (<= pos (cdr hide-imports--imports-region))))))

(defun hide-imports--get-window-state (window)
  "Get the cursor-in-imports state for WINDOW."
  (alist-get window hide-imports--window-states))

(defun hide-imports--set-window-state (window state)
  "Set the cursor-in-imports STATE for WINDOW."
  (setf (alist-get window hide-imports--window-states) state))

(defun hide-imports--cleanup-window-states ()
  "Remove entries for non-existent windows from window states."
  (setq hide-imports--window-states
        (seq-filter (lambda (entry)
                      (window-live-p (car entry)))
                    hide-imports--window-states)))

(defun hide-imports--any-window-in-imports-p ()
  "Check if any window showing this buffer has cursor in imports region."
  (let ((current-buffer (current-buffer)))
    (seq-some (lambda (window)
                (and (eq (window-buffer window) current-buffer)
                     (hide-imports--cursor-in-imports-p window)))
              (window-list))))

(defun hide-imports--post-command-hook ()
  "Handle cursor movement for auto-unhide functionality."
  (when hide-imports-mode
    (let* ((current-window (selected-window))
           (cursor-in-imports (hide-imports--cursor-in-imports-p))
           (previous-state (hide-imports--get-window-state current-window))
           (window-has-overlays (hide-imports--window-has-overlays-p current-window)))
      
      ;; Update window state for current window
      (hide-imports--set-window-state current-window cursor-in-imports)
      
      ;; Clean up dead windows
      (hide-imports--cleanup-window-states)
      
      ;; Show/hide imports for the current window only
      (cond
       (cursor-in-imports
        ;; Cursor in imports - show imports in this window (remove overlays)
        (when window-has-overlays
          (hide-imports--remove-overlays current-window)))
       ((not cursor-in-imports)
        ;; Cursor not in imports - hide imports in this window (add overlays)
        (unless window-has-overlays
          (hide-imports--create-window-overlay current-window)))))))

(defun hide-imports--hide-imports ()
  "Hide imports in the current buffer for all windows."
  (when (hide-imports--supported-mode-p)
    (let ((region (hide-imports--get-imports-region)))
      (when region
        (setq hide-imports--imports-region region)
        ;; Create overlays for all windows showing this buffer where cursor is not in imports
        (dolist (window (window-list))
          (when (eq (window-buffer window) (current-buffer))
            (unless (hide-imports--cursor-in-imports-p window)
              (hide-imports--create-overlay (car region) (cdr region) window))))))))

(defun hide-imports--show-imports ()
  "Show imports in the current buffer for all windows."
  (hide-imports--remove-overlays))

;;;###autoload
(define-minor-mode hide-imports-mode
  "Minor mode to hide import statements in code."
  :lighter " HideImports"
  :group 'hide-imports
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-i") 'hide-imports-toggle)
            map)
  (if hide-imports-mode
      (progn
        (add-to-invisibility-spec '(hide-imports . t))
        (setq hide-imports--cursor-in-imports nil)
        (hide-imports--hide-imports)
        (add-hook 'after-change-functions 'hide-imports--after-change nil t)
        (add-hook 'post-command-hook 'hide-imports--post-command-hook nil t))
    (progn
      (remove-from-invisibility-spec '(hide-imports . t))
      (hide-imports--show-imports)
      (setq hide-imports--imports-region nil)
      (setq hide-imports--cursor-in-imports nil)
      ;; Clean up window states for this buffer
      (setq hide-imports--window-states
            (seq-filter (lambda (entry)
                          (and (window-live-p (car entry))
                               (not (eq (window-buffer (car entry)) (current-buffer)))))
                        hide-imports--window-states))
      (remove-hook 'after-change-functions 'hide-imports--after-change t)
      (remove-hook 'post-command-hook 'hide-imports--post-command-hook t))))

(defun hide-imports-toggle ()
  "Toggle visibility of imports."
  (interactive)
  (if hide-imports--overlays
      (hide-imports--show-imports)
    (hide-imports--hide-imports)))

(defun hide-imports--after-change (beg end len)
  "Re-hide imports after buffer changes."
  (when hide-imports-mode
    (run-with-idle-timer 0.1 nil
                         (lambda ()
                           (when (buffer-live-p (current-buffer))
                             (with-current-buffer (current-buffer)
                               (setq hide-imports--imports-region nil)
                               (setq hide-imports--cursor-in-imports nil)
                               (hide-imports--show-imports)
                               (hide-imports--hide-imports)))))))

(defun hide-imports--maybe-turn-on ()
  "Turn on hide-imports-mode if the current buffer's major mode is supported."
  (when (and (not hide-imports-mode)
             (hide-imports--supported-mode-p)
             (memq major-mode global-hide-imports-modes))
    (hide-imports-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-hide-imports-mode
  hide-imports-mode
  hide-imports--maybe-turn-on
  :group 'hide-imports
  :require 'hide-imports-mode)

(provide 'hide-imports-mode)

;;; hide-imports-mode.el ends here