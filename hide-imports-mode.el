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

(defvar hide-imports--overlays nil
  "List of overlays created by hide-imports-mode.")

(defvar hide-imports--imports-region nil
  "Cached region containing imports for auto-unhide functionality.")

(defvar hide-imports--cursor-in-imports nil
  "Track if cursor is currently in the imports region.")

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

(defun hide-imports--create-overlay (start end)
  "Create an overlay to hide imports from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'invisible 'hide-imports)
    (overlay-put overlay 'before-string 
                 (propertize hide-imports-replacement-text 
                             'face 'font-lock-comment-face))
    (overlay-put overlay 'hide-imports t)
    (push overlay hide-imports--overlays)
    overlay))

(defun hide-imports--remove-overlays ()
  "Remove all hide-imports overlays."
  (dolist (overlay hide-imports--overlays)
    (when (overlay-buffer overlay)
      (delete-overlay overlay)))
  (setq hide-imports--overlays nil))

(defun hide-imports--cursor-in-imports-p ()
  "Check if cursor is currently in the imports region."
  (when hide-imports--imports-region
    (let ((pos (point)))
      (and (>= pos (car hide-imports--imports-region))
           (<= pos (cdr hide-imports--imports-region))))))

(defun hide-imports--post-command-hook ()
  "Handle cursor movement for auto-unhide functionality."
  (when hide-imports-mode
    (let ((cursor-in-imports (hide-imports--cursor-in-imports-p)))
      (cond
       ((and cursor-in-imports (not hide-imports--cursor-in-imports))
        ;; Cursor entered imports region - show imports
        (hide-imports--show-imports)
        (setq hide-imports--cursor-in-imports t))
       ((and (not cursor-in-imports) hide-imports--cursor-in-imports)
        ;; Cursor left imports region - hide imports
        (hide-imports--hide-imports)
        (setq hide-imports--cursor-in-imports nil))))))

(defun hide-imports--hide-imports ()
  "Hide imports in the current buffer."
  (when (hide-imports--supported-mode-p)
    (let ((region (hide-imports--get-imports-region)))
      (when region
        (setq hide-imports--imports-region region)
        (unless (hide-imports--cursor-in-imports-p)
          (hide-imports--create-overlay (car region) (cdr region)))))))

(defun hide-imports--show-imports ()
  "Show imports in the current buffer."
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

(provide 'hide-imports-mode)

;;; hide-imports-mode.el ends here