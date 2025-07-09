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

(defgroup hide-imports nil
  "Hide import statements in code."
  :group 'convenience
  :prefix "hide-imports-")

(defcustom hide-imports-replacement-text "Imports..."
  "Text to display when imports are hidden."
  :type 'string
  :group 'hide-imports)

(defvar hide-imports--overlays nil
  "List of overlays created by hide-imports-mode.")

(defun hide-imports--python-mode-p ()
  "Check if current buffer is a Python buffer with tree-sitter support."
  (or (eq major-mode 'python-ts-mode)
      (and (eq major-mode 'python-mode)
           (treesit-available-p)
           (treesit-language-available-p 'python))))

(defun hide-imports--get-python-imports-region ()
  "Get the region containing Python imports at the top of the file using treesit."
  (when (and (hide-imports--python-mode-p)
             (treesit-available-p))
    (let ((root (treesit-buffer-root-node 'python))
          (start-pos nil)
          (end-pos nil))
      (when root
        (let ((children (treesit-node-children root))
              (found-non-import nil))
          (dolist (child children)
            (let ((node-type (treesit-node-type child)))
              (cond
               ((or (string= node-type "import_statement")
                    (string= node-type "import_from_statement")
                    (and (string= node-type "comment")
                         (not found-non-import)))
                (unless start-pos
                  (setq start-pos (treesit-node-start child)))
                (setq end-pos (treesit-node-end child)))
               ((not (string= node-type "comment"))
                (setq found-non-import t)))))
          (when (and start-pos end-pos)
            (cons start-pos (1+ end-pos))))))))

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

(defun hide-imports--hide-imports ()
  "Hide imports in the current buffer."
  (when (hide-imports--python-mode-p)
    (let ((region (hide-imports--get-python-imports-region)))
      (when region
        (hide-imports--create-overlay (car region) (cdr region))))))

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
        (hide-imports--hide-imports)
        (add-hook 'after-change-functions 'hide-imports--after-change nil t))
    (progn
      (remove-from-invisibility-spec '(hide-imports . t))
      (hide-imports--show-imports)
      (remove-hook 'after-change-functions 'hide-imports--after-change t))))

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
                               (hide-imports--show-imports)
                               (hide-imports--hide-imports)))))))

(provide 'hide-imports-mode)

;;; hide-imports-mode.el ends here