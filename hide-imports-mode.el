;;; hide-imports-mode.el --- Hide import statements in code -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Marco Dallagiacoma
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/dallagi/hide-imports-mode

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
;; statements in your code to reduce visual clutter.

;;; Code:

(require 'treesit)
(require 'seq)

(defgroup hide-imports nil
  "Hide import statements in code."
  :group 'convenience
  :prefix "hide-imports-")

(defcustom hide-imports-replacement-text "Imports..."
  "Text to display when imports are hidden."
  :type 'string
  :group 'hide-imports)

(defcustom hide-imports-global-modes
  '(python-mode python-ts-mode rust-mode rust-ts-mode rustic-mode elixir-mode elixir-ts-mode)
  "List of major modes where hide-imports-global-mode should be enabled."
  :type '(repeat (symbol :tag "Major mode"))
  :group 'hide-imports)

(defcustom hide-imports-minimum-rows 3
  "Minimum number of rows required to hide imports.
If the import region contains fewer rows than this value, imports will remain visible."
  :type 'integer
  :group 'hide-imports)

(defcustom hide-imports-hide-all-blocks nil
  "When non-nil, hide all contiguous import blocks instead of only the first one.
Each contiguous block of imports/comments must meet the minimum-rows threshold to be hidden."
  :type 'boolean
  :group 'hide-imports)

(defcustom hide-imports-auto-hide-delay 1.0
  "Delay in seconds before automatically hiding imports when cursor exits the region.
Set to 0 to disable auto-hide functionality."
  :type 'number
  :group 'hide-imports)

(defcustom hide-imports-refresh-delay 0.5
  "Delay in seconds before refreshing import regions after buffer changes.
This prevents constant recalculation while typing. Set to 0 for immediate refresh."
  :type 'number
  :group 'hide-imports)

(defvar hide-imports--overlays nil
  "List of overlays created by hide-imports-mode.")

(defvar hide-imports--imports-regions nil
  "Cached list of regions containing imports for auto-unhide functionality.
Each element is a cons cell (START . END).")


(defvar hide-imports--window-states nil
  "Alist mapping windows to their cursor-in-imports state.")

(defvar-local hide-imports--last-cursor-position nil
  "Last cursor position to detect movement direction.")

(defvar-local hide-imports--auto-hide-timers nil
  "Alist of (REGION . TIMER) for auto-hiding imports when cursor exits region.")

(defvar-local hide-imports--refresh-timer nil
  "Timer for delayed refresh of import regions after buffer changes.")

(defvar hide-imports--language-configs
  '((python . ((modes . (python-ts-mode python-mode))
               (language . python)
               (import-types . ("import_statement" "import_from_statement"))))
    (rust . ((modes . (rust-ts-mode rust-mode rustic-mode))
             (language . rust)
             (import-types . ("use_declaration" "extern_crate_declaration"))))
    (elixir . ((modes . (elixir-ts-mode elixir-mode))
               (language . elixir)
               (import-types . ("call")))))
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

(defun hide-imports--is-import-node-p (node language import-types)
  "Check if NODE is an import node for LANGUAGE using IMPORT-TYPES."
  (let ((node-type (treesit-node-type node)))
    (if (eq language 'elixir)
        (hide-imports--is-elixir-import-node-p node)
      (member node-type import-types))))

(defun hide-imports--is-elixir-import-node-p (node)
  "Check if NODE is an Elixir import node (alias, import, require, use)."
  (when (string= (treesit-node-type node) "call")
    (let ((first-child (treesit-node-child node 0)))
      (when (and first-child (string= (treesit-node-type first-child) "identifier"))
        (let ((func-name (treesit-node-text first-child)))
          (member func-name '("alias" "import" "require" "use")))))))

(defun hide-imports--count-rows (start end)
  "Count the number of rows between START and END positions."
  (save-excursion
    (goto-char start)
    (let ((start-line (line-number-at-pos)))
      (goto-char end)
      (let ((end-line (line-number-at-pos)))
        (1+ (- end-line start-line))))))

(defun hide-imports--region-meets-minimum-rows-p (region)
  "Check if REGION has at least `hide-imports-minimum-rows` rows."
  (when region
    (>= (hide-imports--count-rows (car region) (cdr region))
        hide-imports-minimum-rows)))

(defun hide-imports--get-imports-regions ()
  "Get all regions containing imports using treesit.
Returns a list of regions (cons cells). When hide-imports-hide-all-blocks is nil,
only the first region is returned for backward compatibility."
  (when (hide-imports--supported-mode-p)
    (when-let ((config (hide-imports--get-language-config)))
      (let ((language (alist-get 'language (cdr config)))
            (import-types (alist-get 'import-types (cdr config))))
        (let ((all-regions (if (eq language 'elixir)
                               (hide-imports--get-all-elixir-imports-regions)
                             (hide-imports--get-all-standard-imports-regions language import-types))))
          ;; Filter regions that meet minimum rows requirement
          (let ((filtered-regions (seq-filter #'hide-imports--region-meets-minimum-rows-p all-regions)))
            ;; Return all regions or just the first one based on configuration
            (if hide-imports-hide-all-blocks
                filtered-regions
              ;; Return only the first region for backward compatibility
              (when filtered-regions (list (car filtered-regions))))))))))

(defun hide-imports--get-imports-region ()
  "Get the first imports region for backward compatibility.
Returns nil if no regions are found, or the first region as a cons cell."
  (car (hide-imports--get-imports-regions)))

(defun hide-imports--collect-import-nodes (node language import-types)
  "Recursively collect all import nodes from NODE and its descendants."
  (let ((import-nodes nil))
    (when node
      ;; Check if current node is an import
      (when (hide-imports--is-import-node-p node language import-types)
        (push node import-nodes))
      ;; Recursively check all children
      (dolist (child (treesit-node-children node))
        (setq import-nodes (append import-nodes 
                                   (hide-imports--collect-import-nodes child language import-types)))))
    import-nodes))

(defun hide-imports--get-all-standard-imports-regions (language import-types)
  "Get all contiguous import regions for Python and Rust languages at any nesting level."
  (let ((root (treesit-buffer-root-node language))
        (regions nil))
    (when root
      ;; Collect all import nodes from the entire tree
      (let ((import-nodes (hide-imports--collect-import-nodes root language import-types)))
        ;; Sort import nodes by position
        (setq import-nodes (sort import-nodes (lambda (a b) 
                                                (< (treesit-node-start a) 
                                                   (treesit-node-start b)))))
        ;; Group contiguous imports into regions, including comments
        (when import-nodes
          (let ((current-start (treesit-node-start (car import-nodes)))
                (current-end (treesit-node-end (car import-nodes)))
                (prev-end (treesit-node-end (car import-nodes))))
            (dolist (node (cdr import-nodes))
              (let ((node-start (treesit-node-start node))
                    (node-end (treesit-node-end node)))
                ;; Check if this import is contiguous with previous ones
                ;; Allow for comments and whitespace between imports
                (if (hide-imports--imports-are-contiguous-p prev-end node-start language)
                    ;; Extend current region, potentially including intermediate content
                    (setq current-end node-end
                          prev-end node-end)
                  ;; Start new region
                  (push (cons current-start current-end) regions)
                  (setq current-start node-start
                        current-end node-end
                        prev-end node-end))))
            ;; Add final region
            (push (cons current-start current-end) regions)
            ;; Now expand regions to include comments and whitespace at the beginning
            (setq regions (mapcar #'hide-imports--expand-region-for-comments regions))))))
    (nreverse regions)))

(defun hide-imports--expand-region-for-comments (region)
  "Expand REGION to include leading comments and whitespace."
  (let ((start (car region))
        (end (cdr region)))
    (save-excursion
      ;; Look backwards from start to include comments
      (goto-char start)
      (beginning-of-line)
      (let ((new-start start))
        ;; Go backwards line by line to find comments that should be included
        (while (and (> (point) (point-min))
                    (progn
                      (beginning-of-line 0) ; Go to previous line
                      (looking-at "[ \t]*\\(#\\|//\\|$\\)"))) ; Comment or empty line
          ;; Only include comments that appear to be related to imports
          (when (looking-at "[ \t]*\\(#\\|//\\)")
            (setq new-start (point))))
        (cons new-start end)))))

(defun hide-imports--imports-are-contiguous-p (prev-end current-start language)
  "Check if imports at PREV-END and CURRENT-START are contiguous.
Allows for comments and whitespace between them.
Also treats the line containing the cursor as valid to avoid splitting
import blocks while the user is editing (syntax may be temporarily broken)."
  (let ((text-between (buffer-substring-no-properties prev-end current-start))
        (contiguous t)
        (cursor-line-num (line-number-at-pos (point)))
        (start-line-num (line-number-at-pos prev-end)))
    (with-temp-buffer
      (insert text-between)
      (goto-char (point-min))
      (while (and contiguous (not (eobp)))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
              (current-line-num (+ start-line-num (1- (line-number-at-pos)))))
          ;; A line is OK if it's empty/whitespace OR it's a comment OR it contains the cursor.
          (unless (or (string-match-p "^[ \t]*$" line)
                      (string-match-p "^[ \t]*#" line)
                      (string-match-p "^[ \t]*//" line)
                      (= current-line-num cursor-line-num))
            (setq contiguous nil)))
        (forward-line 1)))
    contiguous))

(defun hide-imports--get-all-elixir-imports-regions ()
  "Get all contiguous import regions for Elixir language at any nesting level."
  (let ((root (treesit-buffer-root-node 'elixir))
        (regions nil))
    (when root
      ;; Collect all import nodes from the entire tree
      (let ((import-nodes (hide-imports--collect-elixir-import-nodes root)))
        ;; Sort import nodes by position
        (setq import-nodes (sort import-nodes (lambda (a b) 
                                                (< (treesit-node-start a) 
                                                   (treesit-node-start b)))))
        ;; Group contiguous imports into regions
        (when import-nodes
          (let ((current-start (treesit-node-start (car import-nodes)))
                (current-end (treesit-node-end (car import-nodes)))
                (prev-end (treesit-node-end (car import-nodes))))
            (dolist (node (cdr import-nodes))
              (let ((node-start (treesit-node-start node))
                    (node-end (treesit-node-end node)))
                ;; Check if this import is contiguous with previous ones
                (if (hide-imports--imports-are-contiguous-p prev-end node-start 'elixir)
                    ;; Extend current region
                    (setq current-end node-end
                          prev-end node-end)
                  ;; Start new region
                  (push (cons current-start current-end) regions)
                  (setq current-start node-start
                        current-end node-end
                        prev-end node-end))))
            ;; Add final region
            (push (cons current-start current-end) regions)))))
    (nreverse regions)))

(defun hide-imports--collect-elixir-import-nodes (node)
  "Recursively collect all Elixir import nodes from NODE and its descendants."
  (let ((import-nodes nil))
    (when node
      ;; Check if current node is an import
      (when (hide-imports--is-elixir-import-node-p node)
        (push node import-nodes))
      ;; Recursively check all children
      (dolist (child (treesit-node-children node))
        (setq import-nodes (append import-nodes 
                                   (hide-imports--collect-elixir-import-nodes child)))))
    import-nodes))

(defun hide-imports--create-overlay (start end &optional window)
  "Create an overlay to hide imports from START to END, visible only in WINDOW.
If WINDOW is nil, the overlay is visible in all windows."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'display
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

(defun hide-imports--window-has-overlays-p (window)
  "Check if WINDOW has any hide-imports overlays."
  (seq-some (lambda (overlay)
              (eq (overlay-get overlay 'window) window))
            hide-imports--overlays))

(defun hide-imports--window-has-overlay-for-region-p (window region)
  "Check if WINDOW has an overlay for the specific REGION."
  (seq-some (lambda (overlay)
              (and (eq (overlay-get overlay 'window) window)
                   (overlay-start overlay) ; Make sure overlay is valid
                   (overlay-end overlay)
                   (= (overlay-start overlay) (car region))
                   (= (overlay-end overlay) (cdr region))))
            hide-imports--overlays))

(defun hide-imports--remove-overlay-for-region (window region)
  "Remove overlay for REGION in WINDOW if it exists."
  (let ((remaining-overlays nil))
    (dolist (overlay hide-imports--overlays)
      (if (and (eq (overlay-get overlay 'window) window)
               (overlay-start overlay) ; Make sure overlay is valid
               (overlay-end overlay)
               (= (overlay-start overlay) (car region))
               (= (overlay-end overlay) (cdr region)))
          (when (overlay-buffer overlay)
            (delete-overlay overlay))
        (push overlay remaining-overlays)))
    (setq hide-imports--overlays remaining-overlays)))

(defun hide-imports--create-overlay-for-region (window region)
  "Create overlay for specific REGION in WINDOW."
  (unless (hide-imports--window-has-overlay-for-region-p window region)
    (hide-imports--create-overlay (car region) (cdr region) window)))


(defun hide-imports--cursor-in-imports-p (&optional window)
  "Check if cursor is currently in any imports region for WINDOW.
If WINDOW is nil, use the selected window."
  (when hide-imports--imports-regions
    (let ((pos (if window
                   (window-point window)
                 (point))))
      (seq-some (lambda (region)
                  (and (>= pos (car region))
                       (<= pos (cdr region))))
                hide-imports--imports-regions))))

(defun hide-imports--get-cursor-region (&optional window)
  "Get the specific imports region the cursor is currently in for WINDOW.
Returns nil if cursor is not in any region, or the region as a cons cell (START . END)."
  (when hide-imports--imports-regions
    (let ((pos (if window
                   (window-point window)
                 (point))))
      (seq-find (lambda (region)
                  (and (>= pos (car region))
                       (<= pos (cdr region))))
                hide-imports--imports-regions))))

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


(defun hide-imports--position-cursor-in-imports ()
  "Position cursor appropriately when entering imports region."
  (when hide-imports--imports-regions
    (let* ((current-pos (point))
           (last-pos hide-imports--last-cursor-position))

      ;; Find which region the cursor is in
      (let ((current-region (seq-find (lambda (region)
                                        (and (>= current-pos (car region))
                                             (<= current-pos (cdr region))))
                                      hide-imports--imports-regions)))
        (when (and current-region last-pos)
          (let ((imports-start (car current-region))
                (imports-end (cdr current-region)))
            ;; Only adjust if cursor moved into this region from outside
            (when (or (< last-pos imports-start) (> last-pos imports-end))
              (cond
               ;; Coming from above (last position was before imports) - go to beginning
               ((< last-pos imports-start)
                (goto-char imports-start))
               ;; Coming from below (last position was after imports) - go to end
               ((> last-pos imports-end)
                (goto-char (save-excursion
                             (goto-char imports-end)
                             (beginning-of-line)
                             (point))))))))))))

(defun hide-imports--auto-hide-region (window region)
  "Auto-hide REGION in WINDOW after delay."
  (when (and hide-imports-mode 
             (> hide-imports-auto-hide-delay 0)
             (not (hide-imports--cursor-in-imports-p window)))
    (hide-imports--create-overlay-for-region window region))
  ;; Clean up the timer entry
  (setq hide-imports--auto-hide-timers
        (delq (assoc region hide-imports--auto-hide-timers)
              hide-imports--auto-hide-timers)))

(defun hide-imports--cancel-auto-hide-timer (region)
  "Cancel the auto-hide timer for REGION if it exists."
  (when-let ((timer-entry (assoc region hide-imports--auto-hide-timers)))
    (cancel-timer (cdr timer-entry))
    (setq hide-imports--auto-hide-timers
          (delq timer-entry hide-imports--auto-hide-timers))))

(defun hide-imports--cancel-all-auto-hide-timers ()
  "Cancel all auto-hide timers."
  (dolist (timer-entry hide-imports--auto-hide-timers)
    (cancel-timer (cdr timer-entry)))
  (setq hide-imports--auto-hide-timers nil))

(defun hide-imports--schedule-auto-hide (window region)
  "Schedule auto-hide of REGION in WINDOW after the configured delay."
  (when (and hide-imports-mode (> hide-imports-auto-hide-delay 0))
    ;; Cancel existing timer for this region
    (hide-imports--cancel-auto-hide-timer region)
    ;; Schedule new timer
    (let ((timer (run-with-timer hide-imports-auto-hide-delay nil
                                 #'hide-imports--auto-hide-region window region)))
      (push (cons region timer) hide-imports--auto-hide-timers))))

(defun hide-imports--post-command-hook ()
  "Handle cursor movement for auto-unhide functionality with independent region control."
  (when hide-imports-mode
    (let* ((current-window (selected-window))
           (current-region (hide-imports--get-cursor-region current-window))
           (previous-region (hide-imports--get-window-state current-window)))

      ;; Cancel auto-hide timer only for the region cursor moved into
      (when current-region
        (hide-imports--cancel-auto-hide-timer current-region))

      ;; Handle cursor positioning when entering imports region
      (when (and current-region 
                 (hide-imports--window-has-overlay-for-region-p current-window current-region)
                 (not (equal previous-region current-region))
                 hide-imports--imports-regions)
        (hide-imports--position-cursor-in-imports))

      ;; Clean up dead windows
      (hide-imports--cleanup-window-states)

      ;; Handle region transitions independently
      (cond
       ;; Cursor moved into a new region
       ((and current-region (not (equal current-region previous-region)))
        ;; Show the current region (remove its overlay)
        (hide-imports--remove-overlay-for-region current-window current-region)
        ;; Schedule auto-hide for the previous region if we had one and it's different
        (when (and previous-region (not (equal current-region previous-region)))
          (if (> hide-imports-auto-hide-delay 0)
              (hide-imports--schedule-auto-hide current-window previous-region)
            (hide-imports--create-overlay-for-region current-window previous-region))))

       ;; Cursor moved out of imports entirely
       ((and (not current-region) previous-region)
        ;; Schedule auto-hide for the previous region or hide immediately if delay is 0
        (if (> hide-imports-auto-hide-delay 0)
            (hide-imports--schedule-auto-hide current-window previous-region)
          (hide-imports--create-overlay-for-region current-window previous-region)))

       ;; Cursor moved into imports from outside - ensure all other regions are hidden
       ((and current-region (not previous-region))
        ;; Show the current region
        (hide-imports--remove-overlay-for-region current-window current-region)
        ;; Ensure all other regions are hidden
        (dolist (region hide-imports--imports-regions)
          (unless (equal region current-region)
            (hide-imports--create-overlay-for-region current-window region))))

       ;; Cursor is outside imports and no previous state - ensure all regions are hidden
       ;; But only if there are no pending auto-hide timers
       ((and (not current-region) (not previous-region) hide-imports--imports-regions
             (null hide-imports--auto-hide-timers))
        (dolist (region hide-imports--imports-regions)
          (hide-imports--create-overlay-for-region current-window region))))

      ;; Update window state for current window (now stores the region, not just boolean)
      (hide-imports--set-window-state current-window current-region)

      ;; Update last cursor position for next command
      (setq hide-imports--last-cursor-position (point)))))

(defun hide-imports--hide-imports ()
  "Hide imports in the current buffer for all windows."
  (when (hide-imports--supported-mode-p)
    (let ((regions (hide-imports--get-imports-regions)))
      (when regions
        (setq hide-imports--imports-regions regions)
        ;; Create overlays for all windows showing this buffer
        (dolist (window (window-list))
          (when (eq (window-buffer window) (current-buffer))
            (let ((current-region (hide-imports--get-cursor-region window)))
              (dolist (region regions)
                ;; Hide all regions except the one where cursor is located
                (unless (equal region current-region)
                  (hide-imports--create-overlay (car region) (cdr region) window))))))))))

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
        (setq hide-imports--last-cursor-position (point))
        (setq hide-imports--auto-hide-timers nil)
        (hide-imports--hide-imports)
        ;; Initialize window state to current cursor region
        (let ((current-window (selected-window)))
          (when hide-imports--imports-regions
            (hide-imports--set-window-state current-window 
                                            (hide-imports--get-cursor-region current-window))))
        (add-hook 'after-change-functions 'hide-imports--after-change nil t)
        (add-hook 'post-command-hook 'hide-imports--post-command-hook nil t))
    (progn
      (hide-imports--cancel-all-auto-hide-timers)
      ;; Cancel refresh timer
      (when hide-imports--refresh-timer
        (cancel-timer hide-imports--refresh-timer)
        (setq hide-imports--refresh-timer nil))
      (hide-imports--show-imports)
      (setq hide-imports--imports-regions nil)
      (setq hide-imports--cursor-in-imports nil)
      (setq hide-imports--last-cursor-position nil)
      (setq hide-imports--auto-hide-timers nil)
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

(defun hide-imports--after-change (_beg _end _len)
  "Re-hide imports after buffer change with configurable delay."
  (when hide-imports-mode
    ;; Cancel any existing refresh timer
    (when hide-imports--refresh-timer
      (cancel-timer hide-imports--refresh-timer)
      (setq hide-imports--refresh-timer nil))

    ;; Schedule new refresh with configurable delay
    (setq hide-imports--refresh-timer
          (run-with-idle-timer 
           hide-imports-refresh-delay nil
           (lambda (buffer)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq hide-imports--refresh-timer nil)
                 ;; Cancel any pending timers since regions are changing
                 (hide-imports--cancel-all-auto-hide-timers)
                 ;; Clean up window states for this buffer, as regions will be recalculated
                 (setq hide-imports--window-states
                       (seq-filter (lambda (entry)
                                     (and (window-live-p (car entry))
                                          (not (eq (window-buffer (car entry)) buffer))))
                                   hide-imports--window-states))
                 ;; Now, perform the refresh
                 (setq hide-imports--imports-regions nil)
                 (hide-imports--show-imports)
                 (hide-imports--hide-imports)
                 ;; Re-initialize window state after refresh
                 (dolist (window (get-buffer-window-list buffer nil t))
                   (with-selected-window window
                     (hide-imports--set-window-state window
                                                     (hide-imports--get-cursor-region window)))))))
           (current-buffer)))))

(defun hide-imports--maybe-turn-on ()
  "Turn on hide-imports-mode if the current buffer's major mode is supported."
  (when (and (not hide-imports-mode)
             (hide-imports--supported-mode-p)
             (memq major-mode hide-imports-global-modes))
    (hide-imports-mode 1)))

;;;###autoload
(define-globalized-minor-mode hide-imports-global-mode
  hide-imports-mode
  hide-imports--maybe-turn-on
  :group 'hide-imports
  :require 'hide-imports-mode)

(provide 'hide-imports-mode)

;;; hide-imports-mode.el ends here
