<div align="center">
  <h1 style="width: 100%; text-align: center">hide-imports-mode</h1>
  <p style="font-size: 18px; white-space: pre-line">
    A minor mode for Emacs that helps reduce visual clutter by hiding import statements in your code.
  </p>
</div>

![hide-imports-mode demo](demo.svg)

## Table of Contents

- [Features](#features)
- [Requirements](#requirements)
- [Installation](#installation)
  - [Manual Installation](#manual-installation)
  - [Using a Package Manager](#using-a-package-manager)
- [Usage](#usage)
  - [Enabling the Mode](#enabling-the-mode)
  - [Auto-Hide Timer Behavior](#auto-hide-timer-behavior)
- [Configuration](#configuration)

## Features

- Hides import statements in supported major modes: Elixir, JavaScript, Python, Rust, TypeScript
- Automatically shows imports when your cursor is within the import region.
- Configurable auto-hide timer that hides imports after a delay when cursor exits the region.

## Requirements

- **Emacs 30.1 or later**
- **Tree-sitter grammars** - Required for supported languages (python, rust, elixir, javascript, typescript)

## Installation

### Manual Installation

1.  Download the `hide-imports-mode.el` file.
2.  Place it in your Emacs `load-path`.
3.  Add the following to your Emacs configuration (`init.el` or `~/.emacs`):

    ```emacs-lisp
    (add-to-list 'load-path "/path/to/hide-imports-mode/") ;; Replace with actual path
    (require 'hide-imports-mode)
    ```

### Using a Package Manager

Using `use-package` and `:vc`:

```emacs-lisp
(use-package hide-imports-mode
  :vc (:fetcher github :repo "dallagi/hide-imports-mode")
  :config
  (hide-imports-global-mode 1))
```

## Usage

### Enabling the Mode

-   **Globally:** To enable `hide-imports-mode` automatically in supported major modes, add this to your Emacs config:
    ```emacs-lisp
    (hide-imports-global-mode 1)
    ```
-   **Per-buffer:** To enable it manually in the current buffer:
    ```emacs-lisp
    M-x hide-imports-mode
    ```

### Auto-Hide Timer Behavior

When you move the cursor away from import regions, they automatically hide after a configurable delay. Each import block has its own independent timer that starts when you leave that region.

## Configuration

```emacs-lisp
;; Overlay text function - generates text shown when imports are hidden
;; Default shows "[N hidden import lines]"
(setq hide-imports-overlay-text-function 'hide-imports--default-overlay-text)
;; You can also provide your own function:
;; (setq hide-imports-overlay-text-function (lambda (start end) (format "<%d imports>" (count-lines start end))))
;; Or to simply use static text:
;; (setq hide-imports-overlay-text-function (lambda (start end) "Imports..."))

;; Minimum rows required to hide imports (default: 3)
(setq hide-imports-minimum-rows 3)

;; Set to t to hide all import blocks throughout file, not just the first one (default: nil)
(setq hide-imports-hide-all-blocks nil)

;; Auto-hide delay in seconds when cursor exits region (default: 1.0)
(setq hide-imports-auto-hide-delay 1.0)
;; Set to 0 for immediate hiding

;; Major modes where global mode activates hide-imports-mode
(setq hide-imports-global-modes '(python-mode python-ts-mode rust-mode elixir-mode elixir-ts-mode
                                  js-mode js-ts-mode typescript-mode typescript-ts-mode))

;; Refresh delay after buffer changes to prevent constant recalculation while typing (default: 0.5)
;; Set to 0 for immediate refresh
(setq hide-imports-refresh-delay 0.5)
```
