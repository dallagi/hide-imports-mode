# hide-imports-mode

<div align="center">
<em>A minor mode for Emacs that helps reduce visual clutter by hiding import statements in your code.</em>
</div>

## Table of Contents

- [Features](#features)
- [Demo](#demo)
- [Installation](#installation)
  - [Manual Installation](#manual-installation)
  - [Using a Package Manager](#using-a-package-manager)
- [Usage](#usage)
  - [Enabling the Mode](#enabling-the-mode)
  - [Auto-Hide Timer Behavior](#auto-hide-timer-behavior)
- [Configuration](#configuration)

## Features

- Hides import statements in supported major modes:
    - Elixir
    - JavaScript
    - Python
    - Rust
    - TypeScript
- Automatically shows imports when your cursor is within the import region.
- Configurable auto-hide timer that hides imports after a delay when cursor exits the region.

## Demo

![hide-imports-mode demo](demo.svg)


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

-   `hide-imports-overlay-text-function`: Function to generate overlay text for hidden imports (default: `hide-imports--default-overlay-text`). The function receives START and END positions and should return a string. The default implementation shows "[N hidden import lines]".
    ```emacs-lisp
    ;; Use the default function that shows line count
    (setq hide-imports-overlay-text-function 'hide-imports--default-overlay-text)

    ;; Use custom function
    (setq hide-imports-overlay-text-function
          (lambda (start end)
            (format "<%d imports>" (count-lines start end))))

    ;; Use static text
    (setq hide-imports-overlay-text-function
          (lambda (start end) "Imports..."))
    ```
-   `hide-imports-minimum-rows`: Minimum number of rows required to hide imports (default: 3). If the import region contains fewer rows than this value, imports will remain visible.
    ```emacs-lisp
    ;; Hide imports only if there are 5+ rows
    (setq hide-imports-minimum-rows 5)

    ;; Hide all imports, even single lines
    (setq hide-imports-minimum-rows 1)
    ```
-   `hide-imports-hide-all-blocks`: When non-nil, hide all contiguous import blocks instead of only the first one (default: nil). Each contiguous block of imports/comments must meet the minimum-rows threshold to be hidden.
    ```emacs-lisp
    ;; Hide all import blocks throughout the file
    (setq hide-imports-hide-all-blocks t)

    ;; Hide only the first import block (default behavior)
    (setq hide-imports-hide-all-blocks nil)
    ```
-   `hide-imports-auto-hide-delay`: Delay in seconds before automatically hiding imports when cursor exits the region (default: 1.0). Set to 0 to disable auto-hide functionality and return to immediate hiding.
    ```emacs-lisp
    ;; Hide imports after 2 seconds
    (setq hide-imports-auto-hide-delay 2.0)

    ;; Disable auto-hide timer (immediate hiding)
    (setq hide-imports-auto-hide-delay 0)

    ;; Very short delay for quick coding
    (setq hide-imports-auto-hide-delay 0.5)
    ```
-   `hide-imports-global-modes`: A list of major modes where `hide-imports-global-mode` should activate `hide-imports-mode`.
    ```emacs-lisp
    (setq hide-imports-global-modes '(python-mode python-ts-mode rust-mode))
    ```
-   `hide-imports-refresh-delay`: Delay in seconds before refreshing import regions after buffer changes (default: 0.5). This prevents constant recalculation while typing. Set to 0 for immediate refresh.
    ```emacs-lisp
    ;; Refresh after 1 second of inactivity
    (setq hide-imports-refresh-delay 1.0)

    ;; Immediate refresh (may impact performance)
    (setq hide-imports-refresh-delay 0)
    ```
