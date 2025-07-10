# hide-imports-mode

`hide-imports-mode` is a minor mode for Emacs that helps reduce visual clutter by hiding import statements in your code.

## Features

- Hides import statements in supported major modes:
    - Elixir
    - Python
    - Rust
- Automatically shows imports when your cursor is within the import region.
- Provides a toggle function to manually show/hide imports.

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

## Demo

![hide-imports-mode demo](demo.gif)

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

### Toggling Imports

-   Use `M-x hide-imports-toggle` or the keybinding `C-c C-i` (if enabled) to manually show or hide imports.
-   Imports will automatically reappear when your cursor enters the hidden import region and hide again when you move out.

## Configuration

-   `hide-imports-replacement-text`: Customize the text displayed when imports are hidden (default: "Imports...").
    ```emacs-lisp
    (setq hide-imports-replacement-text "...")
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
-   `hide-imports-global-modes`: A list of major modes where `hide-imports-global-mode` should activate `hide-imports-mode`.
    ```emacs-lisp
    (setq hide-imports-global-modes '(python-mode python-ts-mode rust-mode))
    ```
