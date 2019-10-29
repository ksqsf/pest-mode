# Pest mode

Pest mode provides a major mode for editing Pest files.  It also contains a few advanced features.

Pest-mode features:
* Syntax highlighting
* Indentation
* Imenu integration
* Eldoc integration (requires `pesta`)
* Flymake integration for syntax checking (requires `pesta`)
* Language studio, where you can experiment the grammar (requires `pesta`)

## Installation

`pest-mode` is not on any package archive yet.  You must manually download the file and put it in your `load-path`, and write in your config file:

```emacs-lisp
(autoload 'pest-mode "pest-mode")
(add-to-list #'auto-mode-alist '("\\.pest\\'" . pest-mode))
```

To use the more advanced features, you must have `pesta` installed.

1. [Install the Rust toolchain](https://rustup.rs).
2. `cd pesta && cargo build --release`
3. Find the executable `pesta` under `target/release`, and put it in
   your `PATH`.
