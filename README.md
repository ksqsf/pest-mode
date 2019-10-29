# Pest mode

Pest mode provides a major mode for editing Pest files.  It also contains a few advanced features.

Pest-mode features:
* Syntax highlighting
* Indentation
* Imenu integration
* Eldoc integration (requires `pesta`)
* Flymake integration for syntax checking (requires `pesta`)
* Language studio, where you can experiment the grammar (requires `pesta`)

This repo currently does NOT contain `pest-mode.el`, it's [here](https://github.com/ksqsf/emacs-config/blob/master/lisp/pest-mode.el).  It's not ready yet, but I need a place for the `pesta` helper program.

## Installation

`pest-mode` is not on any package archive yet.  You must manually download the file and put it in your `load-path`, and write in your config file:

```emacs-lisp
(autoload 'pest-mode "pest-mode")
(add-to-list #'auto-mode-alist '("\\.pest\\'" . pest-mode))
```
