# Pest mode

Pest mode provides a major mode for editing Pest files.  It's still fledging, but you can try it and give me feedback to improve it.

Currently, pest-mode features:
* Syntax highlighting
* Indentation
* Imenu integration
* Flymake integration for syntax checking (requires `pesta`)

This repo currently does NOT contain `pest-mode.el`, it's [here](https://github.com/ksqsf/emacs-config/blob/master/lisp/pest-mode.el).  It's not ready yet, but I need a place for the `pesta` helper program.

## Installation

`pest-mode` is not on any package archive yet.  You must manually download the file and put it in your `load-path`, and write in your config file:

```emacs-lisp
(autoload 'pest-mode "pest-mode")
(add-to-list #'auto-mode-alist '("\\.pest\\'" . pest-mode))
```

## TODO

* language studio (where you can experiment the grammar; requires `pesta`)
