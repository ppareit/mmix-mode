MMIX Mode
===========

[Emacs](https://www.gnu.org/software/emacs/) major mode for editing
files in the MMIXAL assembly language. This language is described in
*The Art of Computer Programming, Volume 1, MMIX A RISC Computer for
the New Millennium, Fascicle 1* By *Donald E. Knuth*.

Features
==========

Here are some of the features of `mmix-mode`:

* Font locking
* Autocomplete
* Flycheck support (for syntax checking)
* Integrated help
* Ability to display MMIX object files

Installing
============

The recommended way to install `mmix-mode` is with [`straight.el`](https://github.com/radian-software/straight.el),
a package manager for Emacs that can install packages directly from
their repositories.

Dependencies
---------------

This mode depends on the MMIX binaries. Make sure they are installed and
available in your system's `PATH`.

* [MMIX Home Page](http://mmix.cs.hm.edu/)
* [Linux Binaries](http://mmix.cs.hm.edu/bin/index.html)
* [Windows Executables](http://mmix.cs.hm.edu/exe/index.html)
* [MAC OSX Executables](http://mmix.cs.hm.edu/osx/index.html)

Configuration
---------------------------------------------

The following example shows how to configure `mmix-mode` using `use-package`.
`straight.el` integrates with `use-package` and will automatically
clone the `mmix-mode` repository.

```emacs-lisp
;; `mmix-mode` is the main mode for working with mmix files
(use-package mmix-mode
  :straight (mmix-mode :type git :host github :repo "ppareit/mmix-mode")
  :config
  (setq mmix-mmixal-expand-flag t)
         ;; if you want flycheck support, enable it here
  :hook ((mmix-mode . flycheck-mode)
         ;; default flycheck is highlighting the whole line, setting to nil
         ;; will now only mark in the fringe, wich works best in mmix
         (mmix-mode . (lambda () (setq-local flycheck-highlighting-mode nil))))
  :bind (:map mmix-mode-map
         ;; example setting custom key binding, remove if not needed
         ("M-SPC" . company-complete-common)))

;; `mmo-mode` is included in the same repository.
;; It is autoloaded for files with the .mmo extension.
;; No extra configuration is needed.
(use-package mmo-mode
  :ensure nil
  :after mmix-mode)

;; `mmix-describe` is also in the same repository. We bind its functions
;; in `mmix-mode-map`.
(use-package mmix-describe
  :ensure nil
  :after mmix-mode
  :bind (:map mmix-mode-map
         ("C-h o" . mmix-describe)
         ("C-h r" . mmix-describe-registers)
         ("C-h s" . mmix-summarize-instructions)))
```
After adding this to your Emacs configuration, `mmix-mode` will be installed
automatically when you restart Emacs or evaluate the code with `C-x C-e`.

Using `mmix-mode`
==============================

Opening a MMIXAL assembler source file (extension `.mms`) will start
this mode.

* `RET` (`newline`)
  
  This key will move to a new line and insert a tab, you will be able
  to start inserting opcodes. If you wish to start at the label
  position, see the next command.

* `TAB` (`mmix-smart-tab`)

  This key will indent, complete symbol or insert tab depending on the
  current context.

* `C-j` (`electric-newline-and-maybe-indent`)

  This key will move to a new line and indent. As the line is empty
  this will start at the label position.

* `C-c C-c` (`compile`)
  
  This command compiles the current MMIX program.  The output file is
  the object file, is in the same directory and has the extension
  `.mmo`. This files can be run with `mmix-run`, see below. This files
  can be inspected, see the `mmo-mode` below.

* `` C-x ` `` (`next-error`)
  
  This command moves point to the next error. If flycheck is also
  enabled the error message will be displayed in the right fringe.

* `C-c C-r` (`mmix-run`)
  
  This command runs the current MMIX program. This command assumes
  that the program has already been compiled.

* `C-H o` (`mmix-describe`)

  This command will open a *help buffer* and will describe the current
  opcode or register of MMIX. (This is _WIP_)
  
* `C-H r` (`mmix-describe-registers`)

  This command will open a *help buffer* and will display a list
  of all possible global registers of MMIX.

Using `mmo-mode`
===================

Opening a MMIX object code file (extension `.mmo`) will start this
mode.

Click on the underlined text to go to the source or definition.

<!-- Customizing -->
<!-- ============= -->

<!-- TBD -->

Support
========

* Issues, requests and questions can go on its [issue tracker](https://github.com/ppareit/mmix-mode/issues).
* This mode is maintained at its [github page](https://github.com/ppareit/mmix-mode)
* You can support the maintainer through a [paypal donation](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=ZBVLYKWYMXQ3G)

Credits
========

`mmix-mode` was written by:

* Pieter Pareit <pieter.pareit@gmail.com>

The source code is maintained on GitHub at
<https://github.com/ppareit/mmix-mode> by Pieter Pareit
(<pieter.pareit@gmail.com>).
