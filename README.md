MMIX Mode
===========

[Emacs](https://www.gnu.org/software/emacs/) major mode for editing
files in the MMIXAL assembly language. This language is described in
*The Art of Computer Programming, Volume 1, MMIX A RISC Computer for
the New Millennium, Fascicle 1* By *Donald E. Knuth*.

Features
==========

The MMIXAL assembler source files have
* font locking
* autocomplete
* flycheck support
* integrated help

The MMIX object files can be displayed.

Installing
============

I'll assume a manual installation for now. If this package becomes
available in MELPA, let me know, I'll update the installation notes.
I advice you to create a directory in your `.emacs.d` configuration,
and then clone this project there.

``` bash
mkdir ~/.emacs.d/lisp-gits/
cd ~/.emacs.d/lisp-gits/
git clone git@github.com:ppareit/mmix-mode.git
```

Dependencies
---------------

This mode depends on the MMIX binaries:

* [MMIX Home Page](http://mmix.cs.hm.edu/)
* [Linux Binaries](http://mmix.cs.hm.edu/bin/index.html)
* [Windows Executables](http://mmix.cs.hm.edu/exe/index.html)
* [MAC OSX Executables](http://mmix.cs.hm.edu/osx/index.html)

Setting up mmix-mode with use-package
---------------------------------------------

``` emacs-lisp
(use-package mmix-mode
  :load-path "/home/ppareit/.emacs.d/lisp-gits/mmix-mode/"
  :config
  (add-hook 'mmix-mode-hook #'flycheck-mode)
  (add-hook 'mmix-mode-hook
	    (lambda () (setq-local flycheck-highlighting-mode nil))))

(use-package mmo-mode)

(use-package mmix-describe
  :bind (:map mmix-mode-map
	      ("C-h o" . mmix-describe)))
```
Evaluate the different s-expressions with `C-x C-e` or restart Emacs.

Using `mmix-mode`
==============================

Opening a MMIXAL assembler source file (extension `.mms`) will start
this mode.

* `RET` (`newline`)
  
  This key will move to a new line and insert a tab, you will be able
  to start inserting opcodes. If you wish to start at the label
  position, see the next command.

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
