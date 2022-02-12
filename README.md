MMIX Mode
===========

[Emacs](https://www.gnu.org/software/emacs/) major mode for editing
files in the MMIXAL assembly language. This language is described in
*The Art of Computer Programming, Volume 1, MMIX A RISC Computer for
the New Millennium, Fascicle 1* By *Donald E. Knuth*.

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
  :load-path "/home/ppareit/.emacs.d/lisp-gits/mmix-mode/")
```
Evaluate with `C-x C-e` or restart Emacs.

Using `mmix-mode`
==============================

* opening a `.mms` file will start this mode.

* `C-j` (`electric-newline-and-maybe-indent`)

  This key will move to a new line and insert a tab.

* `C-c C-c` (`compile`)
  
  This command compiles the current MMIX program.  The output file
  is in the same directory and has the extension `.mmo`.

<!-- * `` C-x ` `` (`next-error`) -->

<!--   TBD -->

* `C-c C-r` (`mmix-run`)
  
  This command runs the current MMIX program. This command assumes
  that the program has already been compiled.


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
