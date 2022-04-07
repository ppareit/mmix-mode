;;; mmo-mode.el --- Major mode for viewing MMIX object code

;; Copyright (C) 2022 - 2022 Pieter Pareit <pieter.pareit@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Authors: Pieter Pareit <pieter.pareit@gmail.com>
;; Maintainer: Pieter Pareit <pieter.pareit@gmail.com>
;; Homepage: https://github.com/ppareit/mmix-mode/
;; Created: 13 Mars 2022
;; Last modified: 20 Mars 2022
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.2"))
;; Keywords: mode mmix mmixal mmo mix mixal knuth asm risc

;;; Commentary:
;; Use this mode for viewing MMIX object files

;;; Code:

(defcustom mmix-mmotype-program "mmotype"
  "Location of the mmotype program.
This program reads a binary `mmo' file en lists it in a human-readable form.
If the mmotype program is available in the PATH, than this can stay the default
value.  Alternatively this can also be the full path to the mmotype executable."
  :type 'string
  :group 'mmix-mode)

(defvar mmix-mmo-show-only-symbol-table nil
  "Non-nil if the buffer should only show the symbol table.")

(defvar mmix-mmo-list-also-tetrabytes nil
  "Non-nil if the buffer should also list the tetrabytes.")

(defvar mmix-mmo-mode-map nil
  "Keymap used in `mmix-mmo-mode' buffers.")

(unless mmix-mmo-mode-map
  (setq mmix-mmo-mode-map (make-sparse-keymap))
  (define-key mmix-mmo-mode-map "q" #'quit-window)
  (define-key mmix-mmo-mode-map "g" #'mmix-mmo-redisplay)
  (define-key mmix-mmo-mode-map "v" #'mmix-mmo-mode-toggle-list-also-tetrabytes)
  )

;;(makunbound 'mmix-mmo-mode-map)

(defun mmix-mmo-mode-toggle-list-also-tetrabytes ()
  "Toggle `mmix-mmo-list-also-tetrabytes' and redisplay buffer."
  (interactive)
  (let ((pos (mmix-mmo-get-point)))
    (setq mmix-mmo-list-also-tetrabytes (not mmix-mmo-list-also-tetrabytes))
    (mmix-mmo-redisplay)
    (mmix-mmo-set-point pos)
    ))

(defun mmix-mmo-at-preamble ()
  "Return non-nil when point is at the preamble."
  (if mmix-mmo-list-also-tetrabytes
      (<= (line-number-at-pos) 3)
    (= (line-number-at-pos) 1)))

(defun mmix-mmo-goto-preamble ()
  "Move point to preamble."
  (goto-char (point-min))
  (when mmix-mmo-list-also-tetrabytes
    (forward-line 2)))

(defun mmix-mmo-at-postamble ()
  "Return non-nil when point is at postamble."
  (and
   (not (mmix-mmo-at-preamble))
   (not (mmix-mmo-get-listing-line))
   (save-excursion
    (beginning-of-line)
    (search-forward-regexp "^g[[:xdigit:]]\\{3\\}: [[:xdigit:]]\\{16\\}" nil t))))

(defun mmix-mmo-goto-postabmle ()
  "Move point to postamble."
  (interactive)
  (goto-char (point-min))
  (re-search-forward "^g")
  (backward-char))

(defun mmix-mmo-get-listing-line ()
  "Return line in listing where point is at, or nil when not in listing."
  (interactive)
    (and (save-excursion (search-backward-regexp "^File was created .*\n" nil t))
	 (save-excursion
	   (search-forward-regexp "[[:xdigit:]]\\{16\\}: [[:xdigit:]]\\{8\\}.*"
				  nil t))
	 (save-excursion
	   (beginning-of-line)
	   (let ((end-point (point)))
	     (mmix-mmo-goto-preamble)
	     (1+ (count-matches "^[[:xdigit:]]\\{16\\}: [[:xdigit:]]\\{8\\}.*"
				(point) end-point))))))

(defun mmix-mmo-goto-listing-line (listing-line)
  "Move point to the LISTING-LINE line in the listing."
  (interactive)
  (mmix-mmo-goto-preamble)
  (search-forward-regexp "^[[:xdigit:]]\\{16\\}: [[:xdigit:]]\\{8\\}.*"
			 nil nil listing-line)
  (beginning-of-line))

(defun mmix-mmo-get-symbol-table-symbol ()
  "Return symbol in the symbol table.
Symbol can be
  * 'symbol-table-beginning
  * 'symbol-table-end
  * string of symbol at point
  * nil when not in symbol table"
  (interactive)
  (and
   (not (mmix-mmo-at-postamble))
   (save-excursion
     (end-of-line)
     (search-backward-regexp "^g[[:xdigit:]]\\{3\\}: [[:xdigit:]]\\{16\\}" nil t))
   (save-excursion
     (beginning-of-line)
     (cond ((search-forward-regexp
	     "^Symbol table (beginning at tetra [[:digit:]]?" nil t)
	    'symbol-table-beginning)
	   ((search-forward-regexp
	     "^[[:space:]]\\{4\\}\\([[:alpha:]]+\\) = \\(\\(#[[:xdigit:]]\\{4\\}\\)\\|\\(\$[[:xdigit:]]*\\)\\) ([[:digit:]]+)"
	     nil t)
	    (match-string-no-properties 1))
	   (t 'symbol-table-end))
	 )))

(defun mmix-mmo-goto-symbol-table-symbol (symbol)
  "Move point tot the SYMBOL.
SYMBOL can be
  * 'symbol-table-beginning
  * 'symbol-table-end
  * string: in which case it is a symbol in the symbol table."
  (interactive)
  (mmix-mmo-goto-postabmle)
  (cond ((eq symbol 'symbol-table-beginning)
	 (search-forward-regexp "^Symbol table (beginning at tetra [[:digit:]]?"))
	((stringp symbol)
	 (search-forward symbol))
	((eq symbol 'symbol-table-end)
	 (or (search-forward-regexp "^Symbol table ends at tetra [[:digit:]]?\." nil t)
	     (goto-char (point-max)))))
  (beginning-of-line))

(defun mmix-mmo-get-point ()
  "Return the point in the current object file.
The point is a pair (CAR . CDR) where
* CAR one of 'preamble, 'listing, 'postamble, 'symbol
* CDR is nil if CAR is preamble,
      is number of instruction if CAR is listing,
      is nil if CAR is postamble,
      is the string with symbol name if CAR is symbol."
  (cond ((mmix-mmo-at-preamble) '(preamble . nil))
	((mmix-mmo-at-postamble) '(postamble . nil))
	((mmix-mmo-get-listing-line) (cons 'listing (mmix-mmo-get-listing-line)))
	((mmix-mmo-get-symbol-table-symbol)
	 (cons 'symbol (mmix-mmo-get-symbol-table-symbol)))))

(defun mmix-mmo-set-point (pos)
  "Put the point at the correct position POS.
See `mmix-mmo-get-point' for the contens of POS."
  (cond ((equal (car pos) 'preamble) (mmix-mmo-goto-preamble))
	((equal (car pos) 'postamble) (mmix-mmo-goto-postabmle))
	((equal (car pos) 'listing) (mmix-mmo-goto-listing-line (cdr pos)))
	((equal (car pos) 'symbol) (mmix-mmo-goto-symbol-table-symbol (cdr pos)))))

(defun mmix-mmo-redisplay ()
  "(Re)display the MMIX object file in the buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (insert (shell-command-to-string
             (format "mmotype %s %s"
		     (if mmix-mmo-list-also-tetrabytes "-v" "")
		     (buffer-file-name))))))

(defun mmix-mmo-revert-buffer (&optional ignore-auto noconfirm)
  "Revert buffer by rerunning the `mmotype' program on the file.
Optional parameters IGNORE-AUTO and NOCONFIRM are defined as in
`revert-buffer'."
  (let ((revert-buffer-function #'revert-buffer--default))
    (revert-buffer ignore-auto noconfirm 'preserve-modes)
    (mmix-mmo-redisplay)))

;;;###autoload
(define-derived-mode mmix-mmo-mode fundamental-mode "MMIX MMO"
  "Major mode for viewing MMIX object files.

This mode depends on the binary `mmotype', see
URL `http://mmix.cs.hm.edu/bin/index.html'. The location of the
binary can be costumized with `mmix-mmotype-program'."
  :group 'mmix-mode
  :syntax-table nil
  :abbrev-table nil
  :keymap mmix-mmo-mode-map
  (setq-local buffer-read-only t)
  (setq-local revert-buffer-function #'mmix-mmo-revert-buffer)
  (setq-local buffer-auto-save-file-name nil)
  (buffer-disable-undo)
  (mmix-mmo-redisplay))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mmo" . mmix-mmo-mode))

(provide 'mmo-mode)

;;; mmo-mode.el ends here
