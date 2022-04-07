;;; mmix-mode.el --- Major mode for MMIX assambly code

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
;; Created: 5 Februari 2022
;; Last modified: 5 Februari 2022
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.2"))
;; Keywords: mode mmix mmixal mix mixal knuth asm risc

;;; Commentary:
;; Use this mode for editing files in the MMIXAL language
;; as described in
;; The Art of Computer Programming, Volume 1
;; MMIX A RISC Computer for the New Millennium
;; Fascicle 1
;; By Donald E. Knuth

;;; Todo's and resources:
;; Usefull summary of instructions list:
;;       https://www-cs-faculty.stanford.edu/~knuth/mmop.html
;; We can improve on the usage of comments:
;;       https://sourceware.org/binutils/docs/as/MMIX_002dChars.html
;; We can use named registers:
;;       https://sourceware.org/binutils/docs/as/MMIX_002dRegs.html

;;; Code:

(defgroup mmix-mode nil
  "Options for `mmix-mode'."
  :group 'languages)

(defcustom mmix-mmix-program "mmix"
  "Location of the mmix program, this is the vm that runs mmix object code.
If the mmix program is available in the PATH, than this can stay
the default value.  Alternatively this can also be the full path
to the mmix executable.  This is used by `mmix-run'."
  :type 'string
  :group 'mmix-mode)

(defcustom mmix-mmixal-program "mmixal"
  "Location of the mmixal program.
The mmixal program is the assembler that converts assembly to to
object code for the mmix vm.  If the mmixal program is available
in the PATH, than this can stay the default value.  Alternatively
this can also be the full path to the mmixal executable.  This is
used by `mmix-compile-command'."
  :type 'string
  :group 'mmix-mode)

(defcustom mmix-mmixal-expand-flag nil
  "Non-nil means to expand memmory-oriented commands when assembling.
This is the `-x' option for `mmixal'.  This option expands
memory-oriented commands that cannot be assembled as single
instructions, by assembling auxiliary instructions that make temporary
use of global register $255.
This option is used in `mmix-compile-command' and
`mmixal-flycheck-error-parser'."
  :type 'boolean
  :group 'mmix-mode)

(defvar mmix-mode-syntax-table nil
  "Syntax table for `mmix-mode'.")

(unless mmix-mode-syntax-table
  (setq mmix-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?# "w")
  )

(defvar mmix-mode-abbrev-table nil
  "Abbrev table used while in `mmix-mode'.")
(define-abbrev-table 'mmix-mode-abbrev-table ())

(defvar mmix-mode-map nil
  "Keymap used in `mmix-mode' buffers.")

(unless mmix-mode-map
  (setq mmix-mode-map (make-sparse-keymap))
  (define-key mmix-mode-map "\C-c\C-c" 'compile)
  (define-key mmix-mode-map "\C-c\C-r" 'mmix-run)
  )

;(makunbound 'mmix-mode-map)

(eval-and-compile
  (defconst mmix-pseudo-ops
    '("IS" "LOC" "PREFIX" "GREG" "LOCAL" "BSPEC" "ESPEC"
      "BYTE" "WYDE" "TETRA" "OCTA")
    "Pseudo-operations of `mmix-mode'.
Pseudo-operations are operators of MMIXAL but not of MMIX.  They
provide special information about a symbolic program, without
being instructions of the program itself."))

(eval-and-compile
  (defconst mmix-ops
    '("TRAP" "FCMP" "FUN" "FEQL" "FADD" "FIX" "FSUB" "FIXU"
      "FLOT" "FLOTU" "SFLOT" "SFLOTU"
      "FMUL" "FCMPE" "FUNE" "FEQLE" "FDIV" "FSQRT" "FREM" "FINT"
      "MUL" "MULU" "DIV" "DIVU"
      "ADD" "ADDU" "SUB" "SUBU"
      "2ADDU" "4ADDU" "8ADDU" "16ADDU"
      "CMP" "CMPU" "NEG" "NEGU"
      "SL" "SLU" "SR" "SRU"
      "BN" "BZ" "BP" "BOD"
      "BNN" "BNZ" "BNP" "BEV"
      "PBN" "PBZ" "PBP" "PBOD"
      "PBNN" "PBNZ" "PBNP" "PBEV"
      "CSN" "CSZ" "CSP" "CSOD"
      "CSNN" "CSNZ" "CSNP" "CSEV"
      "ZSN" "ZSZ" "ZSP" "ZSOD"
      "ZSNN" "ZSNZ" "ZSNP" "ZSEV"
      "LDB" "LDBU" "LDW" "LDWU"
      "LDT" "LDTU" "LDO" "LDOU"
      "LDSF" "LDHT" "CSWAP" "LDUNC"
      "LDVTS" "PRELD" "PREGO" "GO"
      "STB" "STBU" "STW" "STWU"
      "STT" "STTU" "STO" "STOU"
      "STSF" "STHT" "STCO" "STUNC"
      "SYNCD" "PREST" "SYNCID" "PUSHGO"
      "OR" "ORN" "NOR" "XOR"
      "AND" "ANDN" "NAND" "NXOR"
      "BDIF" "WDIF" "TDIF" "ODIF"
      "MUX" "SADD" "MOR" "MXOR"
      "SETH" "SETMH" "SETML" "SETL" "INCH" "INCMH" "INCML" "INCL"
      "ORH" "ORMH" "ORML" "ORL" "ANDNH" "ANDNMH" "ANDNML" "ANDNL"
      "JMP" "PUSHJ" "GETA" "PUT"
      "POP" "RESUME" "UNSAVE" "SAVE" "SYNC" "SWYM" "GET" "TRIP")
    "Operation codes of `mmix-mode'.
Operation codes of MMIX, short the op codes."))

(eval-and-compile
  (defconst mmix-alias-ops
    '("SET" "LDA")
    "Alias operation codes of `mmix-mode'.
Alias operations are alternate names for MMIX operations whose
standard names are inappropriate in certain contexts. They are
handled in the same way as the `mmix-ops'."))

(eval-and-compile
  (defconst mmix-ops-and-pseudo-ops
    (append mmix-ops mmix-pseudo-ops mmix-alias-ops)
    "Ops, pseudo ops and aliases that start with a tab.
This list is needed in `mmix-indent-line' and
`mmix-completion-at-point'."))

(eval-and-compile
  (defconst mmix-globals
    '("ROUND_CURRENT" "ROUND_OFF" "ROUND_UP" "ROUND_DOWN" "ROUND_NEAR"
      "Inf"
      "Data_Segment" "Pool_Segment" "Stack_Segment"
      "D_BIT" "V_BIT" "W_BIT" "I_BIT" "O_BIT" "U_BIT" "Z_BIT" "X_BIT"
      "D_Handler" "V_Handler" "W_Handler" "I_Handler"
      "O_Handler" "U_Handler" "Z_Handler" "X_Handler"
      "StdIn" "StdOut" "StdErr"
      "TextRead" "TextWrite" "BinaryRead"
      "BinaryWrite" "BinaryReadWrite"
      "Halt"
      "Fopen" "Fclose" "Fread" "Fgets" "Fgetws"
      "Fwrite" "Fputs" "Fputws" "Fseek" "Ftell")
    "Global variables of `mmix-mode'."))

(eval-and-compile
  (defconst mmix-functions
    '("Main" ":Main")
    "Entry point of MMIX program in `mmix-mode'."))

(eval-and-compile
  (defconst mmix-local-symbols
    '("0H" "1H" "2H" "3H" "4H" "5H" "6H" "7H" "8H" "9H")
    "Local symbols in MMIXAL.
These are special symbols that can be redefined. For instance
 `2H' (2 Here) means that other parts can refer to the labels
 with `2B' (2 Backward) or `2F' (2 Forward)."))

(eval-and-compile
  (defconst mmix-label-completitions
    (append mmix-functions mmix-local-symbols)
    "Possible completitions for the label field.
The main function and the local symbols."))

(defun mmix-at-label-p ()
  "Return non-nil when point is at the label position."
  (< (current-column) 8))

(defun mmix-at-op-p ()
  "Return non-nil when point is at the opcode position."
  (< 7 (current-column) 16))

(defun mmix-at-expr-p ()
  "Return non-nil when point is at the expression position.
We are at an expression when the previous symbol is an op or
pseudo op."
  (save-excursion
    (re-search-backward (format "%s[[:blank:]]"
				(regexp-opt mmix-ops-and-pseudo-ops 'words))
			(line-beginning-position)
			t)))

(defun mmix-completion-at-point ()
  "Function to use in the hook `completion-at-point-functions'."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
	 (start (if bounds (car bounds) (point)))
	 (end (if bounds (cdr bounds) (point)))
	 (collection (cond ((mmix-at-label-p) mmix-label-completitions)
			   ((mmix-at-op-p) mmix-ops-and-pseudo-ops)
			   ((mmix-at-expr-p) mmix-globals))))
	(list start end collection . nil)))

;; Keywords for Syntax-Highlighting
(defconst mmix-font-lock-keywords
  `(("%.*\\|\*.*\\|?.*" . 'font-lock-comment-face)
    (,(regexp-opt mmix-pseudo-ops 'words) . 'font-lock-preprocessor-face)
    (,(regexp-opt mmix-alias-ops 'words) . 'font-lock-builtin-face)
    (,(regexp-opt mmix-ops 'words) . 'font-lock-builtin-face)
    (,(regexp-opt mmix-globals 'words) . 'font-lock-constant-face)
    (,(regexp-opt mmix-functions 'words) . 'font-lock-function-name-face)
   )
  "Additional expressions to highlight in MMIX mode.")

(defun mmix-indent-line ()
  "Indent current line as MMIXAL code.

The indenting for mmixal works as follows:
  * Initially indent with tab, ready to insert op or pseudo op.
  * If line starts with a label, indent starts leftmost.
  * If line starts with an op or pseudo op, start with one tab."
  (interactive)
  (if (line-empty-p)
      (indent-line-to 8)
    (if (string-match-case-sensitive-p (regexp-opt mmix-ops-and-pseudo-ops 'symbols)
				       (first-word-of-line))
	(indent-line-to 8)
      (indent-line-to 0))))

(defun string-match-case-sensitive-p (regexp string)
  "Same as `string-match-p' but the search is case-sensitive.
Return the index of start of first match for REGEXP in STRING or nil."
  (let ((case-fold-search nil))
    (string-match-p regexp string)))

(defun first-word-of-line ()
  "Return the first word of the current line."
  (save-excursion
    (beginning-of-line)
    (current-word)))

(defun previous-line-begins-in-first-column-p ()
  "Return non-nil if previous line begins in the first column."
  (save-excursion
    (forward-line -1)
    (back-to-indentation)
    (= (current-column) 0)))

(defun line-empty-p ()
  "Return non-nil if the current line is empty."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun mmix-compile-command ()
  "Create a compile command for this buffer.
This assumes that the buffer already has a name."
  (cond ((not buffer-file-name)
	 (message "Error to set compile command: %s"
		  "The file has to be saved at least once."))
	((not (executable-find mmix-mmixal-program))
	 (message "Error to set compile command: %s"
		  (format "'%s' not found, see install instructions."
		  mmix-mmixal-program))))
  (format "%s %s %s"
	  mmix-mmixal-program
	  (if mmix-mmixal-expand-flag "-x" "")
	  (shell-quote-argument buffer-file-name)))

(defun mmix-object-file-name (f-name)
  "Return the filename of the MMIX object, using F-NAME."
  (concat (file-name-sans-extension f-name) ".mmo"))

(defun mmix-run ()
  "Run the mmix program in current buffer.
This assumes that the file has already been compiled."
  (interactive)
  (cond ((not (executable-find mmix-mmix-program))
	 (message "Error to start vm: %s"
		  (format "'%s' not found, see install instructions."
			  mmix-mmix-program)))
	(t
	 (let* ((object-file-name (mmix-object-file-name buffer-file-name))
		(cmd (format "%s %s" mmix-mmix-program object-file-name)))
	   (shell-command cmd)))))

;;;###autoload
(define-derived-mode mmix-mode  prog-mode  "MMIX"
  "Major mode for editing MMIXAL assembly programs.
This mode depends on the `mmix' and `mmixal' binaries, see the
MMIX Home Page at the URL ‘http://mmix.cs.hm.edu/’."
  :group 'mmix-mode
  :syntax-table nil ;mmix-mode-syntax-table
  :abbrev-table mmix-mode-abbrev-table
  :keymap mmix-mode-map
  (setq-local comment-start "% ")
  (setq-local font-lock-defaults '(mmix-font-lock-keywords))
  (setq-local comment-column 32)
  (setq-local compile-command (mmix-compile-command))
  (setq-local indent-line-function #'mmix-indent-line)
  (setq-local tab-always-indent nil)
  (add-hook 'completion-at-point-functions
	    'mmix-completion-at-point
	    nil
	    'local))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mms" . mmix-mode))

(flycheck-define-checker mmixal
  "A mmixal syntax checker using the mmixal assembler."
  :command ("mmixal"
	    (option-flag "-x" mmix-mmixal-expand-flag)
	    source)
  :error-patterns
  ((error line-start "\"" (file-name) "\", line " line ": " (message)
	  line-end)
   (warning line-start "\"" (file-name) "\", line " line " warning: "
	    (message) line-end)
   (error line-start "\"(nofile)\", line " line "fatal error: " (message)
	  line-end)
   ;; eat away lines between parenthesis
   (line-start "(" (zero-or-more not-newline) ")" line-end)
   ;; all other erros are global errors w/o line number
   (error line-start (message) line-end))
  :error-filter
  (lambda (errors)
    (setq errors (flycheck-sanitize-errors errors))
    (dolist (err errors)
      ;; give errors w/o line number the last line number
      (unless (flycheck-error-line err)
	(setf (flycheck-error-line err)
	      (1+ (count-lines (point-min) (point-max))))))
    errors)
  :modes mmix-mode)

(add-to-list 'flycheck-checkers 'mmixal)

(provide 'mmix-mode)

;;; mmix-mode.el ends here
