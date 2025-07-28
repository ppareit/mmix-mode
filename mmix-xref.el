;;; mmix-xref.el --- Xref backend for MMIX  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Pieter Pareit  <pieter.pareit@gmail.com>

;; This file is part of mmix-mode.el. See LICENSE for details.

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

;; Keywords: MMIX, debugging, tools
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;;  `mmix-xref.el' integrates Emacs Xref with `mmix-mode'.
;;
;;  What it gives you:
;;
;;    *Jump to definition*   (M-.)        -> `xref-find-definitions'
;;    *Return*               (M-,)        -> `xref-pop-marker-stack'
;;    *Find references*      (M-?)        -> `xref-find-references'
;;    *Find by regexp*       (C-M-?)      -> `xref-find-apropos'
;;
;;  These commands work automatically in any buffer visiting an *.mms*
;;  file, as soon as `mmix-mode' is loaded.  `mmix-mode' will autoload
;;  this file, so no more configuration is needed.
;;
;;  Happy hacking!

;;; Todo:
;;
;; Basic functionallity work fine, add to github issues
;;        https://github.com/ppareit/mmix-mode/issues
;; of there are any problems.
;; PREFIX support is not added, PR are appriciated.

;;; Code:

(require 'xref)
(require 'cl-lib)

(eval-when-compile
  (add-to-list 'load-path
               (file-name-directory (or load-file-name "")))
  (require 'mmix-mode))


(defconst mmix--symbolic-label-def-re
  "^\\([[:alpha:]_:][[:alnum:]_:]*\\)\\b"
  "Definition of a symbolic/global MMIX label at BOL.")

(defconst mmix--local-label-def-re
  "^[0-9]H\\b"
  "Definition of a numeric local label (0H–9H) at BOL.")

(defun mmix--normalize-identifier (id)
  "Remove optional trailing colon from identifier ID.

For example, this converts ‘Main:’ to ‘Main’."
  (if (and id (string-match "\\`\\(.*?\\):?\\'" id))
      (match-string 1 id)
    id))

(defun mmix--identifier-at-point ()
  "Return the identifier at point to use in xref.

Error when no valid identifier at point."
  (save-excursion
    (let* ((sym (thing-at-point 'symbol t)))
      (when sym
        (or (mmix--symbolic-label-p sym)
            (mmix--local-label-ref-p sym)
            (error "No match for xref: %s" sym))
        (mmix--normalize-identifier sym)))))

(defun mmix--symbolic-label-p (id)
  "Return non-nil if ID is a symbolic label.

A symbolic label consists of an optional leading colon, followed by
a letter or underscore, and then any number of letters, digits,
underscores, or colons."
  (string-match-p "\\`:?[[:alpha:]_][[:alnum:]_:]*\\'" id))

(defun mmix--local-label-ref-p (id)
  "Return non-nil if ID is a local label reference (e.g., ‘3F’, ‘3B’, ‘3H’)."
  (string-match-p "\\`[0-9][BFH]\\'" id))

(defun mmix--local-label-def-name (id)
  "Return the definition form of a local label reference ID.

For example, given ‘3F’ (forward) or ‘3B’ (backward), return ‘3H’ (here)."
  (when (string-match "\\`\\([0-9]\\)[BFH]\\'" id)
    (concat (match-string 1 id) "H")))

(defun mmix--make-xref (buf pos)
  "Return an xref pointing to BUF at POS, with the line’s text as summary."
  (with-current-buffer buf
    (save-excursion
      (goto-char pos)
      (xref-make (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))
                 (xref-make-buffer-location buf pos)))))


(defun mmix--scan-definitions (identifier &optional origin)
  "Return xref definitions for IDENTIFIER.

ORIGIN is the point where the lookup was initiated (needed for
resolving local labels like 3B/3F)."
  (let ((case-fold-search nil))
    (cond
     ;; Symbolic / global labels
     ;;       we collect *all* matching lines, but there should only be one
     ((mmix--symbolic-label-p identifier)
      (let ((defs '())
            (re (concat "^" (regexp-quote identifier) "\\b")))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (push (mmix--make-xref (current-buffer)
                                   (match-beginning 0))
                  defs)))
        (nreverse defs)))
     ;; Local numeric label refs (nB / nF / nH) pick just one target
     ((mmix--local-label-ref-p identifier)
      (let ((xref (mmix--find-single-local-def identifier (or origin (point)))))
        (when xref (list xref)))))))

(defun mmix--scan-references (identifier &optional origin)
  "Return xref references for IDENTIFIER.

If IDENTIFIER is a local numeric reference (nB / nF / nH), only the
references tied to the *specific* nH instance surrounding ORIGIN are
returned."
  (let ((case-fold-search nil)
        (id (mmix--normalize-identifier identifier)))
    (cond
     ;; Symbolic / global labels
     ((mmix--symbolic-label-p id)
      (let ((refs '())
            (re (concat "\\_<" (regexp-quote id) "\\_>")))
        (save-excursion
          (goto-char (point-min))
	  (while (re-search-forward re nil t)
	    (let* ((pos (match-end 0)))
	      (push (mmix--make-xref (current-buffer) pos) refs)))
        (nreverse refs))))
     ;;Local numeric labels
     ((mmix--local-label-ref-p id)
      (let* ((digit (substring id 0 1))
             (def-xref (mmix--find-single-local-def id (or origin (point)))))
        (when def-xref
          (let* ((def-pos (xref-location-marker
                           (xref-item-location def-xref))))
            (mmix--references-for-local-H digit def-pos))))))))

(defun mmix--apropos (pattern)
  "Find all label definitions matching PATTERN.

This function searches the current buffer for both symbolic and
local label definitions whose names match the regular expression PATTERN.
It returns a list of `xref` items, one for each definition found.

MMIX is case-sensitive, but in apropos we do a case-insensitive search as
we want as much matches as possible.  In all other searches I suggest we keep
case-sensitive."
  (let ((case-fold-search t)
        (results '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward mmix--symbolic-label-def-re nil t)
        (let ((id (match-string 1)))
          (when (string-match pattern id)
            (push (mmix--make-xref (current-buffer)
                                   (line-beginning-position))
		  results))))
      (goto-char (point-min))
      (while (re-search-forward mmix--local-label-def-re nil t)
        (let ((id (buffer-substring (match-beginning 0) (match-end 0))))
          (when (string-match pattern id)
            (push (mmix--make-xref (current-buffer)
                                   (line-beginning-position))
		  results)))))
    (nreverse results)))

(defun mmix--find-single-local-def (id origin)
  "Return the sole xref for local label reference ID at ORIGIN.

ID is of the form `nB' or `nF'.  Id could even be of the for `nH',
when we are already at the definition.  We get the following rules:
 - `nB' -> nearest previous `nH'
 - `nF' -> nearest next     `nH'
 - `nH' -> just stay here."
  (let* ((digit (substring id 0 1))
         (dir   (substring id 1 2))
         (re    (concat "^" digit "H\\b")))
    (save-excursion
      (goto-char origin)
      (cond
       ((string= dir "B")
        (when (re-search-backward re nil t)
          (mmix--make-xref (current-buffer) (line-beginning-position))))
       ((string= dir "F")
        (when (re-search-forward  re nil t)
          (mmix--make-xref (current-buffer) (line-beginning-position))))
       ((string= dir "H")
        (mmix--make-xref (current-buffer) (line-beginning-position)))))))

(defun mmix--references-for-local-H (digit def-pos)
  "Return xrefs that refer to the local label DIGIT at DEF-POS.
Includes:
  * the definition line itself (nH);
  * every nF above DEF-POS whose nearest lower DIGITH is DEF-POS;
  * every nB below DEF-POS whose nearest higher DIGITH is DEF-POS."
  (let ((case-fold-search nil)
        (refs '())
        (h-re (concat "^" digit "H\\b"))
        (f-re (concat "\\b" digit "F\\b"))
        (b-re (concat "\\b" digit "B\\b"))
        prev-h next-h)
    ;; Definition itself
    (push (mmix--make-xref (current-buffer) def-pos) refs)
    ;; Find previous and next DIGITH boundaries
    (save-excursion
      (goto-char def-pos)
      (when (re-search-backward h-re nil t)
        (setq prev-h (line-beginning-position)))
      (goto-char def-pos)
      (forward-line 1)
      (when (re-search-forward h-re nil t)
        (setq next-h (line-beginning-position))))
    ;; nF above DEF-POS
    (save-excursion
      (goto-char (or prev-h (point-min)))
      (while (re-search-forward f-re def-pos t)
        (push (mmix--make-xref (current-buffer)
                               (line-beginning-position))
	      refs)))
    ;; nB below DEF-POS
    (save-excursion
      (goto-char def-pos)
      (forward-line 1)
      (let ((limit (or next-h (point-max))))
        (while (re-search-forward b-re limit t)
          (push (mmix--make-xref (current-buffer)
                                 (line-beginning-position))
		refs))))
    (nreverse refs)))

(defun mmix--collect-label-identifiers ()
  "Return a list of *all* label identifiers defined in the current buffer."
  (let ((case-fold-search nil)
        (ids '()))
    (save-excursion
      (goto-char (point-min))
      ;; symbolic / global labels
      (while (re-search-forward mmix--symbolic-label-def-re nil t)
        (push (mmix--normalize-identifier (match-string 1)) ids))
      ;; local numeric labels
      (goto-char (point-min))
      (while (re-search-forward mmix--local-label-def-re nil t)
        (let ((def (match-string 0)))
          (push def ids))))
    (delete-dups ids)))

;; Xref Backend registration

(defun mmix-xref-backend ()
  "Return the symbol `mmix' to identify this xref backend."
  'mmix)

(add-hook 'mmix-mode-hook
          (lambda ()
            (add-hook 'xref-backend-functions #'mmix-xref-backend nil t)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'mmix)))
  "Return the MMIX label identifier at point."
  (mmix--identifier-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql 'mmix)) identifier)
  "Find definitions for MMIX label IDENTIFIER."
  (mmix--scan-definitions identifier (point)))

(cl-defmethod xref-backend-references ((_backend (eql 'mmix)) identifier)
  "Find references to MMIX label IDENTIFIER."
  (mmix--scan-references identifier (point)))

(cl-defmethod xref-backend-apropos ((_backend (eql 'mmix)) pattern)
  "Find all MMIX label definitions matching PATTERN."
  (mmix--apropos pattern))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql 'mmix)))
  "Return a completion table of all MMIX labels defined in the current buffer."
  (let ((cands (mmix--collect-label-identifiers)))
    (lambda (string pred action)
      (complete-with-action action cands string pred))))

(provide 'mmix-xref)
;;; mmix-xref.el ends here
