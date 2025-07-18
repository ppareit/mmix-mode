;;; mmix-flycheck.el --- Flychecker for MMIXAL.   -*- lexical-binding: t; -*-

;; Part of mmix-mode, see https://ppareit.github.io/mmix-mode/

;; Copyright (C) 2022 - 2025 Pieter Pareit <pieter.pareit@gmail.com>

;; Package-Requires: ((emacs "25.0") (flycheck "20250527.907"))

;;; Commentary:
;;
;;; Todo:
;;
;;; Code:
(require 'flycheck nil t)

(require 'dash)

(declare-function mmix-at-expr-p "mmix-mode")

(defun mmix-flycheck-create-undefined-symbol-errors (err)
  "Return a list of errors for each undefined symbol in ERR.

In mixal, an undefined symbol is only reported once, but the undefined symbol
might be located in multiple places in the .mms file.  We search all places and
create an error for each.

This might locate undefined symbols in comments, the correct place to fix this
is in `mmix-at-expr-p'.  If this would return false in comments, the problem
would be fixed here also."
  (when-let* ((message (flycheck-error-message err))
              (match (string-match "^undefined symbol: \\(.+\\)" message))
              (symbol (match-string 1 message)))
    (let ((errors '()))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (format "\\b%s\\b" (regexp-quote symbol)) nil t)
          (when (mmix-at-expr-p)
            (push (flycheck-error-new
		   :buffer (flycheck-error-buffer err)
                   :level (flycheck-error-level err)
                   :checker (flycheck-error-checker err)
                   :message message
                   :filename (flycheck-error-filename err)
                   :line (line-number-at-pos)
                   :column (1+ (- (point) (line-beginning-position) (length symbol)))
                   :end-column (1+ (- (point) (line-beginning-position))))
                  errors))))
      (nreverse errors))))

(defun mmix-flycheck-error-filter (errors)
  "Filter the ERRORS, might return more errors.

We use mapcan to allow going from one undefined symbol, to all places where the
undefined symbol is used.  We leave other errors as they are, but any extra
filters could be pluged in here also."
  (mapcan (lambda (err)
            (or (mmix-flycheck-create-undefined-symbol-errors err)
                (list err)))
          errors))

(eval-after-load 'flycheck
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
     (error line-start (message (one-or-more not-newline)) line-end))
    :error-filter (lambda (errors) (-> errors
				  flycheck-sanitize-errors
				  mmix-flycheck-error-filter
				  flycheck-fill-empty-line-numbers))
    :modes mmix-mode))

(add-to-list 'flycheck-checkers 'mmixal)

(provide 'mmix-flycheck)
;;; mmix-flycheck.el ends here
