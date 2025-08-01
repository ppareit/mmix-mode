;;; mmix-interactive.el --- MMIX interactive integration.  -*- lexical-binding: t; -*-

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
;; This library lets you debug MMIX programs from inside Emacs using the
;; reference simulator `mmix'.  It launches `mmix -i -l program.mmo' in a
;; comint buffer, highlights the current instruction line in the
;; corresponding .mms source and  allows breakpoints to be toggled with a
;; mouse click in the left fringe.
;;
;; Quick start (with use-package):
;;
;;   (use-package mmix-interactive
;;     :ensure nil
;;     :commands (mmix-interactive-run)
;;     :bind (:map mmix-mode-map
;;   	           ("C-c C-r" . mmix-interactive-run)))
;;
;; Then press C-cC-r in a .mms buffer after assembling to .mmo to start
;; debugging.

;;; Todo:
;;
;; Lot of litle issues, multiple source files, lock debugging mms file
;; better key bindings in mms file when in interactive run mode,
;; register viewing, memory viewing, call stack viewing, ...

;;; Code:

(require 'comint)
(require 'cl-lib)
(require 'pulse)

(eval-when-compile
  (add-to-list 'load-path
               (file-name-directory (or load-file-name ""))))
(require 'mmix-mode)
(require 'mmo-mode)

(defgroup mmix-interactive nil
  "Interactive MMIX debugger."
  :group 'tools
  :prefix "mmix-interactive-")

(defcustom mmix-interactive-executable "mmix"
  "Path to the MMIX simulator executable."
  :type 'string
  :group 'mmix-interactive)

(defvar-local mmix--source-file nil
  "Full pathname of the current MMIX source (.mms) file.")

(defvar-local mmix--running-p nil
  "Non-nil while program is executing (between prompts).")

;;;;
;;;; Address cache
;;;;

(defvar mmix--address-table (make-hash-table :test #'equal)
  "Cache mapping truename:line -> address integer.")

(defvar mmix--address-table-mtime nil
  "Modification time of the .mmo file used to build `mmix--address-table'.")

(defun mmix--build-address-table (mmo-file)
  "Parse MMO-FILE with mmotype and fill `mmix--address-table'."
  (clrhash mmix--address-table)
  (let ((re-full  (rx bol
                      (group (= 16 xdigit)) ":" (+ blank) (= 8 xdigit) (+ blank)
                      "(\"" (group (*? (not (any "\"")))) "\", line "
                      (group (+ digit)) ")"))
        (re-short (rx bol
                      (group (= 16 xdigit)) ":" (+ blank) (= 8 xdigit) (+ blank)
                      "(line " (group (+ digit)) ")"))
        (current-src nil))
    (with-temp-buffer
      (insert (shell-command-to-string
               (format "mmotype %s" (shell-quote-argument mmo-file))))
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((looking-at re-full)
          (let ((addr  (match-string 1))
                (src   (file-truename (match-string 2)))
                (line  (match-string 3)))
            (setq current-src src)
            (puthash (format "%s:%s" src line)
                     (string-to-number addr 16)
                     mmix--address-table)))
         ((looking-at re-short)
          (let ((addr (match-string 1))
                (line (match-string 2)))
            (when current-src
              (puthash (format "%s:%s" current-src line)
                       (string-to-number addr 16)
                       mmix--address-table)))))
        (forward-line 1)))
    (setq mmix--address-table-mtime
          (file-attribute-modification-time (file-attributes mmo-file)))))

(defun mmix--address-for-line (file line)
  "Return address of LINE in FILE from the cache, rebuilding if needed.

If we are unable to get an address, it means that the user is on a line
that is not an MMIX instruction, for instance a blank line or a line with
a pseudo instruction.  So we notify user here."
  (let* ((mmo (concat (file-name-sans-extension file) ".mmo"))
         (key (format "%s:%d" (file-truename file) line)))
    ;; Rebuild cache if we have none or the .mmo changed since last build.
    (when (or (null mmix--address-table-mtime)
              (time-less-p mmix--address-table-mtime
                           (file-attribute-modification-time
                            (file-attributes mmo))))
      (mmix--build-address-table mmo))
    (or (gethash key mmix--address-table)
	(error "Not an executable instruction at line %d" line))))

;;;;
;;;; Overlay arrow support
;;;;

(defvar mmix--overlay-arrow-position nil
  "Marker used by `mmix--goto-line-in-source' for the overlay arrow.")

(defun mmix--goto-line-in-source (file line)
  "Visit FILE, go to LINE, for stepping.

We show an overlay arrow there, and momentarily pulse the line."
  (when (and file line (integerp line) (> line 0))
    ;; Load the buffer without selecting it
    (let* ((buf (find-file-noselect file))
           ;; Ensure it's visible so both arrow & pulse are rendered
           (win (or (get-buffer-window buf)
                    (display-buffer buf))))
      (with-selected-window win
        ;; Move point to the requested line
        (goto-char (point-min))
        (forward-line (1- line))
        ;; Overlay arrow
        (unless mmix--overlay-arrow-position
          (setq mmix--overlay-arrow-position (make-marker)))
        (set-marker mmix--overlay-arrow-position (point))
        (setq-local overlay-arrow-position mmix--overlay-arrow-position)
        (setq-local overlay-arrow-string "=>")
        ;; Pulse highlight (momentary)
        (pulse-momentary-highlight-region
         (line-beginning-position)
         (line-end-position))))))


;;;;
;;;; Process helpers
;;;;

(defun mmix--send-console-command (cmd)
  "Send CMD plus newline to the current MMIX process."
  (when-let* ((buf (get-buffer "*MMIX-Interactive*"))
              (proc (get-buffer-process buf)))
    (with-current-buffer buf
      (goto-char (process-mark proc))
      (insert cmd "\n")
      (set-marker (process-mark proc) (point)))
    (comint-send-string proc (concat cmd "\n"))))

;;;;
;;;; Unified breakpoint and tracepoint handling
;;;;

(define-fringe-bitmap 'mmix-breakpoint
  (vector #b00111100
          #b01111110
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b01111110
          #b00111100)
  8 8 'center)

(defface mmix-breakpoint-face
  '((t :foreground "red"))
  "Face used for breakpoint fringe bitmap.")

(define-fringe-bitmap 'mmix-tracepoint
  (vector #b00011000
          #b00100100
          #b01000010
          #b10000001
          #b10000001
          #b01000010
          #b00100100
          #b00011000)
  8 8 'center)

(defface mmix-tracepoint-face
  '((t :foreground "orange"))
  "Face for MMIX tracepoints in the fringe.")

(define-fringe-bitmap 'mmix-bp-tp
  (vector #b00111000
          #b01110100
          #b11110010
          #b11110001
          #b11110001
          #b11110010
          #b01110100
          #b00111000)
  8 8 'center)

(defface mmix-bp-tp-face
  '((t :foreground "gold"))
  "Fringe-face voor gecombineerde break-/tracepoints.")

;; Marker struct
(cl-defstruct mmix-marker overlay flags addr)
;;   overlay – the overlay object at line start
;;   flags   – plist :break / :trace booleans
;;   addr    – integer address in MMIX memory

(defvar mmix--markers (make-hash-table :test #'equal)
  "Hash file:line -> `mmix-marker' table.")

(defun mmix--update-marker-visuals (mkr)
  "Refresh overlay icon and face for marker MKR, or delete if no flags."
  (let* ((flags (mmix-marker-flags mkr))
         (bmp   (cond ((and (plist-get flags :break) (plist-get flags :trace))
                       'mmix-bp-tp)
                      ((plist-get flags :break) 'mmix-breakpoint)
                      ((plist-get flags :trace) 'mmix-tracepoint)
                      (t nil)))
         (face  (cond ((and (plist-get flags :break) (plist-get flags :trace))
                       'mmix-bp-tp-face)
                      ((plist-get flags :break) 'mmix-breakpoint-face)
                      ((plist-get flags :trace) 'mmix-tracepoint-face))))
    (when (mmix-marker-overlay mkr)
      (delete-overlay (mmix-marker-overlay mkr))
      (setf (mmix-marker-overlay mkr) nil))
    (when bmp
      (let ((ov (make-overlay (line-beginning-position) (line-beginning-position))))
        (overlay-put ov 'before-string
                     (propertize " " 'display `(left-fringe ,bmp ,face)))
        (overlay-put ov 'mmix-addr   (mmix-marker-addr mkr))
        (overlay-put ov 'mmix-break  (plist-get flags :break))
        (overlay-put ov 'mmix-trace  (plist-get flags :trace))
        (setf (mmix-marker-overlay mkr) ov)))))

(defun mmix--toggle (pos type)
  "Toggle TYPE (either :break or :trace) at POS, updating UI and simulator."
  (save-excursion
    (goto-char pos)
    (let* ((file (buffer-file-name))
           (line (line-number-at-pos))
           (key  (format "%s:%d" file line))
           (mkr  (or (gethash key mmix--markers)
                     (make-mmix-marker :overlay nil
                                       :flags nil
                                       :addr (mmix--address-for-line file line))))
           (old   (plist-get (mmix-marker-flags mkr) type)))
      ;; Flip flag
      (setf (mmix-marker-flags mkr)
            (plist-put (mmix-marker-flags mkr) type (not old)))
      ;; Communicate with simulator
      (pcase (cons type old)
        (`(:break . nil)
	 (mmix--send-console-command (format "bx%x" (mmix-marker-addr mkr))))
        (`(:break . t)
	 (mmix--send-console-command (format "br%x" (mmix-marker-addr mkr))))
        (`(:trace . nil)
	 (mmix--send-console-command (format "t%x" (mmix-marker-addr mkr))))
        (`(:trace . t)
	 (mmix--send-console-command (format "u%x" (mmix-marker-addr mkr)))))
      ;; Update visuals
      (mmix--update-marker-visuals mkr)
      ;; Store or drop marker
      (if (or (plist-get (mmix-marker-flags mkr) :break)
              (plist-get (mmix-marker-flags mkr) :trace))
          (puthash key mkr mmix--markers)
        (remhash key mmix--markers))
      ;; User feedback
      (message "%s %s at %s:%d (addr %x)"
               (capitalize (symbol-name type))
               (if old "removed" "set")
               (file-name-nondirectory file)
               line
               (mmix-marker-addr mkr)))))

(defun mmix-toggle-breakpoint (&optional pos)
  "Interactively toggle a breakpoint at POS or point."
  (interactive)
  (mmix--toggle (or pos (point)) :break))

(defun mmix-toggle-tracepoint (&optional pos)
  "Interactively toggle a tracepoint at POS or point."
  (interactive)
  (mmix--toggle (or pos (point)) :trace))

(defun mmix-toggle-breakpoint-with-mouse (event)
  "Toggle a breakpoint on the line that was clicked in the left fringe.

EVENT is the mouse-click event supplied by Emacs."  (interactive "e")
  (let* ((posn  (event-start event))
         (buf   (window-buffer (posn-window posn)))
         (pos   (posn-point posn)))
    (with-current-buffer buf
      (mmix-toggle-breakpoint pos))))

(defun mmix--set-initial-markers (mms-file)
  "Resend break/trace commands for markers in MMS-FILE to simulator."
  (let ((truename (file-truename mms-file)))
    (maphash (lambda (_ mkr)
               (when (and (mmix-marker-addr mkr)
                          (mmix-marker-overlay mkr))
                 (with-current-buffer (overlay-buffer (mmix-marker-overlay mkr))
                   (when (string= (file-truename (buffer-file-name)) truename)
                     (when (plist-get (mmix-marker-flags mkr) :break)
                       (mmix--send-console-command (format "bx%x" (mmix-marker-addr mkr))))
                     (when (plist-get (mmix-marker-flags mkr) :trace)
                       (mmix--send-console-command (format "t%x" (mmix-marker-addr mkr))))))))
             mmix--markers)))

;;;;
;;;; Debugging session management
;;;;

(defun mmix-interactive-quit ()
  "Send quit command to MMIX interactive session."
  (interactive)
  (mmix--send-console-command "q"))

(defun mmix-interactive-trace-one-instruction ()
  "Trace one MMIX instruction."
  (interactive)
  (mmix--send-console-command "n"))

(defun mmix-interactive-continue ()
  "Continue MMIX simulation until halt or breakpoint."
  (interactive)
  (mmix--send-console-command "c"))

(defun mmix-interactive-show-stats ()
  "Show current MMIX simulation statistics."
  (interactive)
  (mmix--send-console-command "s"))

(defun mmix-interactive-help ()
  "Show MMIX simulator help."
  (interactive)
  (mmix--send-console-command "h"))

(defun mmix-interactive-goto-location (&optional pos)
  "Go to an MMIX memory location at POS (or current line)."
  (interactive)
  (save-excursion
    (when pos (goto-char pos))
    (when-let* ((file (buffer-file-name))
                (line (line-number-at-pos))
                (addr (mmix--address-for-line file line)))
      (mmix--send-console-command (format "@%x" addr))
      (message "Location (@) set at %s:%d (addr %x)"
               (file-name-nondirectory file) line addr))))

(defun mmix-interactive-set-text-segment ()
  "Set current segment to Text_Segment (T)."
  (interactive)
  (mmix--send-console-command "T"))

(defun mmix-interactive-set-data-segment ()
  "Set current segment to Data_Segment (D)."
  (interactive)
  (mmix--send-console-command "D"))

(defun mmix-interactive-set-pool-segment ()
  "Set current segment to Pool_Segment (P)."
  (interactive)
  (mmix--send-console-command "P"))

(defun mmix-interactive-set-stack-segment ()
  "Set current segment to Stack_Segment (S)."
  (interactive)
  (mmix--send-console-command "S"))

(defun mmix-interactive-show-breakpoints ()
  "Show all current breakpoints and tracepoints (B)."
  (interactive)
  (mmix--send-console-command "B"))

(defvar mmix-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'mmix-interactive-trace-one-instruction)
    (define-key map (kbd "SPC") #'mmix-interactive-trace-one-instruction)
    (define-key map (kbd "n") #'mmix-interactive-trace-one-instruction)
    (define-key map (kbd "c") #'mmix-interactive-continue)
    (define-key map (kbd "q") #'mmix-interactive-quit)
    (define-key map (kbd "s") #'mmix-interactive-show-stats)
    (define-key map (kbd "h") #'mmix-interactive-help)
    (define-key map (kbd "t") #'mmix-toggle-tracepoint)
    (define-key map (kbd "T") #'mmix-interactive-set-text-segment)
    (define-key map (kbd "D") #'mmix-interactive-set-data-segment)
    (define-key map (kbd "P") #'mmix-interactive-set-pool-segment)
    (define-key map (kbd "S") #'mmix-interactive-set-stack-segment)
    (define-key map (kbd "B") #'mmix-interactive-show-breakpoints)
    (define-key map (kbd "b") #'mmix-toggle-breakpoint)
    (define-key map (kbd "@") #'mmix-interactive-goto-location)
    (define-key map (kbd "g") #'mmix-interactive-goto-location)
    map))


(define-minor-mode mmix-debug-mode
  "Minor mode for MMIX source buffers during debugging."
  :init-value nil
  :lighter " Dbg"
  :keymap mmix-debug-mode-map
  (if mmix-debug-mode
      (setq-local buffer-read-only t)
    (setq-local buffer-read-only nil)
    (set-marker mmix--overlay-arrow-position nil)
    ))

(defun mmix-interactive-sentinel (proc _)
  "Cleanup after MMIX interactive process PROC terminates."
  (when-let* ((source-buf (process-get proc 'mmix-source-buffer))
	      (_ (buffer-live-p source-buf)))
    (with-current-buffer source-buf
      (when mmix-debug-mode
        (mmix-debug-mode -1)))))

;;;;
;;;; Major mode
;;;;

(defvar mmix-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    ;; convenience bindings
    map)
  "Keymap for `mmix-interactive-mode'.")

(define-derived-mode mmix-interactive-mode comint-mode "MMIX-Dbg"
  "Major mode for interacting with the MMIX simulator."
  :group 'mmix-interactive
  (setq comint-prompt-regexp "^mmix> ")
  (setq comint-use-prompt-regexp t)
  (setq comint-prompt-read-only nil)
  ;; Output filter
  (add-hook 'comint-preoutput-filter-functions #'mmix--output-filter nil t))

(defun mmix--output-filter (output)
  "Process OUTPUT from MMIX, routing trace lines and updating UI.
Returns text that should appear in the comint buffer."
  (let ((lines (split-string output "\n"))
        (result ""))
    (dolist (ln lines)
      (cond
       ;; source line indicator from `mmix -i`: "line 42: ..."
       ((string-match "^line \\([0-9]+\\):.*$" ln)
        (let ((num (string-to-number (match-string 1 ln))))
          (mmix--goto-line-in-source mmix--source-file num))
	(setq result (concat result ln "\n")))
       ;; prompt line indicates stop -> refresh registers
       ((string-match "^mmix> *$" ln)
        (setq mmix--running-p nil)
        (setq result (concat result ln)))
       ;; default: pass through
       (t (setq result (concat result ln "\n")))))
    result))

;;;;
;;;; User entry point
;;;;

;;;###autoload
(defun mmix-interactive-run (mmo-file &optional source-file)
  "Run MMIX debugger on MMO-FILE, optionally linking SOURCE-FILE (.mms)."
  (interactive
   (if (and (buffer-file-name) (string-match-p "\\.mms\\'" (buffer-file-name)))
       (list (concat (file-name-sans-extension (buffer-file-name)) ".mmo")
             (buffer-file-name))
     (list (read-file-name "MMIX object to debug: " nil nil t
                           (when buffer-file-name
                             (concat (file-name-sans-extension buffer-file-name)
				     ".mmo")))
           (when buffer-file-name buffer-file-name))))
  (let* ((buf (make-comint "MMIX-Interactive"
			   mmix-interactive-executable nil
                           "-i" "-l" mmo-file))
	 (proc (get-buffer-process buf))
         (mms-file (or source-file (concat (file-name-sans-extension mmo-file)
					   ".mms")))
         (source-buf (find-file mms-file)))
    (with-current-buffer buf
      (mmix-interactive-mode)
      (setq mmix--source-file mms-file))
    (set-process-sentinel proc #'mmix-interactive-sentinel)
    (process-put proc 'mmix-source-buffer source-buf)
    (with-current-buffer source-buf
      (mmix-debug-mode 1))
    (mmix--build-address-table mmo-file)
    (mmix--set-initial-markers mms-file)
    (display-buffer buf)))

;;;;
;;;; Source integration
;;;;

(with-eval-after-load 'mmix-mode
  ;; Mouse binding in fringe
  (define-key mmix-mode-map [left-fringe mouse-1] #'mmix-toggle-breakpoint-with-mouse)
  (define-key mmix-mode-map (kbd "C-c C-b") #'mmix-toggle-breakpoint))

(provide 'mmix-interactive)
;;; mmix-interactive.el ends here
