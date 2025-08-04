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
(require 'seq)

(eval-when-compile
  (add-to-list 'load-path
               (file-name-directory (or load-file-name ""))))
(require 'mmix-mode)
(require 'mmo-mode)
(require 'mmix-describe)

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

(defvar-local mmix--refresh-state-on-prompt-p nil
  "Non-nil to send `s' after next prompt to refresh UI state.")

(defvar-local mmix--running-p nil
  "Non-nil while program is executing (between prompts).")

(defvar-local mmix--suppress-output-p nil
  "Non-nil to suppress output from the simulator.")

;;;;
;;;; Address cache
;;;;

(defvar mmix--address-table (make-hash-table :test #'equal)
  "Cache mapping truename:line -> address integer.")

(defvar mmix--address-to-line-info-table (make-hash-table)
  "Cache mapping address integer -> truename:line.")

(defvar mmix--address-table-mtime nil
  "Modification time of the .mmo file used to build the address caches.")

(defun mmix--build-address-table (mmo-file)
  "Parse MMO-FILE with mmotype and fill the address caches."
  (clrhash mmix--address-table)
  (clrhash mmix--address-to-line-info-table)
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
                     mmix--address-table)
            (puthash (string-to-number addr 16)
                     (format "%s:%s" src line)
                     mmix--address-to-line-info-table)))
         ((looking-at re-short)
          (let ((addr (match-string 1))
                (line (match-string 2)))
            (when current-src
              (puthash (format "%s:%s" current-src line)
                       (string-to-number addr 16)
                       mmix--address-table)
              (puthash (string-to-number addr 16)
                       (format "%s:%s" current-src line)
                       mmix--address-to-line-info-table)))))
        (forward-line 1)))
    (setq mmix--address-table-mtime
          (file-attribute-modification-time (file-attributes mmo-file)))))

(defun mmix--address-for-line (file line)
  "Return address of LINE in FILE from the cache, rebuilding if needed.

Return nil when:
 - no address corresponds to the given line (e.g., it's a blank
              line, a comment, or contains a pseudo-instruction)
 - the mmo file is non existend of out of sync"
  (let* ((mmo (concat (file-name-sans-extension file) ".mmo"))
         (key (format "%s:%d" (file-truename file) line)))
    (when (and (file-exists-p mmo)
	       (file-newer-than-file-p mmo file))
      ;; Rebuild cache if we have none or the .mmo changed since last build.
      (when (or (null mmix--address-table-mtime)
		(time-less-p mmix--address-table-mtime
                             (file-attribute-modification-time
                              (file-attributes mmo))))
	(mmix--build-address-table mmo))
      (gethash key mmix--address-table))))

(defun mmix--line-for-address (file address)
  "Return line number for ADDRESS in FILE from cache.
This is the inverse of `mmix--address-for-line'.
Rebuilds cache if needed."
  (let* ((mmo (concat (file-name-sans-extension file) ".mmo"))
         (truename (file-truename file)))
    ;; Rebuild cache if we have none or the .mmo changed since last build.
    (when (or (null mmix--address-table-mtime)
              (time-less-p mmix--address-table-mtime
                           (file-attribute-modification-time
                            (file-attributes mmo))))
      (mmix--build-address-table mmo))
    (when-let ((line-info (gethash address mmix--address-to-line-info-table)))
      (when (string-match "\\(.*\\):\\([0-9]+\\)$" line-info)
        (let ((file-from-key (match-string 1 line-info))
              (line-str (match-string 2 line-info)))
          (when (string-equal truename file-from-key)
            (string-to-number line-str)))))))

;;;;
;;;; Overlay arrow support
;;;;

(defvar mmix--overlay-arrow-position nil
  "Marker used by `mmix--show-execution-point' for the overlay arrow.")

(defvar mmix--halted-point-overlay nil
  "Overlay for the halted-at point.
This variable is made buffer-local in `mmix-debug-mode' buffers.")

(defun mmix--remove-halted-point ()
  "Remove the halted point indicator overlay, if any."
  (when (overlayp mmix--halted-point-overlay)
    (delete-overlay mmix--halted-point-overlay)
    (setq mmix--halted-point-overlay nil)))

(defun mmix--show-halted-point (file line)
  "Show halted point at LINE in FILE with a fringe rectangle."
  (when (and file line (integerp line) (> line 0))
    (let* ((buf (find-file-noselect file))
           (win (or (get-buffer-window buf) (display-buffer buf))))
      (with-selected-window win
        (setq-local overlay-arrow-position nil) ; Remove execution arrow.
        (mmix--remove-halted-point) ; Remove previous halted point overlay.
        (goto-char (point-min))
        (forward-line (1- line))
        (let ((ov (make-overlay (line-beginning-position) (line-beginning-position))))
          (overlay-put ov 'before-string
                       (propertize " " 'display '(left-fringe mmix-halted-point mmix-halted-point-face)))
          (setq-local mmix--halted-point-overlay ov))))))

(defun mmix--show-execution-point (file line)
  "Show execution point at LINE in FILE with an overlay arrow."
  (when (and file line (integerp line) (> line 0))
    ;; Load the buffer without selecting it
    (let* ((buf (find-file-noselect file))
           ;; Ensure it's visible so arrow is rendered
           (win (or (get-buffer-window buf)
                    (display-buffer buf))))
      (with-selected-window win
        (mmix--remove-halted-point)
        ;; Move point to the requested line
        (goto-char (point-min))
        (forward-line (1- line))
        ;; Overlay arrow
        (unless mmix--overlay-arrow-position
          (setq mmix--overlay-arrow-position (make-marker)))
        (set-marker mmix--overlay-arrow-position (point))
        (setq-local overlay-arrow-position mmix--overlay-arrow-position)
        (setq-local overlay-arrow-string "=>")))))

(defun mmix--pulse-line-in-source (file line)
  "Pulse LINE in FILE."
  (when (and file line (integerp line) (> line 0))
    ;; Load the buffer without selecting it
    (let* ((buf (find-file-noselect file))
           ;; Ensure it's visible so pulse is rendered
           (win (or (get-buffer-window buf)
                    (display-buffer buf))))
      (with-selected-window win
        ;; Move point to the requested line
        (goto-char (point-min))
        (forward-line (1- line))
        (pulse-momentary-highlight-region
         (line-beginning-position)
         (line-end-position))))))


;;;;
;;;; Process helpers
;;;;

(defun mmix--send-console-command (cmd &rest args)
  "Send CMD plus newline to the current MMIX process.

Accepts keyword arguments ARGS:
  :silent    — if non-nil, do not echo the command
  :refresh   — if non-nil, schedule UI refresh after command completes.

When not running :silent, we will make sure that the window is displayed."
  (let ((silent-p (plist-get args :silent))
        (refresh-p (plist-get args :refresh)))
    (when-let* ((buf (get-buffer "*MMIX-Interactive*"))
                (proc (get-buffer-process buf)))
      (when refresh-p
        (with-current-buffer buf
          (setq mmix--refresh-state-on-prompt-p t)))
      (unless silent-p
	(display-buffer buf)
        (with-current-buffer buf
          (goto-char (process-mark proc))
          (insert cmd "\n")
          (set-marker (process-mark proc) (point))))
      (comint-send-string proc (concat cmd "\n")))))

;;;;
;;;; Unified breakpoint and tracepoint handling
;;;;


;;
;; Short note on how breakpoints and tracepoints are handled:
;;
;; We use overlays at the beginning of lines, which we call "markers". A
;; marker is identified by its `mmix-marker` property.
;;
;; A marker can have `mmix-break` and/or `mmix-trace` properties to indicate
;; whether a breakpoint or tracepoint is set. The function
;; `mmix--update-marker-visuals` updates the visual representation in the
;; fringe by setting the overlay's `before-string` property to display a
;; fringe bitmap.
;;

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

(defface mmix-initial-breakpoint-face
  '((t :foreground "coral"))
  "Face for the initial MMIX breakpoint at `Main'.")

(define-fringe-bitmap 'mmix-halted-point
  (vector #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b11111111
          #b11111111
          #b11111111)
  8 8 'center)

(defface mmix-halted-point-face
  '((t :foreground "white"))
  "Face for MMIX halted point in the fringe.")

;; Overlay helpers: treat overlays-with-property 'mmix-marker as the marker set

(defun mmix--marker-at-bol (&optional pos)
  "Return the MMIX marker overlay at the beginning of line at POS, or nil.

If POS is not given, use (point)."
  (save-excursion
    (goto-char (or pos (point)))
    (let* ((bol (line-beginning-position))
           (ovs (overlays-in bol (+ bol 1))))
      (seq-find (lambda (ov) (overlay-get ov 'mmix-marker)) ovs))))

(defun mmix--new-marker-at-bol (&optional pos)
  "Create and return a new MMIX marker overlay at beginning of line at POS.

If POS is nil, use the current point.  The created overlay is set with
properties: `mmix-marker', `evaporate', and `front-advance'."
  (save-excursion
    (goto-char (or pos (point)))
    (let* ((bol (line-beginning-position))
	   (new (make-overlay bol (1+ bol))))
      (overlay-put new 'mmix-marker t)
      (overlay-put new 'evaporate t)       ;; auto-remove if the text is deleted
      (overlay-put new 'front-advance t)   ;; stick to BOL on inserts
      new)))

(defun mmix--markers-in-buffer (&optional buffer)
  "Return a list of all MMIX marker overlays in BUFFER (default current buffer)."
  (let ((buf (or buffer (current-buffer)))
        res)
    (with-current-buffer buf
      (save-restriction
        (widen)
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (overlay-get ov 'mmix-marker)
            (push ov res)))))
    res))


(defun mmix--update-marker-visuals (ov)
  "Refresh overlay icon and face for marker overlay OV, or delete if no flags."
  (let* ((initial (and ov (overlay-get ov 'mmix-initial-break)))
         (break   (and ov (overlay-get ov 'mmix-break)))
         (trace   (and ov (overlay-get ov 'mmix-trace)))
         (bmp     (cond ((and break trace) 'mmix-bp-tp)
			(break             'mmix-breakpoint)
			(initial           'mmix-breakpoint)
			(trace             'mmix-tracepoint)
			(t nil)))
         (face    (cond ((and break trace) 'mmix-bp-tp-face)
			(break             'mmix-breakpoint-face)
			(trace             'mmix-tracepoint-face)
			(initial           'mmix-initial-breakpoint-face))))
    (cond
     ((null ov) nil)
     ((and (not break) (not trace) (not initial))
      (delete-overlay ov))
     (t
      (overlay-put ov 'before-string
                   (propertize " " 'display `(left-fringe ,bmp ,face)))))))


(defun mmix--toggle (pos type)
  "Toggle TYPE (either :break or :trace) at POS.

We update UI, placing an marker in the fringe.  If the simulator is running
and we are not at an address line, we don't toggle.  If the simulator is
not running, we do toggle, but know that when the simulator is started
we might need to remove the toggle.

We also update the simulator if it is running.  Keep in mind that we place the
breakpoints one tetrabyte (4-bytes) before, from the documentation:
`bx1000' causes a break in the simulation just *after* the tetrabyte in #1000
is executed.  This is unexpected behaviour in modern debugging."
  (save-excursion
    (goto-char pos)
    (let* ((file (buffer-file-name))
           (line (line-number-at-pos))
           (bol  (line-beginning-position))
           (ov   (or (mmix--marker-at-bol bol)
                     (mmix--new-marker-at-bol bol)))
           (flag (pcase type
                   (:break 'mmix-break)
                   (:trace 'mmix-trace)))
           (old (overlay-get ov flag)))
      (when (and (not old)
		 (get-buffer-process (get-buffer "*MMIX-Interactive*"))
		 (not (mmix--address-for-line file line)))
	(error "Cannot set %s: not an executable instruciton at line %d"
	       (symbol-name type)
	       line))
      (when (overlay-get ov 'mmix-initial-break)
        (error "Simulator always breaks at `Main'"))
      ;; Flip desired state in overlay
      (overlay-put ov flag (not old))
      ;; Update visuals or delete overlay if both flags are now off
      (mmix--update-marker-visuals ov)

      ;; Best-effort talk to simulator (only if running and we can resolve)
      (let* ((addr (mmix--address-for-line file line))
             (proc (get-buffer-process (get-buffer "*MMIX-Interactive*"))))
        (when proc
          (pcase (cons type old)
            (`(:break . nil) (when addr
                               (mmix--send-console-command (format "bx%x"
								   (- addr 4)))))
            (`(:break . t)   (when addr
                               (mmix--send-console-command (format "b%x"
								   (- addr 4)))))
            (`(:trace . nil) (when addr
                               (mmix--send-console-command (format "t%x"
								   addr))))
            (`(:trace . t)   (when addr
                               (mmix--send-console-command (format "u%x"
								   addr)))))))
      ;; User feedback
      (message "%s %s at %s:%d%s"
               (capitalize (symbol-name type))
               (if old "removed" "set")
               (file-name-nondirectory file)
               line
               (if-let ((a (mmix--address-for-line file line)))
                   (format " (addr %x)" a)
                 " (pending)")))))


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

EVENT is the mouse-click event supplied by Emacs."
  (interactive "e")
  (let* ((posn  (event-start event))
         (buf   (window-buffer (posn-window posn)))
         (pos   (posn-point posn)))
    (with-current-buffer buf
      (mmix-toggle-breakpoint pos))))

(defun mmix--set-main-marker ()
  "Place a special marker on the `Main' label line.
This marker is visual only, to show where the simulator will initially
break.  It does not set a breakpoint in the simulator.  If a user-defined
marker already exists, this marker is layered on top, but the user's
marker visuals will take precedence."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^Main\\>" nil t)
      (let* ((file (buffer-file-name))
             (line (line-number-at-pos))
             (ov (or (mmix--marker-at-bol)
                     (mmix--new-marker-at-bol))))
        ;; We only set it if it corresponds to an address.
        (when (mmix--address-for-line file line)
          (overlay-put ov 'mmix-initial-break t)
          (mmix--update-marker-visuals ov))))))

(defun mmix--remove-main-marker ()
  "Remove any `mmix-initial-break' markers from the buffer."
  (dolist (ov (mmix--markers-in-buffer))
    (when (overlay-get ov 'mmix-initial-break)
      (overlay-put ov 'mmix-initial-break nil)
      (mmix--update-marker-visuals ov))))

(defun mmix--set-initial-markers ()
  "Resend break/trace commands for markers in the buffer to simulator.

Breakpoints are set one tetrabyte (4-bytes) before, from the documentation:
`bx1000' causes a break in the simulation just *after* the tetrabyte in #1000
is executed.  This is unexpected behaviour in modern debugging.

When we notice here that the line is not pointing to an instruction
we remove the overlay."
    (dolist (ov (mmix--markers-in-buffer))
      (save-excursion
        (goto-char (overlay-start ov))
        (let* ((line (line-number-at-pos))
	       (mms-file (buffer-file-name))
               (addr (mmix--address-for-line mms-file line)))
          (if addr
              (progn
                (when (overlay-get ov 'mmix-break)
                  (mmix--send-console-command (format "bx%x" (- addr 4))))
                (when (overlay-get ov 'mmix-trace)
                  (mmix--send-console-command (format "t%x" addr))))
            (overlay-put ov 'mmix-break nil)
            (overlay-put ov 'mmix-trace nil)
            (mmix--update-marker-visuals ov))))))

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
  (mmix--send-console-command "c" :refresh t))

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
      (mmix--send-console-command (format "@%x" addr) :refresh t)
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

(defun mmix--read-and-send-command (prompt help-string prefix)
  "Read command from user with help and send to MMIX process.
PROMPT is the minibuffer prompt string.
HELP-STRING is the help text to display on `?`.
PREFIX is the command prefix to ensure,
       which we also display as initial input."
  (let* ((minibuffer-help-form help-string)
         (help-event-list (cons ?? help-event-list))
         (query (read-string prompt prefix)))
    (unless (string-prefix-p prefix query)
      (setq query (format "%s%s" prefix query)))
    (mmix--send-console-command query)))

(defconst mmix-interactive--dynamic-register-help-string
  "$<n><t>   show or set dynamic register n in format t

    <t>   is ! (decimal)
          or . (floating)
          or # (hex)
          or \" (string)
          or <empty> (previous <t>)
          or =<value> (change value)"
  "Help string for the dynamic register prompt.")

(defun mmix-interactive-show-dynamic-register ()
  "Show or set a dynamic register.
With `?' shows help and reprompts."
  (interactive)
  (mmix--read-and-send-command "Query (or ?): "
                               mmix-interactive--dynamic-register-help-string
                               "$"))

(defconst mmix-interactive--local-register-help-string
  "l<n><t>   show or set local register n in format t

    <t>   is ! (decimal)
          or . (floating)
          or # (hex)
          or \" (string)
          or <empty> (previous <t>)
          or =<value> (change value)"
  "Help string for the local register prompt.")

(defun mmix-interactive-show-local-register ()
  "Show or set a local register.
With `?' shows help and reprompts."
  (interactive)
  (mmix--read-and-send-command "Query (or ?): "
                               mmix-interactive--local-register-help-string
                               "l"))

(defconst mmix-interactive--global-register-help-string
  "g<n><t>   show or set global register n in format t

    <t>   is ! (decimal)
          or . (floating)
          or # (hex)
          or \" (string)
          or <empty> (previous <t>)
          or =<value> (change value)"
  "Help string for the global register prompt.")

(defun mmix-interactive-show-global-register ()
  "Show or set a global register.
With `?' shows help and reprompts."
  (interactive)
  (mmix--read-and-send-command "Query (or ?): "
                               mmix-interactive--global-register-help-string
                               "g"))

(defvar mmix-interactive--special-register-help-string
  (let* ((base-help "r<name><t>   show or set special register <name> in format t

    <t>   is ! (decimal)
          or . (floating)
          or # (hex)
          or \" (string)
          or <empty> (previous <t>)
          or =<value> (change value)

Available special registers (detailed):
")
         (registers (seq-filter (lambda (d) (eq (mmix-description-type d) 'register))
                                (hash-table-values mmix-description-table)))
         (reg-lines (mapcar #'mmix-describe-register-1line registers)))
    (concat base-help "\n" (apply #'concat (reverse reg-lines))))
  "Help string for the special register prompt.")

(defun mmix-interactive-show-special-register ()
  "Show or set a special register.
With `?' shows help and reprompts."
  (interactive)
  (mmix--read-and-send-command "Query (or ?): "
                               mmix-interactive--special-register-help-string
                               "r"))

(defconst mmix-interactive--memory-help-string
  "M<x><t>   set and/or show memory octabyte in format t

    <t>   is ! (decimal)
          or . (floating)
          or # (hex)
          or \" (string)
          or <empty> (previous <t>)
          or =<value> (change value)"
  "Help string for the memory octabyte prompt.")

(defun mmix-interactive-show-memory ()
  "Show or set a memory octabyte.
With `?' shows help and reprompts."
  (interactive)
  (mmix--read-and-send-command "Query (or ?): "
                               mmix-interactive--memory-help-string
                               "M"))

(defconst mmix-interactive--additional-memory-help-string
  "+<n><t>   show or set n additional octabytes in format t

    <t>   is ! (decimal)
          or . (floating)
          or # (hex)
          or \" (string)
          or <empty> (previous <t>)
          or =<value> (change value)"
  "Help string for the additional octabytes prompt.")

(defun mmix-interactive-show-additional-memory ()
  "Show or set additional memory octabytes.
With `?' shows help and reprompts."
  (interactive)
  (mmix--read-and-send-command "Query (or ?): "
                               mmix-interactive--additional-memory-help-string
                               "+"))

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
    (define-key map (kbd "$") #'mmix-interactive-show-dynamic-register)
    (define-key map (kbd "g") #'mmix-interactive-show-global-register)
    (define-key map (kbd "l") #'mmix-interactive-show-local-register)
    (define-key map (kbd "r") #'mmix-interactive-show-special-register)
    (define-key map (kbd "M") #'mmix-interactive-show-memory)
    (define-key map (kbd "+") #'mmix-interactive-show-additional-memory)
    (define-key map (kbd "b") #'mmix-toggle-breakpoint)
    (define-key map (kbd "@") #'mmix-interactive-goto-location)
    map))


(define-minor-mode mmix-debug-mode
  "Minor mode for MMIX source buffers during debugging.

We manually set the `mode-name', using :lighter would be better,
but then we don't see the change."
  :init-value nil
  :keymap mmix-debug-mode-map
  (if mmix-debug-mode
      (progn
	(setq-local buffer-read-only t)
	(setq mode-name "MMIX Dbg"))
    (setq mode-name "MMIX")
    (setq-local buffer-read-only nil)
    (set-marker mmix--overlay-arrow-position nil)
    (mmix--remove-halted-point)
    (mmix--remove-main-marker)))

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

  ;; Prompt settings
  (setq comint-prompt-regexp "^mmix> ")
  (setq comint-use-prompt-regexp t)
  (setq comint-prompt-read-only nil)

  ;; Auto-scroll
  (setq-local comint-scroll-to-bottom-on-input t)
  (setq-local comint-scroll-to-bottom-on-output t)
  (setq-local comint-move-point-for-output t)
  (setq-local comint-scroll-show-maximum-output t)

  ;; Input/Output filters
  (add-hook 'comint-input-filter-functions #'mmix--input-filter nil t)
  (add-hook 'comint-preoutput-filter-functions #'mmix--output-filter nil t))

(defun mmix--input-filter (input)
  "Process INPUT to MMIX from user."
  (cond ((string-match-p "^@" input)
	 (setq mmix--refresh-state-on-prompt-p t)))
  input)

(defun mmix--output-filter (output)
  "Process OUTPUT from MMIX, routing trace lines and updating UI."
  (let ((lines (split-string output "\n"))
        (result ""))
    (dolist (ln lines)
      (let ((is-prompt (string-match-p "^mmix> *$" ln))
            (append-to-result t))
        (cond
         (is-prompt
          (setq mmix--running-p nil)
          (if mmix--refresh-state-on-prompt-p
              (progn
                (setq mmix--refresh-state-on-prompt-p nil)
                (setq mmix--suppress-output-p t)
                (setq append-to-result nil) ; Swallow this prompt
                (mmix--send-console-command "s" :silent t))
            ;; This is the prompt after 's', or a normal one.
            ;; We should stop suppressing now.
            (setq mmix--suppress-output-p nil)))
         ((string-match "^line \\([0-9]+\\):.*$" ln)
          (let ((num (string-to-number (match-string 1 ln))))
            (mmix--pulse-line-in-source mmix--source-file num)))
         ((string-match "\\(halted\\|now\\)? *at location #\\([0-9a-fA-F]+\\)" ln)
          (when-let* ((state (match-string 1 ln))
		      (addr-hex (match-string 2 ln))
                      (addr (string-to-number addr-hex 16))
                      (line (mmix--line-for-address mmix--source-file addr)))
            (pcase state
              ("halted" (mmix--show-halted-point mmix--source-file line))
              ("now" (mmix--show-execution-point mmix--source-file line))))))
        (when (and append-to-result (not mmix--suppress-output-p))
          (setq result (concat result ln (if is-prompt "" "\n"))))))
    result))

;;;;
;;;; User entry point
;;;;

;;;###autoload
(defun mmix-interactive-run ()
  "Run MMIX debugger on the .mmo file for the current .mms buffer."
  (interactive)
  (let* ((mms-file (buffer-file-name))
         (mmo-file (concat (file-name-sans-extension mms-file) ".mmo")))
    ;; Check if we have a good source and object files
    (when (not (and mms-file (string-match-p "\\.mms\\'" mms-file)))
      (error "Not a .mms file buffer"))
    (when (buffer-modified-p)
      (error "Buffer %s needs to be saved first"
  	     (file-name-nondirectory mms-file)))
    (when (not (file-exists-p mmo-file))
      (error "Object file %s does not exist, assemble first"
  	     (file-name-nondirectory mmo-file)))
    (when (file-newer-than-file-p mms-file mmo-file)
      (error "Source file %s is newer than object file %s, assemble first"
             (file-name-nondirectory mms-file)
             (file-name-nondirectory mmo-file)))
    ;; Check for existing interactive session
    (when-let* ((buf (get-buffer "*MMIX-Interactive*"))
		(proc (get-buffer-process buf))
		(_ (process-live-p proc)))
      (let ((proc-mmo-file (process-get proc 'mmix-mmo-file)))
        (if (equal mmo-file proc-mmo-file)
            (progn
              (display-buffer buf)
              (user-error "A debugging session for %s is already active"
                          (file-name-nondirectory mmo-file)))
          (unless (y-or-n-p
                   (format (concat "A debug session for %s is running."
				   "Kill it and start one for %s? ")
                           (file-name-nondirectory (or proc-mmo-file
						       "an unknown file"))
                           (file-name-nondirectory mmo-file)))
            (user-error "Aborted"))
          (delete-process proc))))
    (let* ((buf (make-comint "MMIX-Interactive"
                             mmix-interactive-executable nil
                             "-i" "-l" mmo-file))
           (proc (get-buffer-process buf))
           (source-buf (current-buffer)))
      (with-current-buffer buf
        (mmix-interactive-mode)
        (setq mmix--source-file mms-file))
      (set-process-sentinel proc #'mmix-interactive-sentinel)
      (process-put proc 'mmix-source-buffer source-buf)
      (process-put proc 'mmix-mmo-file mmo-file)
      (with-current-buffer source-buf
        (mmix-debug-mode 1))
      (mmix--build-address-table mmo-file)
      (mmix--set-initial-markers)
      (mmix--set-main-marker)
      (display-buffer buf))))

;;;;
;;;; Source integration
;;;;

(with-eval-after-load 'mmix-mode
  ;; Mouse binding in fringe
  (define-key mmix-mode-map [left-fringe mouse-1] #'mmix-toggle-breakpoint-with-mouse)
  (define-key mmix-mode-map (kbd "C-c C-b") #'mmix-toggle-breakpoint))

(provide 'mmix-interactive)
;;; mmix-interactive.el ends here
