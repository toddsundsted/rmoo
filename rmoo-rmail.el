;;;;
;;;; Majors modes for dealing with MOO Mail
;;;;
;;;; Original Author: Ron Tapia <tapia@nmia.com>
;;;; $Author: mattcamp $
;;;; $Date: 1999/03/02 00:19:55 $
;;;; $Revision: 1.1 $

;;
;; This file gives MOO worlds a new property.
;;
;; rmoo-rmail-lists-buffer
;;

;;
;; Hook variables
;;
(defvar rmoo-rmail-list-mode-hooks nil "Hooks to run when MOO RMail List mode starts.")
(defvar rmoo-rmail-summary-mode-hooks nil "Hooks to run when MOO RMail Summary mode starts.")
(defvar rmoo-rmail-message-mode-hooks nil "Hooks to run when MOO RMail Message mode starts.")
(defvar rmoo-rmail-window-list nil)
(defvar rmoo-rmail-message-regexp "^\\([^ :]+\\): .*$")


;;
;; Keymaps
;;
(defvar rmoo-rmail-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'rmoo-rmail-get-message)
    (define-key map "l" 'rmoo-rmail-switch-to-lists-buffer)
    (define-key map "s" 'scroll-other-window)
    map)
  "Map for use with MOO RMail Summary Mode.")

(defvar rmoo-rmail-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'rmoo-rmail-list-get-mail)
    (define-key map "\^m"  'rmoo-rmail-list-get-mail-in-sequence)
    map)
  "Map for use with MOO RMail List Mode.")

(defvar rmoo-rmail-message-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Map for use with MOO RMail Message Mode.")

;;
;; Major modes
;;
(defun rmoo-rmail-list-mode ()
"Major mode for looking at MOO Recipients.

Commands:
\\{rmoo-rmail-list-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "RMOO-RMail-Lists")
  (setq major-mode 'rmoo-rmail-list-mode)
  (use-local-map (copy-keymap rmoo-rmail-list-mode-map))
  (run-hooks rmoo-rmail-list-mode-hooks))

(defun rmoo-rmail-summary-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "RMOO-RMail-Summary")
  (setq major-mode 'rmoo-rmail-summary-mode)
  (use-local-map (copy-keymap rmoo-rmail-summary-mode-map))
  (run-hooks rmoo-rmail-summary-mode-hooks))

(defun rmoo-rmail-message-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "RMOO-RMail-Message")
  (setq major-mode 'rmoo-rmail-message-mode)
  (use-local-map (copy-keymap rmoo-rmail-message-mode-map))
  (run-hooks rmoo-rmail-message-mode-hooks))

;;
;; Functions that request that the MOO send us mail thingys.
;;
(defun rmoo-rmail ()
  (interactive)
  (rmoo-send-here rmoo-rmail-get-lists-command))

(defvar rmoo-rmail-get-lists-command "@mcp-subscribed"  "Command to download a list of MOO mail recipients.")
(defvar rmoo-rmail-list-get-mail-command "@mcp-mail")
(defvar rmoo-rmail-get-message-command "@mcp-get-message")

(defun rmoo-rmail-get-message ()
  (interactive)
  (let ((message (rmoo-rmail-find-message)))
    (rmoo-send-here (concat rmoo-rmail-get-message-command
			   " " 
			   message
			   " on "
			   rmoo-recipient))))

(defun rmoo-rmail-list-get-mail ()
  (interactive)
  (let ((rmoo-list (rmoo-rmail-list-find-list)))
    (rmoo-send-here (concat rmoo-rmail-list-get-mail-command
			   " new on "
			   rmoo-list))))

(defun rmoo-rmail-list-get-mail-in-sequence (seq)
  (interactive (list (rmoo-rmail-request-sequence)))
  (let ((rmoo-list (rmoo-rmail-list-find-list)))
    (rmoo-send-here (concat rmoo-rmail-list-get-mail-command
			   " "
			   seq
			   " on "
			   rmoo-list))))


;;;;
;;;; Interface with RMOO-MCP
;;;;

;;
;; Register the request types
;;
(rmoo-mcp-register "display-mail-lists" t t
		  '()
		  'rmoo-rmail-display-lists
		  "rmoo-rmail") 

(rmoo-mcp-register "display-mail-summary" t t
		  '(("list" . 'required))
		  'rmoo-rmail-display-summary
		  "rmoo-rmail")

(rmoo-mcp-register "display-mail-message" t t
		  '(("list" . 'required)
		    ("message-number" . 'required))
		  'rmoo-rmail-display-message
		  "rmoo-rmail")

;;
;; Functions called when the appropriate request comes through.
;;
(defvar rmoo-recipient "")
(make-variable-buffer-local 'rmoo-recipient)

(defun rmoo-rmail-display-summary (list-name)
  (let ((buf (current-buffer))
	(world rmoo-world-here))
    (set-buffer (get-buffer-create (concat "Summary For "
					   list-name)))
    (if buffer-read-only
	(toggle-read-only))
    (setq rmoo-world-here world)
    (rmoo-take-control-of-output 'rmoo-mcp-output-function)
    (put world 'output-buffer (current-buffer))
    (put world 'last-output-buffer buf)
    (erase-buffer)
    (put world 'goto-buffer (current-buffer))
    (setq rmoo-recipient list-name)
    (setq rmoo-mcp-cleanup-function 'rmoo-rmail-display-summary-cleanup)
    (set-buffer buf)))

(defun rmoo-rmail-display-message (list-name message-number)
  (let ((buf (current-buffer))
	(world rmoo-world-here))
    (set-buffer (get-buffer-create (concat "Msg: "
					   message-number
					   " on "
					   list-name)))
    (if buffer-read-only
	(toggle-read-only))
    (setq rmoo-world-here world)
    (rmoo-take-control-of-output 'rmoo-mcp-output-function)
    (put world 'output-buffer (current-buffer))
    (put world 'last-output-buffer buf)
    (erase-buffer)
    (setq rmoo-recipient list-name)
    (setq rmoo-mcp-cleanup-function 'rmoo-rmail-display-message-cleanup)
    (set-buffer buf)))

(defun rmoo-rmail-display-lists ()
  (let ((buf (current-buffer))
	(world rmoo-world-here))
    (set-buffer (get-buffer-create (concat "Lists on "
					   (symbol-name world))))
    (put world 'rmoo-rmail-lists-buffer (current-buffer))
    (if buffer-read-only
	(toggle-read-only))
    (setq rmoo-world-here world)
    (rmoo-take-control-of-output 'rmoo-mcp-output-function)
    (put world 'output-buffer (current-buffer))
    (put world 'last-output-buffer buf)
    (erase-buffer)
    (setq rmoo-mcp-cleanup-function 'rmoo-rmail-display-lists-cleanup)
    (set-buffer buf)))


;;
;; Cleanup functions
;;
(defun rmoo-rmail-display-lists-cleanup ()
  (let ((world rmoo-world-here))
    (rmoo-rmail-list-mode)
    (setq rmoo-world-here world)
    (setq  buffer-read-only t)
    (put world 'goto-buffer (current-buffer))
    (if (string= window-system "x")
	(put world 'goto-function 'switch-to-buffer-other-frame)
      (put world 'goto-function 'switch-to-buffer-other-window))
    (setq rmoo-rmail-window-list (cons (list rmoo-world-here
					    (get-buffer-window 
					     (current-buffer)))
				      rmoo-rmail-window-list))))

(defun rmoo-rmail-display-summary-cleanup ()
  (let ((world rmoo-world-here)
	(recip rmoo-recipient))
    (rmoo-rmail-summary-mode)
    (setq rmoo-world-here world)
    (setq rmoo-recipient recip)
    (setq buffer-read-only t)
    (put rmoo-world-here 'goto-function 'rmoo-rmail-summary-goto-function)
    (put world 'goto-buffer (current-buffer))))
    

(defun rmoo-rmail-summary-goto-function (buf)
  (switch-to-buffer buf)
  (set-buffer buf)
  (message (concat "Current buffer: " (prin1-to-string (current-buffer)))))


(defun rmoo-rmail-display-message-cleanup ()
  (let ((world rmoo-world-here)
	(recip rmoo-recipient))
    (rmoo-rmail-message-mode)
    (setq rmoo-world-here world)
    (setq rmoo-recipient recip)
    (setq buffer-read-only t)
    (display-buffer (current-buffer) t)))

;;
;; Some utility functions
;;
(defun rmoo-rmail-request-sequence ()
  (read-string "Message sequence: "))

(defvar rmoo-rmail-list-regexp "^ *\\([^ ]+\\).*$")

(defun rmoo-rmail-list-find-list ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at rmoo-rmail-list-regexp)
	(buffer-substring (match-beginning 1) (match-end 1))
      "me")))
  
(defun rmoo-rmail-find-message ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at rmoo-rmail-message-regexp)
	(buffer-substring (match-beginning 1) (match-end 1))
      "$")))

(defun rmoo-rmail-switch-to-lists-buffer ()
  (interactive)
  (switch-to-buffer (get rmoo-world-here 'rmoo-rmail-lists-buffer)))

;;;;
;;;; $Log: rmoo-rmail.el,v $
;;;; Revision 1.1  1999/03/02 00:19:55  mattcamp
;;;; Initial revision
;;;;
