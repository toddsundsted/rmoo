;;; rmoo.el --- a major mode for interacting with MOOs.
;;
;; Original Author: Ron Tapia <tapia@nmia.com>
;; $Author: mattcampbell $
;; $Date: 2000/10/18 21:33:39 $
;; $Revision: 1.12 $
;;
;; Pre-RCS modifications by Matthew Campbell <mattcampbell@pobox.com>:
;; Fixed a bug in rmoo-handle-text that prevented rmoo-handle-text-hooks
;; from being run correctly.  Also modified rmoo-quit to provide reasonable
;; feedback after the user confirms his choice to disconnect.
;;
(provide 'rmoo)
(require 'cl)
(require 'comint) ; All that's needed from comint is comint-read-noecho. 

;;
;; Most of the global variables.
;;
(defvar rmoo-world-here nil "The moo world associated with this buffer.")
(make-variable-buffer-local 'rmoo-world-here)
(defvar rmoo-prompt ">" "The prompt to use for this moo.\nTaken from the prompt property of a moo world.")
(make-variable-buffer-local 'rmoo-prompt)
(defvar rmoo-handle-text-hooks nil "A list of hooks run every time that output from a MOO is entered in a moo interactive buffer.")
(defvar rmoo-interactive-mode-syntax-table nil "Syntax table for use with MOO interactive mode.")
(defvar rmoo-interactive-mode-hooks nil "A list of hooks run when a buffer changes to MOO interactive mode.")
(defvar rmoo-autologin t "If a world has a login property, use it to automatically connect when rmoo is run.")
(defvar rmoo-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'rmoo-send)
    (define-key map "\^a" 'rmoo-beginning-of-line)
    (define-key map "\^i" 'dabbrev-expand)
    (define-key map "\^c\^q" 'rmoo-quit)
    (define-key map "\^c\^y" 'rmoo-send-kill)
    (define-key map "\^c\^w" 'rmoo-worlds-map)
    map)
  "Keymap for MOO Interactive Mode.")

(defvar rmoo-setup-done nil
  "Indicates whether rmoo-setup has executed yet.")

;;;###autoload
(defun rmoo (world)
  "Opens a connection to a MOO and sets up a rmoo-interactive-mode buffer."
  (interactive (list (rmoo-request-world)))
  (unless rmoo-setup-done
    (rmoo-setup))
  (let ((buf-name (concat "*" (symbol-name world) "*")))
    (if (memq (get-buffer-process buf-name) (process-list))
	(switch-to-buffer buf-name)
      (let* ((buf (get-buffer-create buf-name))
	     (site (rmoo-request-site-maybe world))
	     (port (rmoo-request-port-maybe world))
	     (login (rmoo-request-login-maybe world))
	     (passwd (rmoo-request-passwd-maybe world))
	     (proc (if (string-match "^[.0-9]+$" site)
		       (start-process site buf "telnet" site 
				      (int-to-string port))
		     (open-network-stream site buf site port))))
	(put world 'process proc)
	(or (get world 'output-function)
	    (put world 'output-function 'rmoo-handle-text))
	(process-kill-without-query proc t)
	(if (and (not (string= login ""))
		 rmoo-autologin)
	    (rmoo-send-string (concat "connect "
				      login " "
				      passwd)
			      proc))
	(switch-to-buffer buf-name)
	(put world 'output-buffer (current-buffer))
	(rmoo-interactive-mode world)))))

(defun rmoo-interactive-mode (world)
  "Major mode for talking to inferior MOO processes.

Commands:
\\{rmoo-interactive-mode-map}

Hooks:
  rmoo-interactive-mode-hooks
  rmoo-handle-text-hooks

Abnormal hook:
  rmoo-handle-text-redirect-functions
  rmoo-send-hooks

Keymap:
  rmoo-interactive-mode-map"

  (interactive (rmoo-request-world))
  (if (null (get world 'process))
      (error "No process"))
  (let ((b (buffer-string)))
    (kill-all-local-variables)
    (insert b))
  (setq rmoo-world-here world)
  (set-process-filter (get world 'process) 'rmoo-filter)
  (setq mode-name "RMOO")
  (setq major-mode 'rmoo-interactive-mode)
  (setq fill-column (1- (window-width)))
  (if (null rmoo-interactive-mode-syntax-table)
      (progn
	(setq rmoo-interactive-mode-syntax-table (make-syntax-table))
	(set-syntax-table rmoo-interactive-mode-syntax-table)
	(modify-syntax-entry ?\[ "(]")
	(modify-syntax-entry ?\] ")[")
	(modify-syntax-entry ?# "w")))
  (use-local-map (copy-keymap rmoo-interactive-mode-map))
  (newline)
  (goto-char (point-max))
  (setq rmoo-prompt (or (get rmoo-world-here 'prompt) rmoo-prompt))
  (set-marker (process-mark proc) (point))
  (insert rmoo-prompt)
  (run-hooks 'rmoo-interactive-mode-hooks))

;;
;; rmoo-filter doesn't run any hooks, but rmoo-handle-text does. 
;;
(defun rmoo-filter (proc string)
  (let ((old-buf (current-buffer))
	goto-func goto-buf)
	(unwind-protect
	    (let (moving)
	      (set-buffer (process-buffer proc))
	      (setq moving (= (point) (process-mark proc)))
	      (save-excursion
		(let ((rmoo-output
		       (rmoo-string-to-list (concat (get rmoo-world-here
							'pending-output)
						   string)))
		      line)
		  (put rmoo-world-here 'pending-output (car rmoo-output))
		  (setq rmoo-output (cdr rmoo-output))
		  (while rmoo-output
		    (set-buffer (process-buffer proc))
		    (setq line (car rmoo-output))
		    (setq rmoo-output (cdr rmoo-output))
		    (funcall (get rmoo-world-here 'output-function) line))))
	      (if moving (goto-char (process-mark proc))))
	  (set-buffer old-buf))
	;;
	;; This needs to be cleaned up.
	;;
	(if (fboundp (setq goto-func (get rmoo-world-here 'goto-function)))
	    (progn
	      (funcall goto-func
		       (get rmoo-world-here 'goto-buffer))
	      (put rmoo-world-here 'goto-function nil)
	      (put rmoo-world-here 'goto-buffer nil)))))
	
;;
;; Note that a moo output-function can always expect to be called in
;; the process buffer. 
;;

;;
;; If an output function wants to take control from whatever
;; is handling the output currently, it can call:
;;
(defun rmoo-take-control-of-output (new-func)
  (put rmoo-world-here 'last-output-function
       (get rmoo-world-here 'output-function))
  (put rmoo-world-here 'output-function new-func))

;;
;; If an output function wants to hand control back the previous
;; output function, it can call:
(defun rmoo-output-function-return-control-to-last ()
  (interactive)
  (let ((last-func (get rmoo-world-here 'last-output-function)))
    (put rmoo-world-here 'last-output-function
	 (get rmoo-world-here 'output-function))
    (put rmoo-world-here 'output-function last-func)))

;;
;; If an output function wants to reset the output buffer
;; it can call one of:
;;
(defun rmoo-set-output-buffer-to-last ()
  (interactive)
  (let ((last-buf (get rmoo-world-here 'last-output-buffer)))
    (put rmoo-world-here 'last-output-buffer
	 (get rmoo-world-here 'output-buffer))
    (put rmoo-world-here 'output-buffer last-buf)))

(defun rmoo-set-output-buffer-to (buf)
  (put rmoo-world-here 'last-output-buffer
       (get rmoo-world-here 'output-buffer))
  (put rmoo-world-here 'output-buffer buf))

;;
;; By default rmoo.el just inserts lines of text in the process buffer.
;; More intelligent functions can be put in the MOO's output-function
;; property. If the default behavior suffices most of the time but you'd
;; like your function called on certain lines, put a matching function in
;; rmoo-handle-text-redirect-functions. 
;;
(defvar rmoo-handle-text-redirect-functions nil "Functions called on each line of output. Each function in this list should take one argument, a string, and return a function (which is called on the line) or nil.")

(defun rmoo-handle-text-need-new-output-function (line)
  (let ((funcs rmoo-handle-text-redirect-functions)
	func
	rfunc)
    (while (and funcs (not rfunc))
      (setq func (car funcs))
      (setq funcs (cdr funcs))
      (setq rfunc (funcall func line)))
    rfunc))

;;
;; rmoo-handle-text checks to see if anybody wants to handle a line passed
;; to it, if someone does, it passes the line. If not, the line gets
;; inserted in the current buffer which is presumably the process buffer
;; of a MOO process.
;;
(defun rmoo-handle-text (line)
  (let (new-handler)
    (if (setq new-handler (rmoo-handle-text-need-new-output-function line))
	(apply new-handler (list line))
      (let (start end)
	(setq start (goto-char (marker-position
				(process-mark 
				 (get rmoo-world-here 'process)))))
	(insert-before-markers (concat line "\n"))
	(save-restriction
	  (narrow-to-region start (point))
	  (goto-char start)
	  (run-hooks 'rmoo-handle-text-hooks)
	  (rmoo-recenter))))))

;;
;; rmoo-handle-text doesn't automatically give control to a new handler
;; if a handler wants to control the output, it should call
;; something like rmoo-take-control-of-output
;;


;;
;; MOO worlds. 
;;
;;
;; Some properties of moo worlds:
;;
;;                  login
;;                  password
;;                  site
;;                  port
;;                  process
;;                  pending-output
;;                  output-buffer
;;                  output-function
;;                  last-output-buffer
;;                  last-output-function
;;                  goto-buffer
;;

(defvar rmoo-worlds-max-worlds 100 "The maximum number of MOO's")
(defvar rmoo-worlds (make-vector rmoo-worlds-max-worlds 0 ))
(defvar rmoo-worlds-add-rmoo-functions nil "A list of functions run every time that a MOO world is added. Each function in this list should take a single argument, a rmoo-world.")
(defvar rmoo-worlds-properties-to-save '(login passwd site port))
(defvar rmoo-worlds-file (expand-file-name "~/.rmoo_worlds") "The name of a file containg MOO worlds.")
(defvar rmoo-worlds-map (make-sparse-keymap) "MOO worlds keymap")

(define-key rmoo-interactive-mode-map "\C-c\C-w" rmoo-worlds-map)
(define-key rmoo-worlds-map "\C-a" 'rmoo-worlds-add-new-moo)
(define-key rmoo-worlds-map "\C-s" 'rmoo-worlds-save-worlds-to-file)

(defun rmoo-worlds-add-moo (world-name &rest pairs)
  (setq world (intern world-name rmoo-worlds))
  (let (pair prop)
    (while pairs
      (setq pair (car pairs))
      (setq pairs (cdr pairs))
      (put world (intern (car pair)) (car (cdr pair)))))
  (let ((funcs rmoo-worlds-add-rmoo-functions)
	func)
    (while funcs
      (setq func (car funcs))
      (setq funcs (cdr funcs))
      (funcall func world))))

;;;###autoload
(defun rmoo-worlds-add-new-moo (name site port)
  (interactive "sWorld name: \nsSite: \nnPort: ")
  (rmoo-worlds-add-moo name (list "site" site) (list "port" port)))

(defun rmoo-worlds-save-worlds-to-file ()
  "Save rmoo-world-here's worlds to rmoo-worlds-file if it's not \"\". Otherwise, prompt for a file name and save there."
  (interactive)
  (let* ((world rmoo-world-here)
	 (file rmoo-worlds-file)
	 buf)
    (if (string= file "")
	(setq file (read-file-name "File: ")))
    (save-excursion
      (setq buf (get-buffer-create (generate-new-buffer-name (concat "*"
								     file
								     "*"))))
      (set-buffer buf)
      (insert ";;\n;; MOO Worlds\n;;\n")
      (mapatoms 'rmoo-worlds-insert-world rmoo-worlds)
      (write-file file)
      (kill-buffer nil))))

(defun rmoo-worlds-insert-world (world)
  (let ((props rmoo-worlds-properties-to-save)
	prop)
    (insert "(rmoo-worlds-add-moo " (prin1-to-string (symbol-name world)))
    (while props
      (setq prop (car props))
      (setq props (cdr props))
      (insert "\n        '(" (prin1-to-string (symbol-name prop))
	      " "
	      (prin1-to-string (get world prop))
	      ")"))
    (insert ")\n\n")))

;;
;; Load world-specific lisp file.
;;
(defun rmoo-init-hook ()
  (let (file)
    (if (setq file (get rmoo-world-here 'world-init-file))
	(load-file (expand-file-name file)))))

;;
;; Make sure that world init files get saved.
;;
(setq rmoo-worlds-properties-to-save
      (append rmoo-worlds-properties-to-save '(world-init-file)))

;;
;; Run rmoo-init-hook whenever rmoo-interactive-mode starts.
;;
(add-hook 'rmoo-interactive-mode-hooks 'rmoo-init-hook)

;;;
;;; Input History Maintenance
;;;
(defvar rmoo-input-history-size 30
  "The number of past input commands remembered for possible reuse")

(defvar rmoo-input-history nil)

(defvar rmoo-input-index 0)

(defun rmoo-make-history (size)
  ;; (head tail . vector)
  ;; head is the index of the most recent item in the history.
  ;; tail is the index one past the oldest item
  ;; if head == tail, the history is empty
  ;; all index arithmetic is mod the size of the vector
  (cons 0 (cons 0 (make-vector (+ size 1) nil))))

(defun rmoo-decr-mod (n m)
  (if (= n 0)
      (1- m)
    (1- n)))

(defun rmoo-history-insert (history element)
  (let* ((head (car history))
	 (tail (car (cdr history)))
	 (vec (cdr (cdr history)))
	 (size (length vec))
	 (new-head (rmoo-decr-mod head size)))
    (aset vec new-head element)
    (setcar history new-head)
    (if (= new-head tail)  ; history is full, so forget oldest element
	(setcar (cdr history) (rmoo-decr-mod tail size)))))

(defun rmoo-history-empty-p (history)
  (= (car history) (car (cdr history))))

(defun rmoo-history-ref (history index)
  (let* ((head (car history))
	 (tail (car (cdr history)))
	 (vec (cdr (cdr history)))
	 (size (if (<= head tail)
		   (- tail head)
		 (+ tail (- (length vec) head)))))
    (if (= size 0)
	(error "Ref of an empty history")
      (let ((i (% index size)))
	(if (< i 0)
	    (setq i (+ i size)))
	(aref vec (% (+ head i) (length vec)))))))

(defun rmoo-initialize-input-history ()
  (make-local-variable 'rmoo-input-history)
  (make-local-variable 'rmoo-input-index)
  (setq rmoo-input-history (rmoo-make-history rmoo-input-history-size))
  (setq rmoo-input-index 0))

(defun rmoo-remember-input (string)
  (if (not (string= string ""))
      (rmoo-history-insert rmoo-input-history string)))

(defun rmoo-previous-command ()
  (interactive)
  (rmoo-browse-input-history 1))

(defun rmoo-next-command ()
  (interactive)
  (rmoo-browse-input-history -1))

(defun rmoo-browse-input-history (delta)
  (cond ((rmoo-history-empty-p rmoo-input-history)
	 (error "You haven't typed any commands yet!"))
	((eq last-command 'rmoo-browse-input-history)
	 (setq rmoo-input-index (+ rmoo-input-index delta)))
	((save-excursion (eq (rmoo-find-input) (point)))
	 (setq rmoo-input-index 0))
	(t
	 (ding)
	 (message "Press %s again to erase line."
		  (key-description (this-command-keys)))
	 (setq delta nil)))
  (setq this-command 'rmoo-browse-input-history)
  (if delta
      (let ((end (rmoo-find-input)))
	(delete-region (point) end)
	(insert (rmoo-history-ref rmoo-input-history rmoo-input-index)))))
  
(defun rmoo-match-input-history (delta)
  (message (prin1-to-string last-command))
  (cond ((rmoo-history-empty-p rmoo-input-history)
	 (error "You haven't typed any commands yet!"))
	((eq last-command 'rmoo-match-input-history)
	 (setq rmoo-input-index (+ rmoo-input-index delta)))
	(t
	 (setq rmoo-input-index 0)))
  (setq this-command 'rmoo-match-input-history)
  (let* ((str (concat "^"
		      (regexp-quote (save-excursion 
				      (buffer-substring (rmoo-find-input)
							(point))))))
	 (tail (nth 1 rmoo-input-history))
	 (vec (nthcdr 2 rmoo-input-history))
	 (size (length vec))
	 (found-match nil))
    (while (not (or (eq rmoo-input-index 
			(+ rmoo-input-index (* delta size)))
		    found-match))
      (if (string-match str (rmoo-history-ref rmoo-input-history
					     rmoo-input-index))
	  (progn
	    (setq found-match t)
	    (delete-region (rmoo-find-input) (point))
	    (insert (rmoo-history-ref rmoo-input-history rmoo-input-index)))
	(setq rmoo-input-index (+ rmoo-input-index delta))))
    (if (not found-match)
	(error "No match in input history."))))

(defun rmoo-previous-matching-command ()
  (interactive)
  (rmoo-match-input-history -1))

(add-hook 'rmoo-interactive-mode-hooks 'rmoo-initialize-input-history)
(add-hook 'rmoo-send-functions 'rmoo-remember-input)

(define-key rmoo-interactive-mode-map "\ep" 'rmoo-previous-command)
(define-key rmoo-interactive-mode-map "\en" 'rmoo-next-command)

;;
;; Various reading functions.
;;				    
(defun rmoo-request-world ()
  (intern (completing-read "MOO world: " rmoo-worlds nil t) rmoo-worlds))

(defun rmoo-request-site-maybe (world)
  (or (get world 'site)
      (read-string "Site: ")))

(defun rmoo-request-port-maybe (world)
  (or (get world 'port)
      (string-to-int (read-string "Port: "))))

(defun rmoo-request-login-maybe (world)
  (or (get world 'login)
      (read-string "Login as: ")))

(defun rmoo-request-passwd-maybe (world)
  (or (get world 'passwd)
      (comint-read-noecho "Password: " t)))

;;
;; RMOO setup
;;

(defvar rmoo-libraries
	(list
	"rmoo-mcp"
	"prefix"
	"rmoo-code"
	"rmoo-text"
	"rmoo-mail"
	"rmoo-objects"
	"rmoo-local-edit"
	"rmoo-extras"))

(defvar rmoo-x-libraries
      (list
       "rmoo-display-jtext"
       "rmoo-rmail"
       "rmoo-menus"))

;;;###autoload
(defun rmoo-setup ()
  (mapcar 'load-library rmoo-libraries)
  (if (string= window-system "x")
      (mapcar 'load-library rmoo-x-libraries))
  (if (featurep 'emacspeak)
      (load-library "emacspeak-rmoo"))
  (setq rmoo-setup-done t))

(if (file-exists-p (expand-file-name rmoo-worlds-file))
    (load-file (expand-file-name rmoo-worlds-file)))

;;
;;
;; Various utility functions:
;;
;;
(defun rmoo-send-string (string proc)
  "Send STRING as input to PROC"
  (comint-send-string proc (concat string "\n")))

(defun rmoo-eobp ()
  (cond ((eobp)
	 t)
	((looking-at ".*\\S-")
	 nil)
	(t
	 (forward-line)
	 (rmoo-eobp))))

(defun rmoo-beginning-of-line ()
  "Move point to beginning-of-line, but after prompt character."
  (interactive)
  (beginning-of-line 1)
  (if (looking-at rmoo-prompt)
      (forward-char (length rmoo-prompt))))

(defun rmoo-find-input ()
  "Move point to rmoo-beginning-of-line, and return end-of-line."
  (end-of-line 1)
  (prog1
      (point)
    (rmoo-beginning-of-line)))

(defun rmoo-beginning-of-line ()
  "Move point to beginning-of-line, but after prompt character."
  (interactive)
  (beginning-of-line 1)
  (if (looking-at rmoo-prompt)
      (forward-char (length rmoo-prompt))))

(defvar rmoo-send-functions nil "A list of functions called everytime a line of input is send to a MOO process as a command in rmoo-interactive-mode. Each function is called with one argument, the line to be sent.")

(defvar rmoo-send-always-goto-end nil
  "Indicates that RMOO should always go to the buffer after sending a line,
no matter where in the buffer the user was.")

(defvar rmoo-send-require-last-line nil
  "Indicates that RMOO should refuse to send what you type if you are not
on the last line of the buffer.")

(defun rmoo-send ()
  "Send current line of input to a MOO (rmoo-world-here)."
  (interactive)
  (if (and rmoo-send-require-last-line
   (not (save-excursion
     (end-of-line)
     (rmoo-eobp))))
    (progn
      (ding)
      (message "Must be on the last lien of the buffer."))
    (progn
      (let ((proc (get rmoo-world-here 'process)))
        (cond ((and proc (memq (process-status proc) '(open run)))
    	   ;; process exists, send line
    	   (let* ((origin (point))
    		  (end (rmoo-find-input))
    		  (line (buffer-substring (point) end))
		  (funcs rmoo-send-functions)
		  func)
	     (rmoo-send-here line)
	     (cond ((save-excursion
		      (end-of-line)
		      (or rmoo-send-always-goto-end (rmoo-eobp)))
		    (goto-char (point-max))
		    (insert ?\n)
		    (move-marker (process-mark proc) (point))
		    (insert rmoo-prompt)
		    (if (= scroll-step 1)
			(recenter -1)))
		   (t
		    (message "Sent line \"%s\"" line)
		    (goto-char origin)))
	     (while funcs
	       (setq func (car funcs))
	       (setq funcs (cdr funcs))
	       (funcall func line))))
	     
	  (t
	   (message "Not connected--- nothing sent.")
	   (insert ?\n)))))))

(defun rmoo-send-here (string)
  "Send STRING as input to rmoo-world-here."
  (comint-send-string (get rmoo-world-here 'process) (concat string "\n")))

(defun rmoo-recenter ()
  "If we should recenter, recenter."
  (if (and (eq (current-buffer) (process-buffer proc))
	   (eq scroll-step 1)
	   (= (point) (point-max)))
      (recenter -1)))

(defun rmoo-string-to-list (string)
  (let ((list nil))
    (while (string-match "\^m?\n" string)
      (setq list (cons (substring string 0
				  (match-beginning 0))
		       list))
      (setq string (substring string (match-end 0) (length string))))
    (cons string (reverse list))))

(defun rmoo-send-kill ()
  "Send the first item on the kill ring to rmoo-worl-here."
  (interactive)
  (rmoo-send-here (car kill-ring)))

(defun rmoo-beginning-of-line ()
  "Move point to beginning-of-line, but after prompt character."
  (interactive)
  (beginning-of-line 1)
  (if (looking-at rmoo-prompt)
      (forward-char (length rmoo-prompt))))

(defun rmoo-quit ()
  "Quit MOO process."
  (interactive)
  (if (yes-or-no-p "Are you sure you want to quit this MOO session? ")
      (progn
	(delete-process (get-buffer-process (current-buffer)))
	(put rmoo-world-here 'process nil)
	(message (substitute-command-keys (concat "Disconnected.  "
         "Press \\[kill-buffer] to kill this buffer."))))))

(defmacro rmoo-match-string (n str)
  (list 'substring str (list 'match-beginning n) (list 'match-end n)))

(defun rmoo-retarget (world)
  (interactive (list (rmoo-request-world)))
  (if (eq (string-match "^\\(.*@\\)\\(.*\\)" mode-name) 0)
      (setq mode-name (concat (substring mode-name (match-beginning 1)
					 (match-end 1))
			      (symbol-name world))))
  (setq rmoo-world-here world))

(defun rmoo-upload-buffer-directly ()
  (interactive)
  (rmoo-send-here (buffer-string)))

;;
;; Thrown in for old times sake..
;;
(defun rmoo-name (world)
  (symbol-name world))

(defun rmoo-destroy ()
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window))

(defun rmoo-upload-and-destroy ()
  (interactive)
  (rmoo-upload-buffer-directly)
  (rmoo-destroy))

;;
;; $Log: rmoo.el,v $
;; Revision 1.12  2000/10/18 21:33:39  mattcampbell
;; Updated my email address.
;;
;; Revision 1.11  1999/11/24 19:32:36  mattcamp
;; Moved declaration of rmoo-setup-done to avoid compiler warning.
;;
;; Revision 1.10  1999/08/17 12:31:16  mattcamp
;; Fixed a typo in the handling of raw IP addresses (changed 'buff' to 'buf').
;;
;; Revision 1.9  1999/07/17 19:25:40  mattcamp
;; Now all of the RMOO setup code runs when the rmoo function runs (if
;; the newly added rmoo-setup-done variable is nil), and rmoo-x-libraries
;; is now defined wiht a defvar rather than a setq function.
;;
;; Revision 1.8  1999/06/13 02:19:23  mattcamp
;; Integrated RMOO setup (the loading of extra libraries and the worlds file), which was formerly placed in the user's .emacs file, into this file, so users don't have to add the large code block to their .emacs files anymore.
;;
;; Revision 1.7  1999/05/28 01:18:15  mattcamp
;; Added new variable, rmoo-send-require-last-line, which when turned on causes rmoo-send to refuse to send anywhere else than the last line of the buffer.  Also modified rmoo-send to handle this.
;;
;; Revision 1.6  1999/05/27 00:37:35  mattcamp
;; I also forgot to make the rmoo-destroy function interactive, so I fixed that.
;;
;; Revision 1.5  1999/05/27 00:18:22  mattcamp
;; Added rmoo-destroy function for destroying a buffer and window without uploading.
;;
;; Revision 1.4  1999/05/27 00:13:22  mattcamp
;; Forgot to make rmoo-upload-and-destroy interactive, so I fixed that.
;;
;; Revision 1.3  1999/05/26 23:38:08  mattcamp
;; Added function rmoo-upload-and-destroy to upload a buffer, kill the buffer, and delete the window.
;;
;; Revision 1.2  1999/05/24 01:23:41  mattcamp
;; Added rmoo-send-always-goto-end option to make RMOO always go to the end
;; of a buffer after sending.
;;
;; Revision 1.1  1999/03/02 00:23:09  mattcamp
;; Initial revision
;;
