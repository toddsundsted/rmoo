;;
;; mud-client protocol
;;
;; Most of the code below is Erik's. I've inserted bits here and there
;; to make it comapatible with rmoo.el.
;; 
;; I may have hacked on this a bit too much.
;;
;; Besides a general replacement of mud with mud (my own whim) this file
;; differs from j-mcp.el in only a few ways:
;;
;;    1. mud-mcp-start-edit     - changed it so that it knows about mud-worlds
;;    2. mud-mcp-cleanup-edit-* - changed so they do the buffer thing and
;;                                 display the window
;;    3. added some functions that act as an interface between the code
;;       in this file and the code in rmoo.el
;;
;; I suspect that 2. was wrong, but I was eager to get a working version.
;; The right thing to do is build the user interface functions in another
;; file on top of the basic mcp functions. Just about everywhere that I've
;; added something moo.el specific, a hook would probably be appropriate.
;; 
;; -Ron
;;
;; Original Author: Ron Tapia
;; $Author: mattcamp $
;; $Date: 1999/11/24 19:20:22 $
;; $Revision: 1.3 $
;;
(require 'rmoo)
(provide 'rmoo-mcp)
(provide 'mcp)


;;(defvar rmoo-mcp-startup-hook rmoo-mode-hook)

(defvar rmoo-mcp-regexp (concat "^#\\$#"
			       "\\([^ :*\\\"]+\\)"         ; request-name
			       "\\(\\*\\)?"                ; data-follows
			       "\\( +\\([^ :\\\"]+\\)\\( \\|$\\)\\)?"
					; authentication-key
			       "\\( *\\(.*\\)\\)$"))       ; key-value pairs

(defvar rmoo-mcp-quoting-prefix "@@@"
  "Prepended to all data sent after an mcp command.
Must not contain any special regexp characters.")

(defvar rmoo-mcp-incomplete nil)

(defvar rmoo-mcp-quoting-end "#$#END"
  "Signals end of mcp data.")

(defvar rmoo-mcp-cleanup-function nil)

(defun rmoo-mcp-init-connection ()
  "Make up a new auth-key and send it; send client options."
  (interactive)
  (make-local-variable 'rmoo-mcp-auth-key)
  (setq rmoo-mcp-auth-key (prin1-to-string (random)))
  (let ((proc (get-buffer-process (current-buffer))))
    (rmoo-send-string (concat "#$#authentication-key " rmoo-mcp-auth-key) proc)
    (rmoo-send-string (apply 'concat
			    "#$#client-options"
			    (mapcar (function
				     (lambda (feature)
				       (concat " " feature)))
				    rmoo-mcp-features-list)) proc)))

(defun rmoo-mcp-dispatch (request-name data-follows auth-key keyval-string)
  "Figure out if we know what to do with the given request;
check the auth-key if it's important;
check that we have all the args we want;
if data-follows, start gathering it."
  (setq fooo (list request-name data-follows auth-key keyval-string))
  (let ((entry (assoc (cons request-name data-follows) rmoo-mcp-request-table)))
    (if entry
	(if (and (rmoo-mcp-need-auth-key entry)
		 (not (equal auth-key rmoo-mcp-auth-key)))
	    (error "Illegal authentication key in %s" (buffer-name))
	  (let ((arglist (rmoo-mcp-arglist entry keyval-string)))
	    (if (listp arglist)
		(apply (rmoo-mcp-setup-function entry) arglist)
	      (rmoo-mcp-handle-unknown data-follows))))
      (rmoo-mcp-handle-unknown data-follows))))

(defun rmoo-mcp-setup-function (entry)
  "What function do we call to deal with this entry in the table?"
  (nth 2 (cdr entry)))

(defun rmoo-mcp-handle-unknown (data-follows)
  (if data-follows
      (let* ((start (point))
	     (line (progn
		     (beginning-of-line 2)
		     (buffer-substring start (point)))))
	(let ((buf (current-buffer)))
	  (set-buffer (rmoo-mcp-setup-data (generate-new-buffer
					   "Unknown data")))
	  (insert line)
	  (set-buffer buf)))
    (rmoo-mcp-remove-line)))

(defun rmoo-mcp-arglist (entry keyval-string)
  (let ((alist (rmoo-mcp-parse-keyvals keyval-string)))
    (if (listp alist)
	(catch 'rmoo-mcp-missing-arg
	  (mapcar
	   (function
	    (lambda (template)
	      (let ((a (assoc (car template) alist)))
		(cond (a
		       (cdr a))
		      ((not (eq 'required (cdr template)))
		       (cdr template))
		      (t
		       (throw 'rmoo-mcp-missing-arg 'rmoo-mcp-missing-arg))))))
	   (nth 1 (cdr (setq barr entry))))))))

(defvar rmoo-mcp-keyval-regexp (concat "\\([^ \\\":]+\\): +"
				      "\\([^ \\\"]+\\|"
				      "\"\\([^\\\"]\\|\\\\.\\)*\"\\)"
				      "\\( +\\|$\\)")
  "Recognize one keyword: value pair.")

(defun rmoo-mcp-parse-keyvals (keyval-string)
  (catch 'rmoo-mcp-failed-parse
    (let ((arglist nil)
	  (start 0))
      (while (< start (length keyval-string))
	(if (string-match rmoo-mcp-keyval-regexp keyval-string start)
	    (setq start (match-end 0)
		  arglist (cons
			   (cons
			    (rmoo-match-string 1 keyval-string)
			    (if (eq (elt keyval-string
					 (match-beginning 2))
				    ?\")
				(car (read-from-string
				      (rmoo-match-string 2 keyval-string)))
			      (rmoo-match-string 2 keyval-string)))
			   arglist))
	  (throw 'rmoo-mcp-failed-parse 'rmoo-mcp-failed-parse)))
      arglist)))
				
(defvar rmoo-mcp-request-table '()
  "Alist of information about known request types, keyed by string.")

(defvar rmoo-mcp-features-list '()
  "List of client features to tell the server about.")

(defun rmoo-mcp-register (request-name data-follows auth-key keys
				      setup-function &optional family)
  "Register a new mcp request type.
REQUEST-NAME is the name of the request type, e.g. \"edit\".
DATA-FOLLOWS is t if the MOO is expected to provide lines of data.
If you want a request to be able to handle both forms, set them up separately.
AUTH-KEY is t if this request type needs an authentication key to work.
KEYS is an alist of pairs (key . default-value).
  The key must be a string.
  The default-value must be a string, or the symbol 'required, which means
that the request must supply a value.
SETUP-FUNCTION is a symbol for the function that gets called to set up
request-specific details.
FAMILY determines the family of client features this lives in; default is
the name of the request, but some requests can be grouped together."
  (let* ((key (cons request-name data-follows))
	 (value (list auth-key keys setup-function))
	 (entry (assoc key rmoo-mcp-request-table)))
    (if entry
	(setcdr entry value)
      (progn
	(setq rmoo-mcp-request-table (cons (cons key value)
					  rmoo-mcp-request-table)
	      family (or family request-name))
	(if (not (member family rmoo-mcp-features-list))
	    (setq rmoo-mcp-features-list
		  (cons family rmoo-mcp-features-list)))))))

(defun rmoo-mcp-need-auth-key (entry)
  "Does this entry in the table need an authentication key?"
  (car (cdr entry)))

(defun rmoo-mcp-remove-line ()
  (let ((start (progn (rmoo-beginning-of-line) (point))))
    (beginning-of-line 2)
    (delete-region start (point))))

(defun rmoo-mcp-setup-data (buffer)
  (rmoo-mcp-remove-line)
  (setq rmoo-state 'unquoting
	rmoo-current-process (get-buffer-process (current-buffer))
	rmoo-buffer buffer))
  
(rmoo-mcp-register "edit" t nil
		  '(("type" . "text")
		    ("name" . 'required)
		    ("upload" . 'required))
		  'rmoo-mcp-start-edit
		  "mcp_edit")

;;
;; What have I done here? Basically, I've told rmoo-mcp-start-edit
;; about rmoo-worlds. 
;;
(defun rmoo-mcp-start-edit (type name upload)
  (let ((buf (current-buffer))
	(world rmoo-world-here))
    (set-buffer (rmoo-mcp-setup-data (get-buffer-create name)))
    (insert (concat "Buffer: " (prin1-to-string (current-buffer))))
    (setq rmoo-world-here world)
    (put world 'output-buffer (current-buffer))
    (put world 'last_output_buffer buf)
    (put world 'last-output-function (get world 'output-function))
    (put world 'output-function 'rmoo-mcp-output-function)
    (erase-buffer)
    (setq rmoo-mcp-cleanup-function
	  (cond
	   ((equal type "program")
	    'rmoo-mcp-cleanup-edit-program)
	   ((equal type "list")
	    'rmoo-mcp-cleanup-edit-list)
	   ((equal type "mail")
	    'rmoo-mcp-cleanup-edit-mail)
	   ((equal type "jtext")
	    'rmoo-mcp-cleanup-edit-jtext)
	   (t
	    'rmoo-mcp-cleanup-edit-text)))
    (insert  upload "\n")
    (set-buffer buf)))

;;
;; I've told rmoo-mcp-cleanup-edit-* about rmoo-worlds. I've also given them
;; the responsibility of displaying the buffer. It might be better
;; to have this responsibility lie elsewhere.
;;
(defun rmoo-mcp-cleanup-edit-program ()
  (let ((world rmoo-world-here))
    (moocode-mode)
    (goto-char (point-max))
    (insert ".\n")
    (goto-char (point-min))
    (setq rmoo-select-buffer (current-buffer))
    (display-buffer (current-buffer) t)
    (setq rmoo-world-here world)))

(defun rmoo-mcp-cleanup-edit-text ()
  (let ((world rmoo-world-here))
    (rmoo-text-mode)
    (goto-char (point-max))
    (insert ".\n")
    (goto-char (point-min))
    (setq rmoo-select-buffer (current-buffer))
    (display-buffer (current-buffer) t)
    (setq rmoo-world-here world)))

(defun rmoo-mcp-cleanup-edit-mail ()
  (let ((world rmoo-world-here))
    (rmoo-mail-mode)
    (goto-char (point-max))
    (insert "\n.\n")
    (backward-char 3)
    (setq rmoo-select-buffer (current-buffer))
    (display-buffer (current-buffer) t)
    (setq rmoo-world-here world)))

(defun rmoo-mcp-cleanup-edit-list ()
  (let ((world rmoo-world-here))
    (rmoo-list-mode)
    (goto-char (point-max))
    (insert ".\n")
    (goto-char (point-min))
    (setq rmoo-select-buffer (current-buffer))
    (display-buffer (current-buffer) t)
    (setq rmoo-world-here world)))

(defun rmoo-mcp-cleanup-edit-jtext ()
  (let ((world rmoo-world-here))
    (rmoo-list-mode)
    (goto-char (point-max))
    (insert ".\n")
    (goto-char (point-min))
    (setq rmoo-select-buffer (current-buffer))
    (switch-to-buffer-other-window (current-buffer))
    (setq rmoo-world-here world)))

;;
;; The functions that don't have data following are easily handled, they 
;; didn't require any modification. But see the mods to the general
;; mcp stuff...
;;
;;
(rmoo-mcp-register "ftp" nil t
		  '(("host" . 'required)
		    ("directory" . 'required)
		    ("file" . 'required)
		    ("type" . 'required)
		    ("destination" . 'required))
		  'rmoo-mcp-do-ftp)

(defun rmoo-mcp-do-ftp (host directory file type destination)
  (rmoo-mcp-remove-line)
  (call-process "fetch-file" nil 0 nil
		host dir file type dest))

(rmoo-mcp-register "gopher" nil t
		  '(("host" . 'required)
		    ("port" . "70")
		    ("path" . "")
		    ("description" . "1"))
		  'rmoo-mcp-do-gopher)

(defvar rmoo-mcp-gopher-buffer nil
  "Buffer to place gopher stuff in.")

(defun rmoo-mcp-do-gopher (host port path description)
  (if (not (fboundp 'gopher-set-object-host))
      (load-library "gopher"))  ; gross hack to get around lack of provide
  (rmoo-mcp-remove-line)
  (let ((here (current-buffer)))
    (gopher-set-object-host gopher-root-node host)
    (setq port (car (read-from-string port)))
    (gopher-set-object-port gopher-root-node port)
    (gopher-dispatch-object
     (vector (aref description 0)
	     (substring description 1)
	     path
	     host
	     port)
     rmoo-mcp-gopher-buffer)
    (cond ((not (equal here (current-buffer)))
	   (setq rmoo-mcp-gopher-buffer (current-buffer))
	   (setq rmoo-select-buffer rmoo-mcp-gopher-buffer)
	   (set-buffer here)))))
  
(rmoo-mcp-register "display-url" nil t
		  '(("url" . 'required)
		    ("command" . 'required))
		  'rmoo-mcp-do-url
		  "urls")

(defun rmoo-mcp-www-buffer (world)
  (let ((buffers (get world 'rmoo-mcp-www-buffers)))
    (while (and buffers (not (buffer-name (car buffers))))
      (setq buffers (cdr buffers)))
    (put world 'rmoo-mcp-www-buffers buffers)
    (if buffers (car buffers))))

(defun rmoo-mcp-do-url (url command)
  (require 'w3)
  (if (not w3-setup-done) (w3-do-setup))
;  (rmoo-mcp-remove-line)
  (let* ((w3-be-asynchronous nil)
	 (buf (current-buffer))
	 (newname (concat "*W3 for " (rmoo-name rmoo-world-here) "*"))
	 (here rmoo-world-here)
	 (switchback (get-buffer-window buf))
	 (oldbuf (if (equal command "goto")
		     (rmoo-mcp-www-buffer rmoo-world-here)))
	 (oldwin (if oldbuf (get-buffer-window oldbuf))))
;;    (w3-retrieve url)
;;; new stuff
    (url-retrieve url)
    (set-buffer url-working-buffer)
    (setq w3-working-buffer url-working-buffer)
;;; end new stuff
    (w3-prepare-buffer t)
    (set-buffer buf)
    (if oldwin
	(progn
	  (select-window oldwin)
	  (switch-to-buffer w3-working-buffer))
      (switch-to-buffer-other-window w3-working-buffer))
    (w3-mode)
    (setq w3-current-last-buffer oldbuf)
    (rename-buffer newname t)
    (put here 'rmoo-mcp-www-buffers (cons (current-buffer)
					 (get here 'rmoo-mcp-www-buffers)))
    (if switchback (switch-to-buffer-other-window buf))))

(defun rmoo-mcp-redirect-function (line)
  (when (equal line "#$#mcp version: 1.0")
    (rmoo-mcp-init-connection))
  (cond ((eq (string-match rmoo-mcp-regexp line) 0)
	 (rmoo-mcp-dispatch (rmoo-match-string 1 line)
			   (if (match-beginning 2) t nil)
			   (if (match-beginning 4)
			       (rmoo-match-string 4 line)
			     nil)
			   (rmoo-match-string 6 line))
	 'rmoo-mcp-nil-function)
	    (t nil)))

(defun rmoo-mcp-nil-function (line) "Okay, this is a kludge")

(defun rmoo-mcp-output-function-hooks ())

(defun rmoo-mcp-output-function (line)
  (cond ((string= rmoo-mcp-quoting-end line)
	 (progn
	   (set-buffer (get rmoo-world-here 'output-buffer))
	   (funcall rmoo-mcp-cleanup-function)
	   (rmoo-output-function-return-control-to-last)
	   (rmoo-set-output-buffer-to-last)
	   'rmoo-mcp-nil-function))
	((eq (string-match (concat "^" rmoo-mcp-quoting-prefix) line) 0)
	 (setq line (substring line (match-end 0)))
	 (set-buffer (get rmoo-world-here 'output-buffer))
	 (let ((start (point))
	       end)
	   (goto-char (point-max))
	   (insert-before-markers (concat line "\n"))
	   (save-restriction
	     (narrow-to-region start (point))
	     (goto-char start)
	     (run-hooks (rmoo-mcp-output-function-hooks)))))
	(t
	 ;;Aieee! Run away
	 (rmoo-output-function-return-control-to-last)
	 (rmoo-set-output-buffer-to-last)
	 (message (concat "Garbled MCP Data: " line)))))

(defun rmoo-mcp-output-function-discard (line)
  (message "Discarding line: %s" line))

;;
;; Interface to moo.el
;;
;; (add-hook 'rmoo-interactive-mode-hooks 'rmoo-mcp-init-connection)
(add-hook 'rmoo-handle-text-redirect-functions 'rmoo-mcp-redirect-function)

;;
;; $Log: rmoo-mcp.el,v $
;; Revision 1.3  1999/11/24 19:20:22  mattcamp
;; Added "(provide 'rmoo-mcp)" so that other modules requiring MCP can require rmoo-mcp and byte-compile successfully (because one of my users insists on having all of RMOO compiled).
;;
;; Revision 1.2  1999/06/07 14:34:51  mattcamp
;; Disabled automatic calling of rmoo-mcp-init-connection on connection; now it is only called if the server sends "#$#mcp version: 1.0".  This is to prevent error messages from servers that don't handle MCP/1.0 messages well.
;;
;; Revision 1.1  1999/03/02 00:14:38  mattcamp
;; Initial revision
;;
