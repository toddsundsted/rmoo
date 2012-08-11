;;; major mode for programming in RMOO-code
;;; by Erik Ostrom (eostrom@research.att.com)
;;; 1992
;;;
;;; for use with mud.el and assorted derivations thereof
;;; to use: load-file this file (or eval-current-buffer)
;;;         M-x rmoo-code-mode
;;; Make sure your client is loaded _before_ this file, if you want to
;;;   use it interactively (i.e., edit verbs online).
;;; This has been tested with the mud.el available on parcftp.xerox.com
;;;   as of Thu Dec  3 16:40:33 1992.  I have no idea if it will work with
;;;   others.  I suggest you put this file somewhere in your load-path,
;;;   modify your copy of mud.el to
;;;     (require 'rmoo-code)
;;;   *at the end of the file*,
;;;   and add the line
;;;     (rmoo-code-mode)
;;;   at the very beginning of the function definition for rmoo-fix-listing.
;;; It would also be nice to have rmoo-code-mode interact nicely with
;;;   MOO's "local editing" facility, but that's more complicated.

;;;
;;; may '92 added a local variable rmoo-code-default-directory
;;; and made rmoo-code-mode set default-directory to that
;;; -RT
;;;
;;; june '92 added a hooks to rmoo-code-mode
;;;
;;; $Author: mattcamp $
;;; $Date: 1999/05/27 00:30:16 $
;;; $Revision: 1.4 $
;;;
(require 'rmoo)
(provide 'rmoo-code)

(defvar rmoo-code-default-directory "~/" "The place where you keep your moo code.")
(defvar rmoo-code-mode-hooks nil)

(defconst rmoo-code-font-lock-keywords
  '(("\\<\\(if\\|elseif\\|else\\|endif\\|for\\|in\\|endfor\\|while\\|endwhile\\|try\\|except\\|finally\\|endtry\\|fork\\|endfork\\|break\\|continue\\|return\\)\\>" 1 font-lock-keyword-face)
    ("#-?[0-9]+" . font-lock-reference-face)))

(defconst rmoo-code-reserved-words
  '(("if[ (]" "for[ (]" "while[ (]" "fork[ (]" "try"    "else" "except"
     "finally")
    ("endif"  "endfor"  "endwhile"  "endfork"  "endtry" "else" "except"
     "finally")))

(define-prefix-command 'rmoo-code-extras-map)
(let ((map 'rmoo-code-extras-map))
  (define-key map "i"  'rmoo-code-if)
  (define-key map "e"  'rmoo-code-else)
  (define-key map "f"  'rmoo-code-for)
  (define-key map "w"  'rmoo-code-while)
  (define-key map "k"  'rmoo-code-fork)
  (define-key map "r"  'rmoo-code-return)
  (define-key map "s"  'rmoo-code-sin)
  (define-key map "c"  'rmoo-code-commentify)
  (define-key map "u"  'rmoo-code-uncommentify))

(defvar rmoo-code-mode-map
  (let ((map (copy-keymap (cond ((boundp 'rmoo-macro-expansion-mode-map)
				 rmoo-macro-mode-map)  ; kluge for diff versions
				((boundp 'rmoo-macro-expansion-mode-map)
				 rmoo-macro-expansion-mode-map)
				(t
				 (make-sparse-keymap))))))
    (define-key map "\t"        'rmoo-code-indent-line)
    (define-key map "\C-c\C-a"  'rmoo-code-extras-map)
    (define-key map "\C-c\""    'rmoo-code-insert-quoted-end)
    (define-key map "\C-c\;"    'rmoo-code-check-semi-colons)
    map)
  "Extra keys used in RMOO-code mode.")

(defmacro rmoo-perform-replace (from to)
  "Replace one string with another."
  (list 'save-excursion
	(list 'while (list 'search-forward from nil t)
	      (cond ((not (equal to ""))
		     (list 'replace-match to t t))
		    (t
		     (list 'delete-char
			   (if (stringp from)
			       (- (length from))
			     (list '- (list 'length from)))))))))

(defvar rmoo-mode-syntax-table nil
  "Syntax table used while in MOO mode.")

(defun rmoo-code-mode ()
  "Major mode for mucking with MOO code.
Commands:
\\{rmoo-code-mode-map}
"
  (interactive)
  (setq mode-name (concat "RMOO-Code@" (symbol-name rmoo-world-here)))
  (setq major-mode 'rmoo-code-mode)
  (if (null rmoo-mode-syntax-table)
      (progn
	(setq rmoo-mode-syntax-table (make-syntax-table))
	(set-syntax-table rmoo-mode-syntax-table)
	(modify-syntax-entry ?* ". 23")
	(modify-syntax-entry ?/ ". 14")
	(modify-syntax-entry ?_ "w")
	(modify-syntax-entry ?\[ "(]")
	(modify-syntax-entry ?\] ")["))
    (set-syntax-table rmoo-mode-syntax-table))
  (use-local-map rmoo-code-mode-map)
  (make-local-variable 'rmoo-expansion-macro-name)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'rmoo-code-indent-line)
  ; Setting up syntax recognition
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (setq comment-start "/* "
	comment-end " */"
	comment-start-skip "/\\*[ \n\t]+")
  (setq font-lock-defaults '((rmoo-code-font-lock-keywords) nil t))
  (setq default-directory rmoo-code-default-directory)
  (message (substitute-command-keys "Use \\[rmoo-upload-buffer-directly] to send, \\[rmoo-upload-and-destroy] to send and destroy, \\[rmoo-destroy] to abort..."))
  (run-hooks 'rmoo-code-mode-hooks))

(if (boundp 'rmoo-macro-modes)
    (setq rmoo-macro-modes
	  (cons 'rmoo-code-mode
		rmoo-macro-modes)))

(defun rmoo-code-return (expression)
  (interactive "sExpression: ")
  (rmoo-code-indent-line)
  (insert "return (" expression ");\n")
  (rmoo-code-indent-line))

(defun rmoo-code-for (variable set)
  (interactive "sVariable: \nsIn set: ")
  (if (not (or (eq (elt set 0) ?\()
	       (eq (elt set (1- (length set))) ?\))
	       (eq (elt set 0) ?\[)
	       (eq (elt set (1- (length set))) ?\])))
      (setq set (format (if (string-match ".+\\.\\..+" set)
			    "[%s]"
			  "(%s)")
			set)))
  (rmoo-code-insert-statement "for" (concat " " variable " in " set)))
	  
(defun rmoo-code-fork (seconds)
  (interactive "sSeconds: ")
  (rmoo-code-insert-statement "fork" (concat " (" seconds ")")))

(defun rmoo-code-while (condition)
  (interactive "sCondition: ")
  (rmoo-code-insert-statement "while" (concat " (" condition ")")))

(defun rmoo-code-if (condition)
  (interactive "sCondition: ")
  (rmoo-code-insert-statement "if" (concat " (" condition ")")))

(defun rmoo-code-insert-statement (statement argument)
  (rmoo-code-indent-line)
  (insert statement argument "\n")
  (rmoo-code-indent-line)
  (save-excursion
    (newline)
    (insert "end" statement)
    (rmoo-code-indent-line)
    (if (not (looking-at "\n"))
	(progn
	  (newline)
	  (rmoo-code-indent-line)))))

(defun rmoo-code-sin (delay message)
  (interactive "nDelay: \nsMessage: ")
  (insert "$command_utils:suspend_if_needed(" (int-to-string delay)
	  (if (eq (length message) 0)
	      ""
	    (concat ", " (prin1-to-string message)))
	  ");"))

(defun rmoo-code-else (elseif)
  (interactive "P")
  (if elseif (call-interactively 'rmoo-code-elseif)
    (progn
      (back-to-indentation)
      (insert "else")
      (rmoo-code-indent-line)
      (newline)
      (rmoo-code-indent-line))))

(defun rmoo-code-elseif (condition)
  (interactive "sCondition: ")
  (insert "elseif (" condition ")")
  (rmoo-code-indent-line)
  (insert "\n")
  (rmoo-code-indent-line))

(defun rmoo-code-indent-line ()
  (interactive)
  (let* ((pos (- (point-max) (point)))
	 (orig (point-marker))
	 (gotoindent (progn (back-to-indentation)
			    (>= (point) orig))))
    (if (not (looking-at "^\\.$"))
	(indent-to
	 (let ((offset 0))
	   (delete-horizontal-space)
	   (if (memq t (mapcar 'looking-at (nth 1 rmoo-code-reserved-words)))
	       (setq offset -2))
	   (save-excursion
	     (if (not (eq (forward-line -1) -1))
		 (progn
		   (while (and (looking-at "^\\s-*$")
			       (not (eq (forward-line -1) -1))))
		   (back-to-indentation)
		   (if (memq t (mapcar 'looking-at
				       (car rmoo-code-reserved-words)))
		       (setq offset (+ 2 offset)))
		   (+ (current-indentation) offset))
	       0)))))
    (if gotoindent
	(back-to-indentation)
      (goto-char orig))))

(defun rmoo-code-commentify (start end)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (end-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (save-restriction
      (narrow-to-region start end)
      (rmoo-perform-replace "\\" "\\\\")
      (rmoo-perform-replace "\"" "\\\"")
      (while (re-search-forward "^.*$" nil t)
	(back-to-indentation)
;	(beginning-of-line)
	(insert "\"")
	(end-of-line)
	(insert "\";")))))
	
(defun rmoo-code-uncommentify (start end)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (end-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (save-restriction
      (narrow-to-region start end)
      (while (re-search-forward "^.*$" nil t)
	(back-to-indentation)
	(delete-char 1)
	(end-of-line)
	(delete-char -2))
      (goto-char start)
      (rmoo-perform-replace "\\\\" "\1")
      (rmoo-perform-replace "\\\"" "\"")
      (rmoo-perform-replace "\1"   "\\"))))

(defun rmoo-code-insert-quoted-end ()
  (interactive)
  (insert ", \".\");"))

(defun rmoo-code-check-semi-colons ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (eq (char-after (point)) ?@)
      (forward-line 1))
    (while (re-search-forward "^.+$" nil t)
      (back-to-indentation)
      (if (memq t (mapcar 'looking-at
			  (apply 'append rmoo-code-reserved-words)))
	  (if (and (progn
		     (end-of-line)
		     (eq (char-after (1- (point))) ?\;))
		   (sit-for 1)
	       (y-or-n-p "Inappropriate semicolon.  Delete? "))
	      (delete-char -1))
	(if (and (prog1
		     (not (looking-at "$"))
		   (end-of-line))
		 (progn
		   (not (memq (char-after (1- (point))) '(?\; ?.))))
		 (sit-for 1)
		 (y-or-n-p "Missing semicolon.  Insert? "))
	    (insert ";")))))
  (message "Done."))

	  
;;
;;
;; Some key bindings
;;
(define-key rmoo-code-mode-map "\^c\^c" 'rmoo-upload-and-destroy)
(define-key rmoo-code-mode-map "\^c\^s" 'rmoo-upload-buffer-directly)
(define-key rmoo-code-mode-map "\^c\^]" 'rmoo-destroy)

;;
;; Assume that files that end with .moo are MOO code.
;;
(or (assoc "\\.moo$" auto-mode-alist)
  (setq auto-mode-alist (cons '("\\.moo$" . rmoo-code-mode)
                              auto-mode-alist)))
;;
;; X stuff
;;
;(if (string= window-system "x")
;    (progn
;      (require 'hilit19)
;      (hilit-set-mode-patterns
;       'rmoo-code-mode
;       '(
;	 ("^ *\"" "\";" comment)		       ;;MOO comments
;	 (hilit-string-find ?' string)	       ;;strings
;	 ;;I found property highlighting to be a bit
;	 ;;too much
;	 ;;	 ("\\.[^ $,;)]+" nil  highlight)	 ;;properties
;	 (":[^( $]+" nil highlight)	       ;;verbs
;	 ("\\$[a-z][^ :);,.]+" nil ForestGreen)  ;;objects
;	 ("#[^ :);,.]+" nil ForestGreen)	       ;;objects
;	 ))
;      (add-hook 'rmoo-code-mode-hooks 'hilit-highlight-buffer)))

;;;
;;; $Log: rmoo-code.el,v $
;;; Revision 1.4  1999/05/27 00:30:16  mattcamp
;;; Bound C-c C-] to new rmoo-destroy function and modified mode startup message to substitute command keys at run-time.
;;;
;;; Revision 1.3  1999/05/26 23:46:03  mattcamp
;;; Bound C-c C-c to rmoo-upload-and-destroy.
;;;
;;; Revision 1.2  1999/03/01 23:21:47  mattcamp
;;; Updated the email address for Erik Ostrom given in the comment
;;; at the top of the file.
;;;
;;; Revision 1.1  1999/03/01 23:20:25  mattcamp
;;; Initial revision
;;;
