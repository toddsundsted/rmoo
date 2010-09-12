;;rmoo-objects
;;
;; This file implements a way of keeping track of rmoo-specific
;; `hot' objects.
;;
;; This file gives MOO worlds new properties:
;; 
;;       objects - an obarray
;;       objects-file - a file in which to save hot objects
;;

;;
;; objects are symbols, here are some of their properties:
;;
;;      objnum - a string, the object number
;;      parent - a string, the parent of the object
;;      verbs  - a list, ((short-name owner perms full-name) ...)
;;      properties - a list, ((name owner perms) ...)
;;

;;
;; This function defines the format of moo object symbol names
;; mo-<moo world name><objnum>
;;
;; Original Author: Ron Tapia <tapia@nmia.com>
;; $Author: mattcamp $
;; $Date: 1999/11/24 19:31:58 $
;; $Revision: 1.2 $

(require 'rmoo)
(require 'rmoo-mcp)
(provide 'rmoo-objects)

(defun mo-symbol-name (world objnum)
  (concat "mo-" (symbol-name world) objnum))

(defun mo-intern (objnum world)
  (intern (mo-symbol-name world objnum) (get world 'objects)))

(defun mo-intern-soft (objnum world)
  (intern-soft (mo-symbol-name world objnum) (get world 'objects)))

(defun mo-symname-to-objnum (world symname)
  (string-match (concat "mo-" (symbol-name world) "\\(.*$\\)")
		symname)
  (substring symname (match-beginning 1)))

(defun mo-symname-to-objnum-here (symname)
  (mo-symname-to-objnum rmoo-world-here symname))

(defun mo-object-name (world object)
  (mo-symname-to-objnum world (symbol-name object)))

(defvar rmoo-objects-max-objects 200 "The maximum number of objects per MOO.")

;;
;; For minibuffer completion, we need some way of knowing what
;; MOO's objects we need to look at.
;;
(defvar rmoo-objects-current-moo nil "A silly, but necessay kludge.")

;;
;;
;; This needs to be rewritten...but it works for now.
;; In particular, I'm just ignoring predicate for now.
;;
;; 
;;
(defun rmoo-objects-verb-completion-function (string predicate flag)
  (rmoo-objects-completion-function ":" ":" 'verbs string predicate flag))

(defun rmoo-objects-property-completion-function  (string predicate flag)
  (rmoo-objects-completion-function "." "\\." 'properties string predicate flag))

(defun rmoo-objects-completion-function (delim delimregexp prop string predicate flag)
  (let ((index (string-match delimregexp string))
	objstring verbstring objcomp verbcomp
	obj comp comps c-func oc-func)
    (if flag
	(setq c-func 'all-completions)
      (setq c-func 'try-completion))
    (if (eq index nil)
	(progn (setq objstring string)       
	       (setq verbstring "") 
	       (setq oc-func c-func))
      (setq objstring (substring string 0 index))
      (setq verbstring (substring string (+ index 1)))
      (setq oc-func 'try-completion))
    (setq objcomp (funcall oc-func  (mo-symbol-name rmoo-objects-current-moo
						    objstring)
			   (get rmoo-objects-current-moo 'objects)))
    (cond ((eq nil objcomp)
	   nil)
	  ((stringp objcomp)
	   (mo-symname-to-objnum rmoo-objects-current-moo objcomp))
	  ((listp objcomp)
	   (setq rmoo-world-here rmoo-objects-current-moo)
	   (setq objcomp (mapcar 'mo-symname-to-objnum-here objcomp))
	   objcomp)
	  (t
	   (setq obj (mo-intern objstring rmoo-objects-current-moo))
	   (setq verbcomp (funcall c-func verbstring
				   (get obj prop)))
	   (cond ((eq verbcomp t)
		     t)
		 ((eq verbcomp nil)
		  nil)
		 (t
		  (if (eq nil flag)
		      (concat objstring delim verbcomp)
		    (setq comps nil)
		    (while verbcomp
		      (setq comps (cons (concat objstring
						delim
						(car verbcomp))
					comps))
		      (setq verbcomp (cdr verbcomp)))
		    comps)))))))

;;
;; Whenever we connect to a moo, set up the appropriate hot objects.
;;  
(defun rmoo-objects-initialize ()
  (let* ((world rmoo-world-here)
	 (objects (get world 'objects))
	 (objects-file (get world 'objects-file)))
    (if (not objects)
	(put world 'objects (make-vector rmoo-objects-max-objects 0)))
    (if objects-file
	(load-file (expand-file-name objects-file)))))

;;
;; Interface to moo.el (rmoo-worlds)
;;
(add-hook 'rmoo-interactive-mode-hooks 'rmoo-objects-initialize)

(setq rmoo-worlds-properties-to-save
      (append rmoo-worlds-properties-to-save '(objects-file)))

;;
;; Interface to MCP - Request that the MOO send us info about objects.
;;
(defvar rmoo-objects-download-object-command
  "@mcp-download-object")
(defun rmoo-objects-download-object (object)
"Download an object from rmoo-world-here for use in completions, etc."
  (interactive "sObject: ")
  (rmoo-send-here (concat rmoo-objects-download-object-command
			 " "
			 object
			 "\n")))

(rmoo-mcp-register "cache-verb" nil t
		  '(("object" . 'required)
		    ("verb" . 'required)
		    ("owner" . 'required)
		    ("perms" . 'required)
		    ("full-name" . 'required))
		  'rmoo-objects-cache-verb
		  "cached-objects")

(rmoo-mcp-register "cache-property" nil t
		  '(("object" . 'required)
		    ("property" . 'required)
		    ("owner" . 'required)
		    ("perms" . 'required))
		  'rmoo-objects-cache-property
		  "cached-objects")

(rmoo-mcp-register "cache-parent" nil t
		  '(("object" . 'required)
		    ("parent" . 'required))
		  'rmoo-objects-cache-parent
		  "cached-objects")

(rmoo-mcp-register "cache-object" nil t
		  '(("object" . 'required)
		    ("objnum" . 'required))
		  'rmoo-objects-cache-object
		  "cached-objects")

(defun rmoo-objects-cache-object (objname objnum)
  (let ((object (mo-intern objname rmoo-world-here)))
    (put object 'properties nil)
    (put object 'verbs nil)
    (put object  'objnum objnum)
    (message (concat "Caching " objname)))
  'rmoo-objects-nill-function)

(defun rmoo-objects-cache-verb (objname verb owner perms full-name)
  (let* ((object (mo-intern objname rmoo-world-here))
	 (verbs (get object 'verbs)))
    (put object 'verbs (cons (list verb owner perms full-name)
			     verbs))
    (message (concat "Caching " objname ":"  verb))
  'rmoo-objects-niil-function))


(defun rmoo-objects-cache-property (objname property owner perms)
  (let* ((object (mo-intern objname rmoo-world-here))
	 (properties (get object 'properties)))
    (put object 'properties (cons (list property owner perms)
				  properties))
    (message (concat "Caching " objname "." property))
    'rmoo-objects-niil-function))

(defun rmoo-objects-cache-parent (object parent)
  (let* ((object (mo-intern object rmoo-world-here))
	 (parent (mo-intern parent rmoo-world-here)))
    (put object 'parent parent)
    'rmoo-objects-nil-function))

;;
;; A function for adding an object and some properties
;; 
(defun rmoo-objects-add (object-name plist)
    (let ((object (mo-intern object-name rmoo-world-here)))
      (setplist object plist)))
;;
;; A function for saving hot objects to a file
;;
(defun rmoo-objects-insert (object)
  (let (s)
    (setq s (concat "(rmoo-objects-add "
		    (prin1-to-string (mo-object-name rmoo-world-here object))
		    " '"
		    (prin1-to-string (symbol-plist object))
		    ")\n"))
  (insert-before-markers s)))
    
(defun rmoo-objects-write-objects-file (file)
  "Write the current set of cached objects to a rmoo-world-here's objects-file if it's defined. Otherwise prompt for a file name and write to it."
  (interactive (list (or (get rmoo-world-here 'objects-file)
			 (read-file-name "Objects file: "))))
  (save-excursion
    (let ((objects (get rmoo-world-here 'objects))
	  (buf (get-buffer-create 
		(generate-new-buffer-name (concat "* mo-save-"
						  (symbol-name rmoo-world-here)
						  "*"))))
	  (world rmoo-world-here))
      (set-buffer buf)
      (setq rmoo-world-here world)
      (mapatoms 'rmoo-objects-insert objects)
      (write-file file)
      (kill-buffer nil))))
      
;;
;; Objects map
;;
(defvar rmoo-objects-map (make-sparse-keymap) "MOO objects keymap")

(define-key rmoo-interactive-mode-map "\C-c\C-o" rmoo-objects-map)

(define-key rmoo-objects-map "\C-o"
  'rmoo-objects-download-object)
(define-key rmoo-objects-map "\C-s"
  'rmoo-objects-write-objects-file)
(define-key rmoo-objects-map "\C-d"
  'rmoo-objects-delete-object-here)

;;
;; Utilities
;;
(defun rmoo-objects-delete-object (world object)
  (let* ((objects (get world 'objects))
	(i (- (length objects) 1))
	elt)
    (while (>= i 0)
      (setq elt (elt objects i))
      (if (and (symbolp elt) elt  (string= object (mo-symname-to-objnum world (symbol-name elt))))
	  (progn
	    (aset objects i 0)))
      (setq i (- i 1)))
      (put world 'objects objects)))
      
(defun rmoo-objects-delete-object-here (object)
  (interactive (list (rmoo-objects-read-object-here)))
  (rmoo-objects-delete-object rmoo-world-here object))


(defun rmoo-objects-read-object-here ()
  (setq rmoo-objects-current-moo rmoo-world-here)
  (completing-read "Object: " 
		   'rmoo-objects-object-completion-function
		   nil
		   t
		   nil
		   nil))

(defun rmoo-objects-object-completion-function (string predicate flag)
  (let* ((world rmoo-objects-current-moo)
	(newstring (mo-symbol-name world  string))
	comp)
    (setq rmoo-world-here world)
    (cond ((eq nil flag)
	   (setq comp (try-completion newstring
				      (get world 'objects)
				      predicate))
	   (if (eq nil comp)
	       nil
	     (or (eq t comp) (mo-symname-to-objnum-here comp))))
	  ((eq t flag)
	   (setq comp (all-completions newstring 
				       (get world 'objects)
				       predicate))
	   (mapcar 'mo-symname-to-objnum-here comp))
	  ((eq 'lambda flag)
	   (if ((intern-soft newstring (get world 'objects)))
	       t
	     nil)))))

;;
;; $Log: rmoo-objects.el,v $
;; Revision 1.2  1999/11/24 19:31:58  mattcamp
;; Added missing "(require 'rmoo-mcp)" so that everything byte-compiles successfully.
;;
;; Revision 1.1  1999/03/02 00:18:22  mattcamp
;; Initial revision
;;
