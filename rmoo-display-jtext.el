;;
;; This is some code for displaying jtext.
;; Jtext can be spewed in many forms. This code assumes that it will
;; be output in lemoo form
;;
;; Original Author: Ron Tapia <tapia@nmia.com>
;; $Author: mattcamp $
;; $Date: 1999/03/02 00:04:08 $
;; $Revision: 1.2 $
;;
(require 'rmoo)

(defvar jtext-tag-actions '((title . ignore) ;; we don't do headers (yet)
			    (string . insert-before-markers)
			    (text . insert-before-markers)
			    (header . jtext-insert-header)
			    (hgroup . jtext-insert-hgroup)
			    (paragraph . jtext-insert-paragraph)
			    (link . jtext-insert-link)
			    )
  "List of jtext tags and functions to pass control to.")

(defun jtext-tag-class (form)
  "To what class does the given form belong?"
  (if (stringp form)
      'string
    (car form)))

(defun jtext-tag-values (form)
  "What values are associated with the tag in the given form?"
  (if (stringp form)
      (list form)
    (cdr form)))

(defun jtext-insert-vbox (form)
  "Given a vbox, insert it.
The vbox should be a string or a list, and can be obtained by passing
jtext-tagged text to the read function."
  (let ((a (assoc (jtext-tag-class form) jtext-tag-actions)))
    (if a
	(apply (cdr a) (jtext-tag-values form))
      (message "Form not known")
      (insert-before-markers (prin1-to-string form))))
  (insert-before-markers "\n"))

(defvar jtext-header-actions '(jtext-header-action-1
				      jtext-header-action-2
				      jtext-header-action-3
				      jtext-header-action-4
				      jtext-header-action-5
				      jtext-header-action-6)
  "List of actions to take when given header of various levels.")

(defvar jtext-header-faces '(jtext-header-1
			     jtext-header-2
			     jtext-header-3
			     jtext-header-4
			     jtext-header-5
			     jtext-header-6))

(mapcar 'make-face jtext-header-faces)
(mapcar (lambda (face) 
	  (set-face-underline-p face t))
	jtext-header-faces)

(defun jtext-header-action-1 (start end)
  (add-text-properties start end (list 'jtext-start start 'end end))
  (add-text-properties start end  '(face jtext-header-1)))
(defun jtext-header-action-2 (start end )
  (add-text-properties start end (list 'jtext-start start 'end end))
  (add-text-properties start end  '(face jtext-header-2)))
(defun jtext-header-action-3 (start end )
  (add-text-properties start end (list 'jtext-start start 'end end))
  (add-text-properties start end  '(face jtext-header-3)))
(defun jtext-header-action-4 (start end )
  (add-text-properties start end (list 'jtext-start start 'end end))
  (add-text-properties start end  '(face jtext-header-4)))
(defun jtext-header-action-5 (start end )
  (add-text-properties start end (list 'jtext-start start 'end end))
  (add-text-properties start end  '(face jtext-header-5)))
(defun jtext-header-action-6 (start end )
  (add-text-properties start end (list 'jtext-start start 'end end))
  (add-text-properties start end  '(face jtext-header-6)))

(defun jtext-insert-header (level &rest forms)
  (let ((start (point)))
    (mapcar 'jtext-insert-hbox forms)
    (funcall (nth (1- level) jtext-header-actions)
	     start (point)))
  (insert-before-markers "\n"))

(defun jtext-insert-hbox (form)
  "Insert the given form, which should be an hbox."
  (let ((a (assoc (jtext-tag-class form) jtext-tag-actions)))
    (if a
	(apply (cdr a) (jtext-tag-values form))
      (message "Form not known")
      (insert-before-markers (prin1-to-string form)))))

(defun jtext-insert-paragraph (&rest forms)
  (mapcar 'jtext-insert-hbox forms))
(defun jtext-insert-hgroup (&rest forms)
  (mapcar 'jtext-insert-hbox forms))


(defvar jtext-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\^cm" 'jtext-indicated-link)))
  


(defun jtext-insert-link (address link-text)
  (let ((start (point)))
    (jtext-insert-hbox link-text)
    (let ((point (point)))
      (add-text-properties start point 
			   (list 'jtext-start start 'jtext-end point))
      (add-text-properties start point '(face bold-italic))
      (add-text-properties start point (list 'address address))
      (add-text-properties start point '(local-map jtext-link-map)))))
      


(defun jtext-indicated-link (event)
  (end-of-buffer)
  (save-window-excursion
    (save-excursion
      (let* ((position (event-start event))
	     (buffer (window-buffer (posn-window position)))
	     (p (posn-point position))
	     (jtext-address (get-text-property p 'address)) 
	     (text (and jtext-address
			(save-excursion
			  (set-buffer buffer)
			  (buffer-substring
			   (get-text-property p 'jtext-start)
			   (get-text-property p 'jtext-end)))))
	     (case-fold-search t)
	     i)
	(if jtext-address
	    (let* ((address jtext-address)
		   (to-send (concat "#$#jtext-pick address-type: "
				    (symbol-name (car address))
				    " args: \"")))
	      (mapcar (function
		       (lambda (pair)
			 (let ((s (prin1-to-string
				   (prin1-to-string
				    (car (cdr pair))))))
			   (setq to-send (concat to-send (car pair) ": "
						 (substring s 1
							    (1- (length s)))
						 " ")))))
		      (nth 1  address))
	      (rmoo-send-here (concat to-send "\""))
	      t))))))


(defun jtext-select-link (event)
  (interactive "e")
  ;(mouse-set-point event)
  (or (jtext-indicated-link event)
      (mouse-set-point event)))


;;
;; Stuff for interfacing with rmoo.el
;;                           

(defun rmoo-jtext-redirect-function (line)
  (cond ((eq (string-match "lemootag:" line) 0)
	 'rmoo-jtext-insert-line)
	(t nil)))

(defun rmoo-jtext-insert-line (line)
  (goto-char (marker-position (process-mark
			       (get rmoo-world-here 'process))))
  (jtext-insert-vbox (read (substring line (match-end 0)))))

(add-hook 'rmoo-handle-text-redirect-functions 'rmoo-jtext-redirect-function)
(define-key rmoo-interactive-mode-map [mouse-1] 'jtext-select-link)

;; $Log: rmoo-display-jtext.el,v $
;; Revision 1.2  1999/03/02 00:04:08  mattcamp
;; Added revision log.
;;
