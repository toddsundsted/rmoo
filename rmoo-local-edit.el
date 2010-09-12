;; rmoo-local-edit.el
;;
;; Some code to interact with Lambdastyle local editing.
;;
;; Original Author: Ron Tapia <tapia@nmia.com>
;; $Author: mattcamp $
;; $Date: 1999/11/24 19:21:46 $
;; $Revision: 1.3 $

(require 'rmoo)
(require 'rmoo-mcp)
(provide 'rmoo-local-edit)

(defvar rmoo-local-edit-regexp (concat "^#\\$# "
				      "edit"
				      " name: "
				      "\\(.*\\)"
				      " upload: "
				      "\\(.*\\)$"))

(add-hook 'rmoo-handle-text-redirect-functions 'rmoo-local-edit-redirect-function)

(defun rmoo-local-edit-redirect-function (line)
  (cond ((eq (string-match rmoo-local-edit-regexp line) 0)
	 (let ((buf (get-buffer-create (generate-new-buffer-name
					(substring line
						   (match-beginning 1)
						   (match-end 1)))))
	       (world rmoo-world-here))
	   (set-buffer buf)
	   (setq rmoo-world-here world)
	   (put world 'last-output-function (get world 'output-function))
	   (put world 'output-function 'rmoo-local-edit-output-function)
	   (put world 'last-output-buffer (get world 'output-buffer))
	   (put world 'output-buffer (current-buffer))
	   (insert (substring line (match-beginning 2) (match-end 2)))
	   (insert "\n"))
	 'rmoo-mcp-nil-function)
	(t
	 nil)))
  
(defun rmoo-local-edit-output-function (line)
  (cond ((eq (string-match "^\\.$" line) 0)
	 (set-buffer (get rmoo-world-here 'output-buffer))
	 (insert ".\n")
	 (funcall 'rmoo-local-edit-cleanup-function)
	 (rmoo-output-function-return-control-to-last)
	 (rmoo-set-output-buffer-to-last)
	 'rmoo-mcp-nil-function)
	(t
	 (set-buffer (get rmoo-world-here 'output-buffer))
	 (insert (concat line "\n")))))

(defun rmoo-local-edit-cleanup-function ()
  (let ((world rmoo-world-here))
    (rmoo-code-mode)
    (setq rmoo-world-here world)
    (goto-char (point-min))
    (put rmoo-world-here 'goto-function 'switch-to-buffer-other-window)
    (put rmoo-world-here 'goto-buffer (current-buffer))))
	       
;;
;; $Log: rmoo-local-edit.el,v $
;; Revision 1.3  1999/11/24 19:21:46  mattcamp
;; Now requires rmoo-mcp, so it will byte-compile successfully.
;;
;; Revision 1.2  1999/03/02 00:10:09  mattcamp
;; Removed the word "stoopid" from the comment at the top.
;;
;; Revision 1.1  1999/03/02 00:09:29  mattcamp
;; Initial revision
;;
