;;rmoo-list
;;
;; Random functions for editing moo list properties
;;
;; Original Author: Ron Tapia <tapia@nmia.com>
;; $Author: mattcamp $
;; $Date: 1999/03/02 00:06:49 $
;; $Revision: 1.1 $
;;
;; I'll work on this later. For now:
(require 'rmoo)
(provide 'rmoo-list)

(defvar rmoo-list-mode-hooks nil "List of functions run everytime emacs enters MOO List Mode")

(defvar rmoo-list-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map "\^c\^s" 'rmoo-upload-buffer-directly)
  map))

(defun rmoo-list-mode ()
  "A major mode for editing MOO lists.

Commands:
\\{rmoo-list-mode-map}"

  (interactive)
  (let ((world rmoo-world-here))
    (setq mode-name (concat "RMOO-List@" (symbol-name world)))
    (setq major-mode 'rmoo-list-mode)
    (use-local-map (copy-keymap rmoo-list-mode-map))
    (run-hooks rmoo-list-mode-hooks)))

;;
;; $Log: rmoo-list.el,v $
;; Revision 1.1  1999/03/02 00:06:49  mattcamp
;; Initial revision
;;
