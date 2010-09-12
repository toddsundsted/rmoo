;;rmoo-text
;;
;; Random functions for editing moo text properties
;;
;; Original Author: Ron Tapia <tapia@nmia.com>
;; $Author: mattcamp $
;; $Date: 1999/03/02 00:21:11 $
;; $Revision: 1.1 $
;;
;; I'll work on this later. For now:
(require 'rmoo)
(provide 'rmoo-text)

(defvar rmoo-text-mode-hooks nil "List of functions run everytime emacs enters MOO Text Mode")

(defvar rmoo-text-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map "\^c\^s" 'rmoo-upload-buffer-directly)
  map))

(defun rmoo-text-mode ()
  "A major mode for editing MOO text.

Commands:
\\{rmoo-text-mode-map}"

  (interactive)
  (let ((world rmoo-world-here))
    (setq mode-name (concat "RMOO-Text@" (symbol-name world)))
    (setq major-mode 'rmoo-text-mode)
    (use-local-map (copy-keymap rmoo-text-mode-map))
    (run-hooks rmoo-text-mode-hooks)))

;;
;; $Log: rmoo-text.el,v $
;; Revision 1.1  1999/03/02 00:21:11  mattcamp
;; Initial revision
;;
