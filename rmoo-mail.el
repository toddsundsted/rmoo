;;rmoo-mail.el
;;
;; Original Author: Ron Tapia <tapia@nmia.com>
;; $Author: mattcamp $
;; $Date: 1999/03/02 00:12:11 $
;; $Revision: 1.1 $

(require 'rmoo)
(provide 'rmoo-mail)
(defvar rmoo-mail-command "@qsend"
  "Command to use to upload mail to a MOO.")
(make-variable-buffer-local 'rmoo-mail-command)

(defvar rmoo-mail-use-new-frame nil
  "If non-nil, create a new frame for mailing.")

(defvar rmoo-mail-mode-hooks nil "Hooks to run whenever MOO Mail mode
starts up in a buffer.")

;;(defvar rmoo-mail-mode-map (make-sparse-keymap))
(defvar rmoo-mail-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map "\^c\^s" 'rmoo-upload-buffer-directly)
  map))

(define-key rmoo-interactive-mode-map "\^c\^m" 'rmoo-mail)

(provide 'rmoo-mail)
(defun rmoo-mail (to subject)
  (interactive (list (read-string "To: ")
                     (read-string "Subject: ")))
  (let ((buf (get-buffer-create (generate-new-buffer-name 
                                (concat "*Mail to: " to " *"))))
        (world rmoo-world-here)
        (command rmoo-mail-command)
        frame)
    (set-buffer buf)
    (setq rmoo-world-here world)
    (rmoo-mail-mode)
    (goto-char (point-min))
    (setq rmoo-mail-command command)
    (insert-before-markers (concat command 
				   " \"" to "\"\n"
				   subject "\n"
				   "\n."))
    (backward-char 2)
    (if rmoo-mail-use-new-frame
        (make-frame)
      (switch-to-buffer-other-window (current-buffer)))))

(defun rmoo-mail-send-buffer ()
  (interactive)
  (rmoo-send-here (concat rmoo-mail-command "\n" (buffer-string))))


(defun rmoo-mail-mode ()
  (interactive)
  (setq mode-name (concat "RMOOMail@" (symbol-name rmoo-world-here)))
  (setq major-mode 'rmoo-mail-mode)
  (use-local-map rmoo-mail-mode-map)
  (run-hooks 'rmoo-mail-mode-hooks))

;;
;; $Log: rmoo-mail.el,v $
;; Revision 1.1  1999/03/02 00:12:11  mattcamp
;; Initial revision
;;
