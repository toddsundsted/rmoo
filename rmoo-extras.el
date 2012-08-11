;; rmoo-extras.el
;;
;; Extras that really don't belong anywhere else.
;;
;; Original Author: Ron Tapia <tapia@nmia.com>
;; $Author: mattcamp $
;; $Date: 1999/03/02 00:05:04 $
;; $Revision
;;

(require 'rmoo)
(require 'rmoo-code)
(require 'rmoo-objects)

(defvar rmoo-extras-verb-edit-command "@edit" "Command to use to get verb code.")
(defvar rmoo-extras-prop-edit-command "@edit" "Command to use to get a non-text property.")
(make-variable-buffer-local 'rmoo-extras-verb-edit-command)
(make-variable-buffer-local 'rmoo-extras-prop-edit-command)

;;
;; These expect to get called from buffer with a MOO process.
;;
;; Maybe they should be in the files that define the appropriate modes.
;;
(defun rmoo-extras-get-verb (verb)
  (interactive (progn
		  (setq rmoo-objects-current-moo rmoo-world-here)
		  (list (completing-read 
			 "Verb: " 
			 'rmoo-objects-verb-completion-function))))
    (comint-send-string nil (concat rmoo-extras-verb-edit-command " " verb "\n")))

(defun rmoo-extras-read-property (string)
  (setq rmoo-objects-current-moo rmoo-world-here)
  (completing-read
   string 'rmoo-objects-property-completion-function))

(defun rmoo-extras-get-prop (prop)
  "Sends a command to a MOO asking for a property.
If you're lucky, it may even display the verb code in a Text Mode buffer."
  (interactive (list (rmoo-extras-read-property "Property: ")))
  (comint-send-string nil (concat rmoo-extras-prop-edit-command " " prop "\n")))

(defun rmoo-@paste-kill (&optional prefix)
  "Send whatever's in the kill ring to the MOO, using #8855's @paste command."
  (interactive (list (if current-prefix-arg
			 (read-string "Prefix: " "|")
		       "")))
  (rmoo-send-here (concat "@paste " prefix "\n" (car kill-ring) "\n.")))

;;
;; Interface to moo.el
;;
(define-key rmoo-interactive-mode-map "\^c\^v" 'rmoo-extras-get-verb)
(define-key rmoo-interactive-mode-map "\^c\^p" 'rmoo-extras-get-prop)
;;(define-key rmoo-interactive-mode-map "\^c\^p" 'rmoo-@paste-kill)
;;(define-key rmoo-interactive-mode-map "\^c\^t" 'rmoo-extras-get-text-prop)
;;(define-key rmoo-interactive-mode-map "\^c\^j" 'rmoo-extras-get-jtext)

;;(defun rmoo-extras-get-jtext (prop)
;;  "Sends a command to a MOO asking for a property that is a list.
;;If you're lucky, it may even display the verb code in a MOO List Mode buffer."
;;  (interactive (list (rmoo-extras-read-property "JDocument: ")))
;;  (send-string nil (concat rmoo-extras-jtext-edit-command " " prop "\n")))
;;(defun rmoo-extras-get-text-prop (prop)
;;  "Sends a command to a MOO asking for a text property.
;;If you're lucky, it may even display the verb code in a MOO Text Mode buffer."
;;  (interactive (list (rmoo-extras-read-property "Property: ")))
;;  (send-string nil (concat rmoo-extras-text-edit-command " " prop "\n")))
;;



;;(defvar rmoo-extras-text-edit-command "@edit" "Command to use to get text properties.")
;;(defvar rmoo-extras-jtext-edit-command "" "Command to use to get a jtext property.")
;;(make-variable-buffer-local 'rmoo-extras-jtext-edit-command)
;;(make-variable-buffer-local 'rmoo-extras-text-edit-command)

;; $Log: rmoo-extras.el,v $
;; Revision 1.1  1999/03/02 00:05:04  mattcamp
;; Initial revision
;;
