;; rmoo-menus.el: Pull-down menus for RMOO
;;
;; Original Author: Ron Tapia <tapia@nmia.com>
;; $Author: mattcamp $
;; $Date: 1999/03/02 00:16:00 $
;; $Revision: 1.1 $

(require 'rmoo)
(require 'moocode-mode)
(require 'rmoo-objects)

;;
;; Some commands to download MOO stuff to a buffer for editing.
;;
(defvar rmoo-edit-menu-map (make-sparse-keymap "RMOO-Edit"))
(define-key rmoo-interactive-mode-map [menu-bar rmoo-edit] (cons "RMOO-Edit" rmoo-edit-menu-map))

;;(define-key rmoo-edit-menu-map [rmoo-jtext] '("Jtext" . rmoo-extras-get-jtext))
;;(define-key rmoo-edit-menu-map [rmoo-list] '("List" . rmoo-extras-get-list))
;;(define-key rmoo-edit-menu-map [rmoo-text] '("Text Property" . rmoo-extras-get-text-prop))
(define-key rmoo-edit-menu-map [rmoo-prop] '("Property" . rmoo-extras-get-prop))
(define-key rmoo-edit-menu-map [rmoo-verb] '("Verb" . rmoo-extras-get-verb))


;;
;; Miscelaneous stuff
;;
(defvar rmoo-mode-menu-map (make-sparse-keymap "RMOO"))
(define-key rmoo-interactive-mode-map  [menu-bar moo] (cons "RMOO" rmoo-mode-menu-map))


;;
;;Support for tcl..
;;
(defvar rmoo-foo nil)
(if rmoo-foo
    (progn
      (defvar rmoo-tk-menu-map (make-sparse-keymap "RMOO-TK"))
      (define-key rmoo-mode-menu-map [rmoo-tk] (cons "RMOO-TK" rmoo-tk-menu-map))
      (define-key rmoo-tk-menu-map [start-tk] '("Start TK" . rmoo-start-tk))
      (define-key rmoo-tk-menu-map [blank] '("Clear Canvas" . rmoo-tk-canvas-blank))))

;;
;; Objects
;;
(defvar rmoo-objects-menu-map (make-sparse-keymap "Objects"))
(define-key rmoo-mode-menu-map [rmoo-objects] (cons "Objects" rmoo-objects-menu-map))
(define-key rmoo-objects-menu-map [rmoo-save-objects] '("Save Objects" . rmoo-objects-write-objects-file))
(define-key rmoo-objects-menu-map [delete-object] '("Delete object" . rmoo-objects-delete-object-here))
(define-key rmoo-objects-menu-map [download-object] '("Cache object" . rmoo-objects-download-object))

;;
;; Worlds
;;

(defvar rmoo-worlds-menu-map (make-sparse-keymap "Worlds"))
(define-key rmoo-mode-menu-map [rmoo-worlds] (cons "Worlds" rmoo-worlds-menu-map))
(define-key rmoo-worlds-menu-map [save-worlds] '("Save worlds" . rmoo-worlds-save-worlds-to-file))
(define-key rmoo-worlds-menu-map [add-new] '("Add World" . rmoo-worlds-add-new-moo))

;;
;; Misc
;;

(define-key rmoo-mode-menu-map [rmoo-rmail] '("MOO RMail" . rmoo-rmail))
(define-key rmoo-mode-menu-map [rmoo-mail]  '("Mail" . rmoo-mail))
(define-key rmoo-mode-menu-map [rmoo-send-kill] '("Send Kill" . rmoo-send-kill))
(define-key rmoo-mode-menu-map [rmoo-send-selection] '("Send X-selection" . rmoo-send-x-selection))
(define-key rmoo-mode-menu-map [rmoo-paste-kill] '("@Paste Kill" . rmoo-@paste-kill))
(define-key rmoo-mode-menu-map [rmoo-paste-selection] '("@paste X-selection" . rmoo-@paste-x-selection))

;; Menu for rmoo-code-mode
;;
;(defvar rmoo-code-mode-menu-map (make-sparse-keymap "RMOO-Code"))
;(define-key rmoo-code-mode-map  [menu-bar rmoo-code] (cons "RMOO-Code" rmoo-code-mode-menu-map))
;(define-key rmoo-code-mode-menu-map [rmoo-retarget] '("Retarget" . rmoo-retarget))
;(define-key rmoo-code-mode-menu-map [rmoo-oupload] '("Upload" . rmoo-upload-buffer-directly))

;;
;; Menu for rmoo-mail-mode
;;
(defvar rmoo-mail-mode-menu-map (make-sparse-keymap "RMOO-Mail"))
(define-key rmoo-mail-mode-map  [menu-bar rmoo-mail] (cons "RMOO-Mail" rmoo-mail-mode-menu-map))

(define-key rmoo-mail-mode-menu-map [rmoo-upload] '("Upload" . rmoo-upload-buffer-directly))

;;
;; Menus for MOO RMail
;;
(defvar rmoo-rmail-list-mode-menu-map (make-sparse-keymap "RMOO-RMail"))
(define-key rmoo-rmail-list-mode-map  [menu-bar rmail] (cons "RMOO-RMail" rmoo-rmail-list-mode-menu-map))
(define-key rmoo-rmail-list-mode-menu-map [get-summary] '("Get New Mail" . rmoo-rmail-list-get-mail))
(define-key rmoo-rmail-list-mode-menu-map [get-sequence] '("Get Msg Seq" . rmoo-rmail-list-get-mail-in-sequence))

(defvar rmoo-rmail-summary-mode-menu-map (make-sparse-keymap "RMOO-RMail"))
(define-key rmoo-rmail-summary-mode-map  [menu-bar rmoo-rmail] (cons "RMOO-RMail" rmoo-rmail-summary-mode-menu-map))
(define-key rmoo-rmail-summary-mode-menu-map [get-message] '("Get Message" . rmoo-rmail-get-message))
(define-key rmoo-rmail-summary-mode-menu-map [get-message] '("Display Lists" . rmoo-rmail-switch-to-lists-buffer))

;;
;; Utilities
;;
(defun rmoo-@paste-x-selection ()
  (interactive)
  (rmoo-send-here (concat "@paste \n" (x-selection) "\n.")))

(defun rmoo-send-x-selection ()
  (interactive)
  (rmoo-send-here (x-selection)))





;;
;; popup command menus
;;
;;(defvar rmoo-commands-map '(keymap "MOO Commands"))
;;(define-key rmoo-interactive-mode-map [C-c-down-mouse-2] rmoo-commands-map)
;;(defvar common-commands (make-sparse-keymap "Common Commands"))
;;(defvar 

;;
;; $Log: rmoo-menus.el,v $
;; Revision 1.1  1999/03/02 00:16:00  mattcamp
;; Initial revision
;;
