;; emacspeak-rmoo.el: Speech-enabling extensions to RMOO
;; Copyright 1999-2000 by Matthew Campbell
;; Some code adapted from an extension written by Amit Patel
;; <amitp@cs.stanford.edu>.  However, please contact Matt regarding
;; any questions or problems you have with this software.
;; This is free software, covered by the GNU General Public License.
;;
;; Original Author: Matthew Campbell <mattcampbell@pobox.com>
;; $Author: mattcampbell $
;; $Date: 2000/10/18 21:32:22 $
;; $Revision: 1.10 $
;;

(require 'rmoo)
(condition-case () (progn
		     (require 'emacspeak-speak)
		     (require 'emacspeak-fix-interactive))
  (error nil))
(require 'advice)
(provide 'emacspeak-rmoo)

(emacspeak-define-sound 'moo-login "cow.wav")
(emacspeak-define-sound 'moo-activity "cow5.au")

(defvar emacspeak-rmoo-already-did-activity-notify nil)
(make-variable-buffer-local 'emacspeak-rmoo-already-did-activity-notify)
(defvar emacspeak-rmoo-unheard-output-start nil)
(make-variable-buffer-local 'emacspeak-rmoo-unheard-output-start)
(defvar emacspeak-rmoo-unheard-output-end nil)
(make-variable-buffer-local 'emacspeak-rmoo-unheard-output-end)

(defun emacspeak-rmoo-handle-text ()
"This function is intended to be added to the rmoo-handle-text-hooks
variable in order to make RMOO speak incoming text and notify the user
of activity in RMOO buffers other than the current one."
  (declare (special emacspeak-comint-autospeak))
  (let ((dtk-stop-immediately nil))
    (when (and
     (eq (window-buffer) (current-buffer))
     emacspeak-comint-autospeak)
      (emacspeak-speak-region (point-min) (point-max)))
      (unless (or
     (eq (window-buffer) (current-buffer))
     emacspeak-rmoo-already-did-activity-notify)
      (setq emacspeak-rmoo-already-did-activity-notify t)
      (emacspeak-auditory-icon 'moo-activity)
      (message (concat "Activity in " (buffer-name))))
    (unless (eq (window-buffer) (current-buffer))
      (when (eq emacspeak-rmoo-unheard-output-start nil)
        (setq emacspeak-rmoo-unheard-output-start (point-min)))
      (setq emacspeak-rmoo-unheard-output-end (point-max)))))

(add-hook 'rmoo-handle-text-hooks 'emacspeak-rmoo-handle-text)

(defadvice emacspeak-speak-mode-line (after emacspeak-rmoo last pre act)
  (when (eq major-mode 'rmoo-interactive-mode)
    (emacspeak-rmoo-catchup)))

(defun emacspeak-rmoo-catchup ()
"This function is intended to be called by the advice form for
emacspeak-speak-mode-line in the emacspeak-rmoo module.  It speaks any activity in the
current buffer that the user may have missed while he was off in
another buffer, and updates the variables used internally by
the emacspeak-rmoo module."
  (declare (special emacspeak-comint-autospeak))
  (when (and
   (not (eq emacspeak-rmoo-unheard-output-start nil))
   (not (eq emacspeak-rmoo-unheard-output-end nil)))
    (when emacspeak-comint-autospeak
      (let ((dtk-stop-immediately nil))
        (emacspeak-speak-region emacspeak-rmoo-unheard-output-start emacspeak-rmoo-unheard-output-end)))
    (setq emacspeak-rmoo-unheard-output-start nil)
    (setq emacspeak-rmoo-unheard-output-end nil))
  (setq emacspeak-rmoo-already-did-activity-notify nil))

(emacspeak-fix-interactive-command-if-necessary 'rmoo-worlds-add-new-moo)

;; The following code was contribued by T. V. Raman <raman@adobe.com>
(defadvice rmoo-send (around emacspeak pre act comp)
  "Speak what is displayed"
  (cond 
   ((interactive-p)
    (let ((orig (point)))
      (set-mark (point))
      ad-do-it
      (emacspeak-speak-region orig (point))))
   (t ad-do-it))
  ad-return-value)

(defadvice rmoo-interactive-mode (after emacspeak-rmoo pre act)
  "Play an appropriate auditory icon and sync Emacspeak and the synthesizer."
  (emacspeak-auditory-icon 'moo-login)
  (emacspeak-dtk-sync))

;;
;; $Log: emacspeak-rmoo.el,v $
;; Revision 1.10  2000/10/18 21:32:22  mattcampbell
;; Updated my email address and changed the copyright notice.
;;
;; Revision 1.9  2000/10/16 22:14:41  mattcamp
;; Removed the activity alert that happens when the user is typing; this caused problems for Emacs 20.5 and later.
;;
;; Revision 1.8  2000/01/15 20:36:05  mattcamp
;; Added support for special MOO sounds, including a quick hack to notify the user via an auditory icon if new text arrives from the MOO while he's typing in that MOO buffer.
;;
;; Revision 1.7  1999/10/30 01:09:47  mattcamp
;; Moved requires for emacspeak modules into a condition-case, as in w3-speak, so that this module will compile when emacspeak isn't loaded.
;;
;; Revision 1.6  1999/07/18 00:10:58  mattcamp
;; Added an advice form for the rmoo function to provide an appropriate
;; auditory icon and sync Emacspeak with the synthesizer.  Also removed
;; the line to load the buff-menu library and replaced the many advice
;; forms on buffer-related functions with a single advice form for
;; emacspeak-speak-mode-line, to make sure that emacspeak-rmoo-catchup is
;; called wherever it makes sense to call it.
;;
;; Revision 1.5  1999/03/21 14:33:05  mattcamp
;; Replaced the algorithm for determining whether a user should be notified
;; of activity in a MOO buffer with something that is more reliable and
;; makes more sense.
;;
;; Revision 1.4  1999/03/20 19:43:32  mattcamp
;; Took a little advice from the Emacspeak code (pun completely intended)
;; and fixed the advice forms to act only when the functions are called
;; interactively, then added advice for the functions that open buffers
;; selected from the buffer menu.  Now the system should consistently
;; read missed activity in a MOO buffer when the user switches to that
;; buffer.
;;
;; Revision 1.3  1999/03/20 17:31:59  mattcamp
;; Simplified the code that sets the emacspeak-rmoo-unheard-output-start
;; and emacspeak-rmoo-unheard-output-end variables.
;;
;; Revision 1.2  1999/03/07 04:18:29  mattcamp
;; Fixed bug in advice definitions that was causing some of
;; emacspeak's advice forms to be overwritten.
;;
;; Revision 1.1  1999/03/01 23:01:02  mattcamp
;; Initial revision
;;
