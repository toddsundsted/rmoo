;;; moocode-mode.el - Major mode for editing LambdaMOO code files
;;
;; Copyright (C) 2012 Rob Myers <rob@robmyers.org>
;; moocode-font-lock-(maybe)-notedit adapted from ruby-mode.el
;; Copyright (C) 1994-2008 Free Software Foundation, Inc.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Installation
;;
;; To install, add this file to your Emacs load path.
;; You can then load it using M-x moocode-mode
;; Alternatively you can have Emacs load it automatically for files with
;; a .moo extension by adding the following to your .emacs file:
;; 
;;    (require 'moocode-mode)
;;    (add-to-list 'auto-mode-alist '("\\.moo$" . moocode-mode))

(require 'rmoo)

;;; TODO:
;;
;; ;"comments"
;; strings as object descriptors in declarations

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom moocode-indent 2
  "How much to indent each MOO code block. Defaults to LambdaMOO style (2)."
  :type '(integer)
  :group 'moocode-mode)

(defcustom moocode-electric-mode t
  "*Non-nil means ; in MOO code major mode calls `moocode-electric-semicolon'."
  :type 'boolean
  :group 'moocode-mode)

(defcustom moocode-mode-hook nil
  "Hook run when entering MOO code major mode."
  :type 'hook
  :group 'moocode-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moocode-font-lock-notedit (limit)
  (when (re-search-forward "^@notedit\\>" limit t)
    (end-of-line)
    (let ((from (point)))
      (forward-line 1)
      (when (re-search-forward "^\\.$" limit t)
	(beginning-of-line)
	(set-match-data (list from (point)))
	t))))

(defun moocode-font-lock-maybe-notedit (limit)
  (let (from)
    (save-excursion
      (when (and (re-search-backward "^\\(@notedit\\>\\|\\.$\\)" nil t)
		 (string= (match-string 1) "@notedit"))
	(end-of-line)
	(setq from (point))))
    (if (and from (and (re-search-forward "^\\(@notedit\\>\\|\\.$\\)" nil t)
		       (string= (match-string 1) ".")))
	(save-excursion
	  (beginning-of-line)
	  (set-match-data (list beg (point)))
	  t)
      nil)))

(defconst moocode-font-lock-keywords
  '(
    ;; @commands: @create @edit etc, although see below for @verb and @prop
    ("^\\s-*@\\w+\\b"
     . font-lock-preprocessor-face)
    ;; Types
    ("\\<\\(ERR\\|FLOAT\\|INT\\|LIST\\|NUM\\|OBJ\\|STR\\)\\>"
     . font-lock-constant-face)
    ;; Keywords
    ("^\\s-*\\(?:break\\|continue\\|e\\(?:lse\\(?:if\\)?\\|nd\\(?:fork?\\|if\\|while\\)\\)\\|fork?\\|if\\|return\\|while\\)\\>"
     ;; Yes, zero. Weird
     . (0 font-lock-keyword-face))
    ;; Single local variable declarations
    ("^\\s-*\\(\\w+\\)\\s-*=[^=]"
     (1 font-lock-variable-name-face))
    ;; Multiple local variable declarations
    ;; Note the empty \\(\\) to ensure there's always a 2, even if empty
    ("^\\s-*{" "\\(\\(\\<\\w+\\>\\)[^,]*\\|\\(\\)\\s-*}\\)" nil nil
     (2 font-lock-variable-name-face))
    ;; Verb declarations
    ("^\\S-*@verb\\s-+\\(\\(\".+\"\\|\\w+\\):\\(\\w+\\)\\)\\(.*\\)$"
     ;; the verb name
     (3 font-lock-function-name-face)
     ;; The verb spec and permissions
     (4 font-lock-constant-face))
    ;; Property declarations
    ("@\\(prop\\|property\\)\\s-+\\(\\w+\\.\\w+\\)\\(.+\\s-+\\([rwc]+\\)\\)?"
     ;; The property name
     (2 font-lock-variable-name-face)
     ;; The property permission flags
     (4 font-lock-constant-face))
    ;; Automatically provided variables in verbs
    ("\\<\\(args\\|argstr\\|caller\\|dobj\\|dobjstr\\|iobj\\|iobjstr\\|player\\|prepstr\\|this\\|verb\\)\\>"
     . font-lock-variable-name-face)
    ;; Built-in functions
    ("\\<\\(\\(?:a\\(?:bs\\|cos\\|dd_\\(?:property\\|verb\\)\\|\\(?:si\\|ta\\)n\\)\\|b\\(?:inary_hash\\|oot_player\\|uffered_output_length\\)\\|c\\(?:all\\(?:_function\\|er\\(?:\\(?:_perm\\)?s\\)\\)\\|eil\\|h\\(?:ildren\\|parent\\)\\|lear_property\\|o\\(?:nnect\\(?:ed_\\(?:\\(?:player\\|second\\)s\\)\\|ion_\\(?:name\\|options?\\)\\)\\|sh?\\)\\|r\\(?:eate\\|ypt\\)\\|time\\)\\|d\\(?:b_disk_size\\|e\\(?:code_binary\\|lete_\\(?:property\\|verb\\)\\)\\|\\(?:isassembl\\|ump_databas\\)e\\)\\|e\\(?:ncode_binary\\|qual\\|val\\|xp\\)\\|f\\(?:l\\(?:o\\(?:\\(?:atst\\|o\\)r\\)\\|ush_input\\)\\|orce_input\\|unction_info\\)\\|i\\(?:dle_seconds\\|ndex\\|s_\\(?:clear_property\\|\\(?:memb\\|play\\)er\\)\\)\\|kill_task\\|l\\(?:ength\\|ist\\(?:append\\|delete\\|en\\(?:ers\\)?\\|\\(?:inser\\|se\\)t\\)\\|og\\(?:10\\)?\\)\\|m\\(?:a\\(?:tch\\|x\\(?:_object\\)?\\)\\|emory_usage\\|in\\|ove\\)\\|notify\\|o\\(?:bject_bytes\\|pen_network_connection\\|utput_delimiters\\)\\|p\\(?:a\\(?:rent\\|ss\\)\\|layers\\|ropert\\(?:ies\\|y_info\\)\\)\\|queue\\(?:_info\\|d_tasks\\)\\|r\\(?:a\\(?:ise\\|ndom\\)\\|e\\(?:ad\\|cycle\\|number\\|s\\(?:et_max_object\\|ume\\)\\)\\|index\\|match\\)\\|s\\(?:e\\(?:conds_left\\|rver_\\(?:log\\|version\\)\\|t\\(?:_\\(?:connection_option\\|p\\(?:layer_flag\\|roperty_info\\)\\|task_perms\\|verb_\\(?:args\\|code\\|info\\)\\)\\|add\\|remove\\)\\)\\|hutdown\\|inh?\\|qrt\\|tr\\(?:cmp\\|ing_hash\\|sub\\)\\|u\\(?:bstitute\\|spend\\)\\)\\|t\\(?:a\\(?:nh?\\|sk_\\(?:id\\|stack\\)\\)\\|i\\(?:cks_left\\|me\\)\\|o\\(?:float\\|int\\|literal\\|num\\|obj\\|str\\)\\|runc\\|ypeof\\)\\|unlisten\\|v\\(?:al\\(?:id\\|ue_\\(?:bytes\\|hash\\)\\)\\|erb\\(?:_\\(?:args\\|code\\|info\\)\\|s\\)\\)\\)\\)\\s-*("
     . (1 font-lock-builtin-face))
    ;; Objects on #1 such as $thing and $string_utils
    ("\\<$\\w+\\>"
     . font-lock-constant-face)
    ;; Don't format the contents of @notedit blocks as code 
    ;; (In fact, overwrite any highlighting with the default font)
    (moocode-font-lock-notedit
     0 'default t)
    (moocode-font-lock-maybe-notedit
     0 'default t)
    ;; Warn of bare object number references
    ("#[0-9]+" . font-lock-warning-face)
  )
  "Highlighting for MOO code major mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moocode-indent-incp ()
  "Whether the current line should result in the indentation level increasing."
  (looking-at "\\s-*\\(?:else\\(?:if\\s-*\\)?\\|for\\(?:k?\\s-*\\)\\|if\\s-*\\|while\\)\\>"))

(defun moocode-indent-decp ()
  "Whether the current line should result in the indentation level decreasing."
  (looking-at "\\s-*\\(?:e\\(?:lse\\(?:if\\)?\\|nd\\(?:fork?\\|if\\|while\\)\\)\\)"))

(defun moocode-blank-linep ()
  "Check whether the current line is empty or consists entirely of whitespace."
  (looking-at "^\\s-*$"))

(defun moocode-property-edit-linep ()
  "Check whether the current line is the start of a propedit block."
    (looking-at "^\\s-*\\(\\(?:@\\(?:answer\\|notedit\\|send\\)\\)\\|@edit\\s-+\\w+\\.\\w+\\)"))

(defun moocode-point-in-property-editp (rlimit)
  "Reverse from (point), checking if a property @edit began before rlimit."
  ;; In verb code downloaded from the MOO, we'll hit (point-min) quickly
  ;; In files of @create @edit etc. we'll hit an @... quickly
  ;; So this isn't quite as bad as it may look
  (save-excursion
    (beginning-of-line)
    (let ((is nil)
	  (keep-searching t))
      (while (and (> (point) rlimit)
		  keep-searching)
	;; Looking at a line starting with @? Whatever it is, we should finish
	(when (looking-at "^\\(\\s-*@\\|\\.$\\)")
	  (setf keep-searching nil)
	  ;; Looking at the start of a property @edit?
	  (when (moocode-property-edit-linep)
	    (setf is t)))
	(forward-line -1))
      is)))

(defun moocode-skip-blank-lines-backward ()
  "Skip lines that are empty or consist entirely of whitespace."
  (while (and (not (bobp))
	      (moocode-blank-linep))
    (forward-line -1)))

(defun moocode-indent-current-line ()
  "Calculate the indentation for the current line and indent it."
  (beginning-of-line)
  (delete-horizontal-space)
   (let ((indent 0))
     (if (moocode-indent-decp)
	 (decf indent moocode-indent))
     (save-excursion
       (if (not (eq (forward-line -1) -1))
	   (progn
	     (moocode-skip-blank-lines-backward)
	     (back-to-indentation)
	     (if (moocode-indent-incp)
		 (incf indent moocode-indent))
	     (incf indent (current-indentation)))
	 ;; Otherwise
	 (setf indent 0)))
     (indent-to indent)))

(defun moocode-indent-line ()
  "Indent the current line as MOO code."
  (interactive)
  (when (not (moocode-point-in-property-editp (point-min)))
      (moocode-indent-current-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for missing semicolons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moocode-line-needs-semicolonp ()
  "Guess whether the current line should end with a semicolon."
  (not (looking-at "^\\(^\\s-*$\\|^\\s-*\\<@\\w+\\>\\|^.$\\|^\\s-*;\\|^\\w+$\\|\\s-*\\<\\(?:break\\|continue\\|e\\(?:lse\\(?:if\\)?\\|nd\\(?:fork?\\|if\\|while\\)\\)\\|fork?\\|if\\|while\\)\\>\\)")))

(defun moocode-line-ends-with-semicolonp ()
  "Check whether the current line ends with a semicolon."
  (looking-at ".+;\\s-*$"))

(defun moocode-skip-edit-forward ()
  "Skip most of the body of an @edit (assuming it contains one entry block)."
  (while (and (not (eobp))
	      (not (looking-at "^\\.$")))
    (forward-line 1)))

(defun moocode-check-semicolons ()
  "Check the buffer for missing semicolons."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (not (eobp))
      (cond ((moocode-edit-propertyp) (moocode-skip-edit-forward))
	    ((moocode-blank-linep) nil) ;; Just skip the line
	    (t (when (and (moocode-line-needs-semicolonp)
			  (not (moocode-line-ends-with-semicolonp))
			  (y-or-n-p "Add semicolon to end of line?"))
		 (end-of-line)
		 (insert ";")
		 (beginning-of-line))))
      (forward-line 1)))
  ;; Don't leave the y-or-n-p message showing after finishing 
  (message nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Electricity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moocode-electric-semicolon ()
  "Insert a semicolon and indent the line."
  (interactive)
  (insert ";")
  (when moocode-electric-mode
    (moocode-indent-line)
    (end-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar moocode-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Allow some extra characters in words
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?@ "w" st)
    (modify-syntax-entry ?$ "w" st)
    ;; Brackets
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    ;; MOO uses c-style comments /* ... */
    (modify-syntax-entry ?/ ". 14" st)
    (modify-syntax-entry ?* ". 23" st)
    st)
  "Syntax table for MOO code major mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar moocode-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    (define-key map ";" 'moocode-electric-semicolon)
    (define-key map "\C-c;" 'moocode-check-semicolons)
    (define-key map "\^c\^c" 'rmoo-upload-and-destroy)
    (define-key map "\^c\^s" 'rmoo-upload-buffer-directly)
    (define-key map "\^c\^]" 'rmoo-destroy)
    map)
  "Keymap for MOO code major mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode moocode-mode fundamental-mode "MOO"
  "Major mode for editing LambdaMOO programming language files.
\\{moocode-mode-map}"
  :group 'moocode-mode
  (use-local-map moocode-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(moocode-font-lock-keywords))
  (set (make-local-variable 'indent-line-function)
       'moocode-indent-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide the mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'moocode-mode)
