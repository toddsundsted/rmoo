;;; moocode-mode.el - Major mode for editing LambdaMOO code files
;;
;; Copyright (C) 2012-2013 Rob Myers <rob@robmyers.org>
;; moocode-font-lock-(maybe)-notedit adapted from ruby-mode.el
;; Copyright (C) 1994-2008 Free Software Foundation, Inc.
;; smie code adapted from octave-mode.el
;; Copyright (C) 1997, 2001-2013 Free Software Foundation, Inc.
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

;;; TODO:
;;
;; strings as object descriptors in declarations
;; string comments
;; Short form error handing
;; templates

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'rmoo)
(require 'smie)

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
    ("\\<\\(ERR\\|FLOAT\\|INT\\|LIST\\|MAP\\|NUM\\|OBJ\\|STR\\)\\>"
     . font-lock-constant-face)
    ;; Keywords
    ("^\\s-*\\(?:break\\|continue\\|e\\(?:lse\\(?:if\\)?\\|nd\\(?:fork?\\|if\\|try\\|while\\)\\|xcept\\)\\|f\\(?:inally\\|ork?\\)\\|if\\|return\\|try\\|while\\)\\>"
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

(require 'smie)

(defconst moocode-operator-table
  '((assoc ";") ;; Did have "\n" in octave, but seems OK without it
    (assoc ",")
    (assoc "*" "/" "%")
    (assoc "+" "-")
    (nonassoc "==" "!=" "<" "<=" ">" ">=" "in")
    (assoc "&&" "||")
    (nonassoc ":" ".")
    (nonassoc "?" "|")
    (assoc "=")))

(defconst moocode-smie-bnf-table
  '((atom)
    ;; We can't distinguish the first element in a sequence with
    ;; precedence grammars, so we can't distinguish the condition
    ;; if the `if' from the subsequent body, for example.
    ;; This has to be done later in the indentation rules.
    (exp
     ;; We seem to get away without this as it doesn't affect indentation
     ;;(exp "\n" exp)
     ;; We need to mention at least one of the operators in this part
     ;; of the grammar: if the BNF and the operator table have
     ;; no overlap, SMIE can't know how they relate.
     (exp ";" exp)
     ("try" exp "except" exp "endtry")
     ("try" exp "finally" exp "endtry")
     ("unwind_protect" exp
      "unwind_protect_cleanup" exp "end_unwind_protect")
     ("unwind_protect" exp "unwind_protect_cleanup" exp "end")
     ("for" exp "endfor")
     ("fork" exp "endfork")
     ("if" exp "endif")
     ("if" exp "else" exp "endif")
     ("if" exp "elseif" exp "else" exp "endif")
     ("if" exp "elseif" exp "elseif" exp "else" exp "endif")
     ("while" exp "endwhile")
     )))

(defconst moocode-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2 moocode-smie-bnf-table
                     ;; We need this to resolve a conflict
                     '((assoc ";"))) ;; Did have "\n" in Octave
    (smie-precs->prec2 moocode-operator-table))))

(defun moocode-smie-forward-token ()
  (skip-chars-forward " \t")
  (cond
   ((and (looking-at "$")
         ;; Ignore newline if it's within parentheses
         (prog1 (let ((ppss (syntax-ppss)))
                  (not (and (nth 1 ppss)
                            (eq ?\( (char-after (nth 1 ppss))))))
           (forward-comment (point-max))))
    ;; Why bother distinguishing \n and ;?
    ";") ;;"\n"
   ((looking-at ";[ \t]*\\($\\)")
    ;; Combine the ; with the subsequent \n.
    (goto-char (match-beginning 1))
    (forward-comment 1)
    ";")
   (t
    (smie-default-forward-token))))

(defun moocode-smie-backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (not (eq (char-before) ?\;)) ;Coalesce ";" and "\n".
           (> pos (line-end-position))
           t
           ;; Ignore it if it's within parentheses.
           (let ((ppss (syntax-ppss)))
             (not (and (nth 1 ppss)
                       (eq ?\( (char-after (nth 1 ppss)))))))
      (skip-chars-forward " \t")
      ;; Why bother distinguishing \n and ;?
      ";") ;;"\n"
     (t
      (smie-default-backward-token)))))

(defun moocode-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) moocode-indent)
    ;; This rule is needed to align the contents of blocks properly
    ;; Otherwise they line up to the token after e.g. if rather than indenting
    (`(:after . ";")
     (when (smie-rule-parent-p "if" "while" "else" "elseif" "for" "fork" "try" "finally")
       (smie-rule-parent moocode-indent)))))

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
;; Syntax table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar moocode-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Allow some extra characters in words
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?@ "w" st)
    (modify-syntax-entry ?$ "w" st)
    ;; Both /* ... */ and // style comments
    (modify-syntax-entry ?\/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n "> " st)
    st)
  "Syntax table for MOO code major mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar moocode-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    (define-key map "\C-c;" 'moocode-check-semicolons)
    (define-key map "\^c\^c" 'rmoo-upload-and-destroy)
    (define-key map "\^c\^s" 'rmoo-upload-buffer-directly)
    (define-key map "\^c\^]" 'rmoo-destroy)
    map)
  "Keymap for MOO code major mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode moocode-mode prog-mode "MOO"
  "Major mode for editing LambdaMOO programming language files.
\\{moocode-mode-map}"
  :group 'moocode-mode
  (use-local-map moocode-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(moocode-font-lock-keywords))
  ;; The convention of using strings as comments breaks smie-indent-comment
  (set (make-local-variable 'comment-start) "/*")
  (set (make-local-variable 'comment-end) "*/")
  (set (make-local-variable 'electric-indent-chars)
       (cons ?\; electric-indent-chars))
  (set (make-local-variable 'electric-layout-rules) '((?\; . after)))
  (smie-setup moocode-smie-grammar #'moocode-smie-rules
              :forward-token  #'moocode-smie-forward-token
              :backward-token #'moocode-smie-backward-token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide the mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'moocode-mode)
