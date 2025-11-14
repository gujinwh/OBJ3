;;; obj3-mode.el --- Emacs mode for the programming language Obj3 -*- lexical-binding: t; -*-
;; It's simply maude-mode modified, the below comments are from maude-mode.el original file.
;; Copyright (C) 2004, 2007  Free Software Foundation, Inc.

;; Author: Ellef Gjelstad <ellefg+obj3*ifi.uio.no>
;; Maintainer: Rudi Schlatte <rudi@constantly.at>
;; URL: https://github.com/rudi/abs-mode
;; Package-Requires: ((emacs "25"))
;; Version: 0.3
;; Keywords: Languages obj3

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;

;; A major mode for the equational logic language Obj3

;; This started with inspiration from Scott Andrew Borton's language
;; mode creation tutorial,
;; http://two-wugs.net/emacs/mode-tutorial.html

;; Todo:
;;
;; - Full Obj3 (and parametrised modules)
;;
;; - C-u C-c C-c to move point to end of inferior-obj3 buffer before
;;   evaluating buffer.
;;
;; - Sometimes font-lock gets into an endless loop

;;; Code:

;; stuff we need

(require 'font-lock)
(require 'comint)
(require 'compile)
(require 'derived)
(require 'ansi-color)
(require 'easymenu)
(require 'imenu)
(require 'newcomment)
(require 'rx)
(require 'cl-lib)

(defgroup obj3 nil
  "Major mode for editing files in the programming language Obj3."
  :group 'languages)

(defcustom obj3-command (executable-find "obj3")
  "Path to the obj3 executable.  Use \\[run-obj3] to run obj3."
  :type 'file
  :group 'obj3)

(defcustom obj3-command-options (list "-ansi-color")
  "Options when starting Obj3."
  :type '(repeat string)
  :group 'obj3)

(defcustom obj3-mode-hook (list 'imenu-add-menubar-index)
  "Hook for customizing `obj3-mode'."
  :type 'hook
  :options (list 'imenu-add-menubar-index)
  :group 'obj3)

(defcustom obj3-indent standard-indent
  "The amount of indentation to use."
  :type 'integer
  :group 'obj3)
(put 'obj3-indent 'safe-local-variable 'integerp)

(defcustom inferior-obj3-mode-hook nil
  "Hook for customizing `inferior-obj3-mode'."
  :type 'hook
  :group 'obj3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Running Obj3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is tested on Unix only.

;; For documentation on the functionality provided by comint mode, and
;; the hooks available for customising it, see the file `comint.el'.

(defvar obj3-last-source-buffer nil
  "The last buffer we operated on.
Used for switching back from the inferior obj3 buffer.")

(define-derived-mode inferior-obj3-mode
  comint-mode "inferior-obj3"
  "Major mode for running Obj3."
  (if (< emacs-major-version 22)
      ;; HACK: emacs 21 knows about the four-argument form of
      ;; add-hook, but starting an inferior obj3 process complains
      ;; about the final `t' in the hook variable.
      (add-hook 'comint-preoutput-filter-functions 'obj3-preoutput-filter)
    (add-hook 'comint-preoutput-filter-functions 'obj3-preoutput-filter nil t))
  (define-key inferior-obj3-mode-map (kbd "C-c C-z")
    'obj3-switch-back-to-source))


;;; Try to eliminate multiple "`Obj3>'" prompts on one line.
(defun obj3-preoutput-filter (output-string)
  "Filter out prompts not at beginning of line.
Argument OUTPUT-STRING: comint output to filter.
This is intended to go into `comint-preoutput-filter-functions'."
  (if (and (string= "Obj3> " output-string)
	   (/= (let ((inhibit-field-text-motion t)) (line-beginning-position))
	       (point)))
      ""
    output-string))

;; for running Obj3
(defvar inferior-obj3-buffer nil
  "Defines the buffer to call the Obj3 engine in.")

;;; This alist tells `compilation-minor-mode' how to detect and parse
;;; compile errors in Obj3's output.
(defvar obj3-compilation-regexp-alist
  `(("^Warning: \"\\([^\"]+\\)\", line \\([0-9]+\\)"
     1 2)
    ("^Advisory: \"\\([^\"]+\\)\", line \\([0-9]+\\)"
     1 2 1))
  "`compilation-error-regexp-alist' for inferior Obj3.")

(defun obj3-send-region (start end)
  "Send the region from START to END to the obj3 process."
  (interactive "r")
  (if (buffer-live-p inferior-obj3-buffer)
      (save-excursion
	(comint-send-region inferior-obj3-buffer start end)
	(unless (string-match "\n$" (buffer-substring start end))
	  (comint-send-string inferior-obj3-buffer "\n")
          (message "Sent string to buffer %s."
                   (buffer-name inferior-obj3-buffer)))
;;         (if obj3-pop-to-buffer-after-send-region
;;             (pop-to-buffer inferior-obj3-buffer)
;;           (display-buffer inferior-obj3-buffer))
        (setq obj3-last-source-buffer (current-buffer)))
    (message "No Obj3 process started.  M-x run-obj3.")))

(defun obj3-send-paragraph ()
  "Send the current paragraph to the Obj3 process."
  (interactive)
  (let ((start (save-excursion
		 (backward-paragraph)
		 (point)))
	(end (save-excursion
	       (forward-paragraph)
	       (point))))
    (obj3-send-region start end)
    (setq obj3-last-source-buffer (current-buffer))))

(defun obj3-send-definition ()
  "Send the current definition to the Obj3 process."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (mark-defun nil)
    (obj3-send-region (point) (mark))))

(defun obj3-send-buffer ()
  "Send the buffer contents to the Obj3 process."
  (interactive)
  (if (buffer-live-p inferior-obj3-buffer)
      (progn
        (comint-check-source (buffer-file-name))
        (comint-send-string inferior-obj3-buffer
                            (concat "in "
                                    ;; (shell-quote-argument (buffer-file-name))
                                    (buffer-file-name)
                                    "\n")))
    (message "No Obj3 process started.  M-x run-obj3."))
  (setq obj3-last-source-buffer (current-buffer)))

(defun obj3-send-file (filename)
  (if (buffer-live-p inferior-obj3-buffer)
      (progn
        (comint-check-source filename)
        (comint-send-string inferior-obj3-buffer
                            (concat "in " filename "\n")))
    (message "No Obj3 process started.  M-x run-obj3.")))

(defun obj3-use-region-p ()
  (cond ((fboundp 'use-region-p) (use-region-p))
        ((fboundp 'region-active-p) (region-active-p))
        (t nil)))

(defun obj3-next-action ()
  "Send buffer or active region to Obj3, starting it if necessary."
  (interactive)
  (if (not (buffer-live-p inferior-obj3-buffer))
      (save-excursion (run-obj3)))
  (if (obj3-use-region-p)
      (obj3-send-region (region-beginning) (region-end))
    (obj3-send-buffer)))

(defun obj3-switch-to-inferior-obj3 ()
  "Switch to the inferior obj3 buffer.
If Obj3 is not running, starts an inferior Obj3 process."
  (interactive)
  (setq obj3-last-source-buffer (current-buffer))
  (run-obj3))

(defun obj3-switch-back-to-source ()
  "Switch from the Obj3 process back to the last active source buffer.
The last buffer is the one we switched form via \\[switch-to-obj3]."
  (interactive)
  (when obj3-last-source-buffer
    (pop-to-buffer obj3-last-source-buffer)))

(defun run-obj3 ()
  "Run an inferior Obj3 process, input and output via buffer *Obj3*.
Runs the hook `inferior-obj3-mode-hook' (after `comint-mode-hook'
is run).

If a Obj3 process is already running, just switch to its buffer.

Use \\[describe-mode] in the process buffer for a list of commands."
  (interactive)
  (if (comint-check-proc inferior-obj3-buffer)
      (pop-to-buffer inferior-obj3-buffer)
    (progn
      (when (buffer-live-p inferior-obj3-buffer)
        (kill-buffer inferior-obj3-buffer))
      (setq inferior-obj3-buffer
            (apply 'make-comint "Obj3" obj3-command nil obj3-command-options))
      (pop-to-buffer inferior-obj3-buffer)
      (inferior-obj3-mode)
      (ansi-color-for-comint-mode-on)
      (set (make-local-variable 'compilation-error-regexp-alist)
           obj3-compilation-regexp-alist)
      (compilation-shell-minor-mode 1)
      (sit-for 0.1)                     ; eliminates multiple prompts
      ;; (comint-simple-send inferior-obj3-buffer "set show timing off .\n")
      (set-process-query-on-exit-flag (get-buffer-process inferior-obj3-buffer)
                                      nil))
    ;; TODO: "cd <dir of buffer>"
    ))

(defun run-full-obj3 ()
  (interactive)
  (run-obj3)
  (comint-send-string inferior-obj3-buffer "in full-obj3.obj3\n")
  (comint-send-string inferior-obj3-buffer "loop init .\n"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Syntax higlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This should be removed, but I don't have the guts yet.
;; May be good in full obj3 context???
(defconst obj3-keywords 
  (eval-when-compile
    (regexp-opt '(;; "in"
                  ;; "loop" "match" "xmatch" "set" "trace" "on" "off" "show" "debug" "select" "include" 
                  ;; "pr" "protecting" 
                  ;; "inc" "including"
                  ;; "sorts" "sort" 
                  ;; "subsorts" "subsort"  ;; Handled otherwise
                  ;; "id:" "identity:" "assoc" "associative" "comm" "commutative" "prec" "precedence" ;; These are handled as operator attributes
                  ;; "idem" "idempotent" "strat" "strategy" 
                  ;; "vars" "var" 
                  "red" "reduce" "rew" "rewrite" "cond" "condition" "subst" "substitution" "cont" "continue" "flat" "flattened" "parens" "parentheses" "cmd" "command"
                  ;; "sort" "sorts" "op" "ops" "var" "vars" "mb" "mbs" "eq" "eqs" "rl" "rls"
                  "kinds" "components" "module"
                  "pr" "inc" "is" "class" "cmb" "rl" "crl" "msg" "msgs" "ceq"
                  ;; "ctor" "id"
                  "and" "or" "else" "fi"
                  "fth" "endfth" "view" "endv"
                  "subclass") 
                t))
  "Obj3 keywords")


;; Making faces
(defface obj3-attribute-face nil
  "attributes to operators (like comm and gather)"
  :group 'obj3)
(defvar obj3-attribute-face 'obj3-attribute-face
  "Face for attributes to operators (like comm and gather)")

(defface obj3-attribute-value-face nil
  "values of attributes to operators (like comm and gather)"
  :group 'obj3)
(defvar obj3-attribute-value-face 'obj3-attribute-value-face
  "Face for values of attributes to operators (like comm and gather)")

(defface obj3-element-face nil
  "about what values we can have in a sort (ctor, subsort etc)"
  :group 'obj3)
(defvar obj3-element-face 'obj3-element-face
  "Face for about what values we can have in a sort (ctor, subsort etc)")

(defface obj3-start-face
  '((t (:inherit font-lock-variable-name-face :weight bold)))
  "Face to starting words like obj in obj3"
  :group 'obj3)
(defvar obj3-start-face 'obj3-start-face
  "Face to starting words like obj in obj3")

(defface obj3-module-name-face
  '((t (:inherit font-lock-type-face :weight bold)))
  "Face to declaration of e.g. modules in obj3"
  :group 'obj3)
(defvar obj3-module-name-face 'obj3-module-name-face
  "Face to declaration of e.g. modules in obj3")

(defface obj3-pattern-face
  '((t (:slant italic)))
  "Face in patterns in ifs and equations"
  :group 'obj3)
(defvar obj3-pattern-face 'obj3-pattern-face
  "Face in patterns in ifs and equations")

(defface obj3-label-face
  '((((type x w32 mac) (class color))
     (:box (:line-width -1 :style released-button)
           ;; 	   :background "grey75" :foreground "black"
           ))
    (t
     (:inverse-video t)))
  "Face on labels in Obj3."
  :group 'obj3)
(defvar obj3-label-face 'obj3-label-face
  "Face on labels in Obj3.")

(defface obj3-file-face
  '((t (:inherit obj3-module-name-face :inverse-video t)))
  "Face on files and directories"
  :group 'obj3)
(defvar obj3-file-face 'obj3-file-face
  "Face on files and directories")

(defface obj3-end-face
  '((t (:inherit bold)))
  "Face on the final '.'"
  :group 'obj3)
(defvar obj3-end-face 'obj3-end-face
  "Face on the final '.'")

;; Temporary variables for obj3-font-lock-regexp.  
;; However, didnt find elegant way of setting them local (with let or something)
;; These regexps match the space after them.
(defvar obj3--flk-label "\\(\\(\\[.+?\\]\\)\\s-+?\\(:\\)\\s-+?\\)?") ; [label]
(defvar obj3--flk-pattern "\\(.*?\\)\\s-+?")              ; pattern term
(defvar obj3--flk-term "\\(.+\\)\\s-+")                   ; term
(defvar obj3--flk-name "\\(\\w+\\)\\s-+") ; General name.  Try to use sth else
(defvar obj3--flk-type-name "\\([a-zA-Z0-9()|{},<>$-]+@?[a-zA-Z0-9()|{},<>$-]*\\s-+\\)") ; sort name.  May contain @{}-,<>$ and several ()
      ;; (defvar obj3--flk-module "\\(\\w\\S-*\\s-+\\)") ; module name	
(defvar obj3--flk-mod-id "\\(\\w\\S-*\\s-+\\)") ; module name	
(defvar obj3--flk-mod-exp "\\(\\w.*?\\)\\s-+") ; module expression.  May be parametrised module, M*N, M+N, (M)
(defvar obj3--flk-end "\\s-?\\(\\.\\)\\s-")         ; end of command. XXX make whitespace mandatory once this isn't used in other expressions
(defvar obj3--flk-end-command "\\(\\.\\))?\\s-") ; end of command.  ) for Full Obj3
(defvar obj3--flk-number-in-square "\\(\\[[0-9]+\\]\\s-+\\)?") ; [10]
(defvar obj3--flk-in-module "\\(\\(\\<in\\>\\)\\s-+\\(\\w+\\)\\s-+\\)?") ; in FOO : 
(defvar obj3--flk-term-possibly-two-lines  ".*?\\s-*?.*?\\s-*?")
(defvar obj3--flk-debug "\\(\\<debug\\>\\s-+\\)?")
(defvar obj3--flk-such-that-condition "\\(\\(\\<such\\s-+that\\>\\|\\<s\\.t\\.\\)\\s-+\\(.+\\)\\s-\\)?")
(defvar obj3--flk-file-name "\\(\\S-+\\)\\s-*")
(defvar obj3--flk-directory "\\(\\w\\S-*\\)\\s-*")
(defvar obj3--flk-on-off "\\<\\(on\\|off\\)\\>\\s-+")

(defun obj3--flk-keyword (keyword)
  (concat "\\(\\<" keyword "\\>\\)\\s-+?"))
(defun obj3--flk-attribute (attribute)
  (concat "\\[.*\\(\\<" attribute "\\>\\).*]"))
(defun obj3--flk-attribute-value (attribute value)
  (concat "\\[.*\\(\\<" attribute "\\>\\)\\s-+\\(" value "\\).*]"))
(defun obj3--flk-attribute-colon-value (attribute value)
  (concat "\\[.*\\(\\<" attribute ":\\)\\s-+\\(" value "\\).*]"))

;; To a certain degree, this follows the order of the Obj3 grammar
(defconst obj3-font-lock-keywords
  (list
   ;; Fontify keywords
   ;;   (cons  (concat "\\<\\(" obj3-keywords "\\)\\>") 'font-lock-keyword-face)
   ;;    ;; Fontify system
   ;;    (cons (concat "\\<\\(" system "\\)\\>") 'font-lock-type-face)
   ;; punctuations : . ->    =    =>     <=    <      >    =/=
   ;;	 (cons "\:\\|\\.\\|\-\>\\|\~\>\\|\=\\|\=\>\\|\<\=\\|\<\\|\>\\|\\/" 'font-lock-keyword-face)
;;; SYSTEM COMMANDS
   (list (concat "^\\s-*" (obj3--flk-keyword "in") obj3--flk-file-name "$")
         '(1 obj3-start-face t t) '(2 obj3-file-face t t))
   (list (concat "^\\s-*\\<\\(quit\\|q\\|eof\\|popd\\|pwd\\)\\\\s-*$")
         '(1 obj3-start-face t t))
   (list (concat (obj3--flk-keyword "cd\\|push") obj3--flk-directory "$")
         '(1 obj3-start-face t t) '(2 obj3-file-face t t))
   (list (concat (obj3--flk-keyword "ls") "\\(.*?\\)\\s-+" obj3--flk-directory "$")
         '(1 obj3-start-face t t) '(2 font-lock-builtin-face t t) '(3 obj3-file-face t t))
;;; COMMANDS
   (list (concat (obj3--flk-keyword "select") obj3--flk-name obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 obj3-module-name-face t t) '(3 obj3-end-face t t))
   (list (concat (obj3--flk-keyword "load") obj3--flk-file-name "$")
         '(1 obj3-start-face t t) '(2 obj3-file-face t t))
   ;; 	 (list (concat (obj3--flk-keyword "in") obj3--flk-file-name)
   ;; 				 '(1 obj3-start-face t t) '(2 obj3-module-name-face t t))
   (list (concat (obj3--flk-keyword "parse") obj3--flk-in-module "\\(:\\)" obj3--flk-term obj3--flk-end-command)
         '(1 obj3-start-face t t) '(3 font-lock-keyword-face t t) '(4 obj3-module-name-face t t) '(6 obj3-end-face))
   (list (concat obj3--flk-debug (obj3--flk-keyword "red\\|reduce")
                 obj3--flk-in-module "\\(:\\)" obj3--flk-term obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 obj3-start-face t t) '(4 font-lock-keyword-face) '(5 obj3-module-name-face t t)
         '(6 font-lock-keyword-face t t) '(8 obj3-end-face))
   (list (concat obj3--flk-debug (obj3--flk-keyword "red\\|reduce")
                 obj3--flk-term obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 obj3-start-face t t)  '(4 obj3-end-face))
   (list (concat obj3--flk-debug (obj3--flk-keyword "rew\\|rewrite") obj3--flk-number-in-square obj3--flk-in-module
                 obj3--flk-term-possibly-two-lines obj3--flk-end-command)
         '(1 obj3-start-face prepend t) ; debug
         '(2 obj3-start-face prepend t) ; rew
         '(3 font-lock-builtin-face prepend t) ; [10]
         '(5 font-lock-keyword-face prepend t) ; in
         '(6 obj3-module-name-face prepend t)
         '(7 obj3-end-face prepend t))
   (list (concat obj3--flk-debug (obj3--flk-keyword "frew\\|frewrite")
                 "\\(\\[[0-9, ]+\\]\\)?\\s-+" ; Note the regexp [10, 10]
                 obj3--flk-in-module
                 obj3--flk-term-possibly-two-lines obj3--flk-end-command)
         '(1 obj3-start-face prepend t) ; debug
         '(2 obj3-start-face prepend t) ; rew
         '(3 font-lock-builtin-face prepend t) ; [10]
         '(5 font-lock-keyword-face prepend t) ; in
         '(6 obj3-module-name-face prepend t)
         '(7 obj3-end-face prepend t))
   (list (concat (obj3--flk-keyword "x?match") obj3--flk-number-in-square
                 obj3--flk-in-module obj3--flk-term "\\(<=\\?\\)" "\\(.+?\\)\\s-"
                 obj3--flk-such-that-condition obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 font-lock-builtin-face t t) '(4 font-lock-keyword-face t t) '(5 obj3-module-name-face t t)
         '(7 obj3-start-face t t)      ; <=?
         '(10 font-lock-keyword-face t t)                  ; such that
         '(12 obj3-end-face t t))                         ; such that
   (list (concat "(?" (obj3--flk-keyword "search") obj3--flk-number-in-square
                 obj3--flk-in-module obj3--flk-term "\\(=>[!+*1]\\)" "\\(.+?\\)\\s-"
                 obj3--flk-such-that-condition obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 font-lock-builtin-face t t) '(4 font-lock-keyword-face t t) '(5 obj3-module-name-face t t)
         '(7 obj3-start-face t t)                                ; =>
         '(10 font-lock-keyword-face t t) ; such that
         '(12 obj3-pattern-face t t)
         '(12 obj3-end-face t t))
   (list (concat (obj3--flk-keyword "continue\\|cont") "\\([1-9][0-9]\\)*\\s-+" obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 font-lock-builtin-face t t) '(3 obj3-end-face t t))
   (list (concat (obj3--flk-keyword "loop") obj3--flk-in-module obj3--flk-term obj3--flk-end-command)
         '(1 obj3-start-face t t) '(3 font-lock-keyword-face t t) '(4 obj3-module-name-face t t) '(6 obj3-end-face t t))
   (list (concat (obj3--flk-keyword "trace") "\\<\\(select\\|deselect\\|include\\|exclude\\)\\>\\s-+" obj3--flk-mod-exp obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 font-lock-keyword-face t t) '(4 obj3-end-face t t))
   (list (concat (obj3--flk-keyword "print") "\\<\\(conceal\\|reveal\\)\\>\\s-+" obj3--flk-mod-exp obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 font-lock-keyword-face t t) '(4 obj3-end-face t t))
   (list (concat (obj3--flk-keyword "break") "\\<\\(select\\|deselect\\)\\>\\s-+" obj3--flk-mod-exp obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 font-lock-keyword-face t t) '(4 obj3-end-face t t))
   (list (concat (obj3--flk-keyword "show") obj3--flk-term obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 'default t t) '(3 obj3-end-face t t))
   (list (concat (obj3--flk-keyword "show") (obj3--flk-keyword "modules") obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 font-lock-keyword-face t t) '(3 obj3-end-face t t))
   (list (concat (obj3--flk-keyword "show") (obj3--flk-keyword "search\\s-+graph") obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 font-lock-keyword-face t t) '(3 obj3-end-face t t))
   (list (concat (obj3--flk-keyword "show") (obj3--flk-keyword "path") "\\([1-9][0-9]*\\)\\s-+" obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 font-lock-keyword-face t t) '(3 font-lock-builtin-face t t) '(4 obj3-end-face t t))
   (list (concat (obj3--flk-keyword "do") (obj3--flk-keyword "clear\\s-+memo") obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 font-lock-keyword-face t t) '(3 obj3-end-face t t))
   (list (concat (obj3--flk-keyword "set") (obj3--flk-keyword "show\\|print\\|trace\\|include") obj3--flk-mod-exp obj3--flk-on-off obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 obj3-attribute-face t t) '(3 obj3-attribute-face t t) '(4 obj3-attribute-value-face t t) '(5 obj3-end-face t t))
   (list (concat (obj3--flk-keyword "set") (obj3--flk-keyword "break") obj3--flk-on-off obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 obj3-attribute-face t t) '(3 obj3-attribute-value-face t t) '(4 obj3-end-face t t))
;;; DEBUGGER COMMANDS
   (list (concat (obj3--flk-keyword "resume\\|abort\\|step\\|where") obj3--flk-end-command)
         '(1 obj3-start-face t t) '(2 obj3-end-face t t))
;;; MODULE
   (list "(?\\(mod\\|obj\\|omod\\|view\\|fth\\)\\s-+\\(.+\\)\\s-+\\(is\\)"
         '(1 obj3-start-face prepend t)
         '(2 obj3-module-name-face prepend t)
         '(3 obj3-start-face prepend t))
   (list  "\\(endm\\|endobj\\|endom\\|endv\\|endfth\\)"
          '(1 obj3-start-face prepend t))
   (list (concat "\\<\\(\\<protecting\\|extending\\|including\\|ex\\|pr\\|inc\\)\\>\\s-+" obj3--flk-mod-exp obj3--flk-end)
         '(1 font-lock-keyword-face append t) '(2 obj3-module-name-face prepend t)'(3 obj3-end-face))
;;; VIEW
   (list (concat (obj3--flk-keyword "view") "\\(.*\\)\\s-+?"
                 (obj3--flk-keyword "from") obj3--flk-mod-exp
                 (obj3--flk-keyword "to") obj3--flk-mod-exp
                 (obj3--flk-keyword "is"))
         '(1 obj3-start-face prepend t) ; view
         '(2 obj3-module-name-face prepend t)
         '(3 obj3-start-face prepend t) ; from
         '(4 obj3-module-name-face prepend t)
         '(5 obj3-start-face prepend t) ; to
         '(6 obj3-module-name-face prepend t)
         '(7 obj3-start-face prepend t)) ; is
   ;; endv handled above together with endm, endobj
;;; MODULE * TYPES
   (list (concat (obj3--flk-keyword "sorts?") "\\(\\([a-zA-Z0-9(){},<>-]+\\s-+\\)+\\)" obj3--flk-end) ; The double \\(\\) because font-lock only match once a line
         '(1 font-lock-keyword-face)
         '(2 font-lock-type-face prepend t)
         '(4 obj3-end-face prepend t))
   ;; subsort.  Havent found good way to do this without colorizing the >s.
   (list "\\(\\<subsorts?\\>\\)\\s-\\(.+\\)\\(\\.\\)" '(1 font-lock-keyword-face) '(2 obj3-element-face prepend t) '(3 obj3-end-face prepend t))
   (list "\\(\\<subsorts?\\>\\)[^<]+\\(<\\).+\\(\\.\\)" '(1 font-lock-keyword-face) '(2 'default prepend t) '(3 obj3-end-face prepend t))
   (list "\\(\\<subsorts?\\>\\)[^<]+<[^<]+\\(<\\).+\\(\\.\\)" '(1 font-lock-keyword-face) '(2 'default prepend t) '(3 obj3-end-face prepend t))
   (list "\\(\\<subsorts?\\>\\)[^<]+<[^<]+<[^<]+\\(<\\).+\\(\\.\\)" '(1 font-lock-keyword-face) '(2 'default prepend t) '(3 obj3-end-face prepend t))
   (list "\\(\\<subsorts?\\>\\)[^<]+<[^<]+<[^<]+<[^<]+\\(<\\).+\\(\\.\\)" '(1 font-lock-keyword-face) '(2 'default prepend t) '(3 obj3-end-face prepend t))
   (list "\\(\\<subsorts?\\>\\)[^<]+<[^<]+<[^<]+<[^<]+<[^<]+\\(<\\).+\\(\\.\\)" '(1 font-lock-keyword-face) '(2 'default prepend t) '(3 obj3-end-face prepend t))
   ;; Hmm.  Doesnt work
   ;; 	 (list (concat "\\(subsorts?\\)\\s-+" obj3--flk-type-name "\\(\\(<\\)\\s-+" obj3--flk-type-name "\\)+" obj3--flk-end)
   ;; 				 '(1 obj3-module-name-face t t)
   ;; 				 '(2 'default append append)
   ;; 				 '(3 obj3-element-face prepend t)
   ;; 				 '(4 definiendum-face append ())
   ;; 				 '(5 obj3-element-face prepend t)
   ;; 				 '(6 obj3-end-face prepend t))
   ;;  	 ;; subsorts.  Silly way to do this.  Anyone better?
   ;;  	 (list "\\(\\<subsorts?\\>\\)\\([^<]+\\)\\s-+\\(\\.\\)" '(1 font-lock-keyword-face) '(2 obj3-element-face prepend t) '(3 obj3-end-face prepend t))
   ;;  	 (list "\\(\\<subsorts?\\>\\)\\s-+[^<]+<\\([^<]+\\)\\s-+\\(\\.\\)" '(1 font-lock-keyword-face) '(2 obj3-element-face prepend t) '(3 obj3-end-face prepend t))
   ;;  	 (list "\\(\\<subsorts?\\>\\)\\s-+[^<]+<[^<]+<\\([^<]+\\)\\s-+\\(\\.\\)" '(1 font-lock-keyword-face) '(2 obj3-element-face prepend t) '(3 obj3-end-face prepend t))
   ;;  	 (list "\\(\\<subsorts?\\>\\)\\s-+[^<]+<[^<]+<[^<]+<\\([^<]+\\)\\s-+\\(\\.\\)" '(1 font-lock-keyword-face) '(2 obj3-element-face prepend t) '(3 obj3-end-face prepend t))
   ;;  	 (list "\\(\\<subsorts?\\>\\)\\s-+[^<]+<[^<]+<[^<]+<[^<]+<\\([^<]+\\)\\s-+\\(\\.\\)" '(1 font-lock-keyword-face) '(2 obj3-element-face prepend t) '(3 obj3-end-face prepend t))
   ;;  	 (list "\\(\\<subsorts?\\>\\)\\s-+[^<]+<[^<]+<[^<]+<[^<]+<[^<]+<\\([^<]+\\)\\s-+\\(\\.\\)" '(1 font-lock-keyword-face) '(2 obj3-element-face prepend t) '(3 obj3-end-face prepend t))
   ;;  	 (list "\\(\\<subsorts?\\>\\)\\s-+[^<]+<[^<]+<[^<]+<[^<]+<[^<]+<[^<]+<\\([^<]+\\)\\s-+\\(\\.\\)" '(1 font-lock-keyword-face) '(2 obj3-element-face prepend t) '(3 obj3-end-face prepend t))
   ;;  	 (list "\\(\\<subsorts?\\>\\)\\s-+[^<]+<[^<]+<[^<]+<[^<]+<[^<]+<[^<]+<[^<]+<\\([^<]+\\)\\s-+\\(\\.\\)" '(1 font-lock-keyword-face) '(2 obj3-element-face prepend t) '(3 obj3-end-face prepend t))
   ;;  	 (list "\\(\\<subsorts?\\>\\)\\s-+[^<]+<[^<]+<[^<]+<[^<]+<[^<]+<[^<]+<[^<]+<[^<]+<\\([^<]+\\)\\s-+\\(\\.\\)" '(1 font-lock-keyword-face) '(2 obj3-element-face prepend t) '(3 obj3-end-face prepend t))
   ;; classs    ; could be made more effective?
   (list "\\<\\(\\<class\\)\\s-+\\(.+\\)|"
         '(2 font-lock-type-face prepend t))
   ;; 	 ;; subclasss
   ;; 	 (list "\\<\\(subclass\\)\\>\\([^<]+\\)"
   ;; 				 '(2 font-lock-type-face prepend t))
   ;; 	 (list "\\<subclasses\\>\\([^<]+\\)"
   ;; 				 '(2 font-lock-type-face prepend t))
;;; MODULE * OPERATORS
   ;; ;; This hangs
   ;; (list (concat (obj3--flk-keyword "ops?") "\\(.*\\)\\s-"
   ;;               "\\(:\\)\\s-+\\(" obj3--flk-type-name "\\)*"
   ;;               "\\([-~]>\\)\\s-+" obj3--flk-type-name
   ;;               "\\(\\[[^]]*\\]\\s-+\\)?" obj3--flk-end)
   ;;       '(1 font-lock-keyword-face prepend t)
   ;;       '(2 font-lock-function-name-face prepend t)
   ;;       '(3 font-lock-keyword-face prepend t) ; :
   ;;       '(6 font-lock-keyword-face prepend t) ; ->
   ;;       '(9 obj3-end-face prepend t))
   ;; Attr
   (list (obj3--flk-attribute "assoc\\|associative") '(1 obj3-attribute-face prepend t))
   (list (obj3--flk-attribute "comm\\|commutative") '(1 obj3-attribute-face prepend t))
   (list (obj3--flk-attribute-colon-value "id" "[^]\n]*")       '(1 obj3-attribute-face prepend t) '(2 obj3-attribute-face t t))
   (list (obj3--flk-attribute-colon-value "\\(\\<left\\>\\|\\<right\\>\\)\\s-+id" "[^]\n]*") ; Need to be before the other attributes in the elisp code
         '(1 obj3-attribute-face prepend t) '(2 obj3-attribute-face prepend t) '(3 obj3-attribute-face prepend t))
   (list (obj3--flk-attribute "idem\\|idempotent") '(1 obj3-attribute-face prepend t))
   (list (obj3--flk-attribute "iter\\|iterated") '(1 obj3-attribute-face prepend t))
   (list (obj3--flk-attribute "memo") '(1 obj3-attribute-face prepend t))
   (list (obj3--flk-attribute "ditto") '(1 obj3-attribute-face prepend t))
   (list (obj3--flk-attribute-value "poly" "([ 0-9]+)")       '(1 obj3-attribute-face prepend t)      '(2 obj3-attribute-value-face prepend t))
   (list (obj3--flk-attribute-value "strat\\|strategy" "([ 0-9]+)")       '(1 obj3-attribute-face prepend t)      '(2 obj3-attribute-value-face prepend t))
   (list (obj3--flk-attribute-value "gather" "([ eE&]+)")       '(1 obj3-attribute-face prepend t)      '(2 obj3-attribute-value-face prepend t))
   (list "\\[.*\\(\\<format\\)\\s-+\\(([^)]+)\\).*\\]"     '(1 obj3-attribute-face prepend t)        '(2 obj3-attribute-value-face prepend t))
   (list (obj3--flk-attribute-value "special" "(.+)")       '(1 obj3-attribute-face prepend t)      '(2 obj3-attribute-value-face prepend t))
   ;; StatementAttr elsewhere: nonexec, otherwise, metadata, label
   (list (obj3--flk-attribute-value "prec\\|precedence" "[0-9]+")   '(1 obj3-attribute-face prepend t)	 '(2 obj3-attribute-value-face prepend t))
   ;; 	 (list "\\[.*\\(\\<id:\\)\\s-+\\(\\w+\\).*\\]"   '(1 obj3-attribute-face prepend t)   '(2 obj3-attribute-value-face prepend t))
   (list (obj3--flk-attribute "ctor\\|constructor") '(1 obj3-element-face prepend t))
   (list (obj3--flk-attribute "frozen") '(1 obj3-attribute-face prepend t))
   (list (obj3--flk-attribute-value "frozen" "([ 0-9]+)")       '(1 obj3-attribute-face prepend t)      '(2 obj3-attribute-value-face prepend t))
;;; MODULE * VARIABLES
   (list (concat (obj3--flk-keyword "vars?")
                 "\\(\\([0-9a-zA-Z@$'_,<>-]+\\s-+\\)*\\)"
                 "\\(:\\)\\s-+" obj3--flk-type-name obj3--flk-end)
         '(1 font-lock-keyword-face prepend t)
         '(2 font-lock-variable-name-face prepend t)
         '(4 font-lock-keyword-face prepend t)
         '(6 obj3-end-face prepend t))
;;; MODULE * MEMBERSHIP
   (list (concat "\\<\\(mb\\)\\>\\s-+?" obj3--flk-label obj3--flk-pattern
                 "\\(:\\)\\s-+?" obj3--flk-type-name obj3--flk-end)
         '(1 font-lock-keyword-face prepend t) ; mb
         '(3 obj3-label-face prepend t)       ; [label]
         '(4 font-lock-keyword-face prepend t) ; :
         '(5 obj3-pattern-face prepend t)
         '(6 font-lock-keyword-face prepend t) ; :
         '(7 obj3-element-face prepend t)
         '(8 obj3-end-face prepend t))
   (list (concat "\\<\\(cmb\\)\\>\\s-+?" obj3--flk-label obj3--flk-pattern
                 "\\(:\\)\\s-+?" obj3--flk-type-name
                 "\\(if\\)\\s-+" obj3--flk-pattern obj3--flk-end)
         '(1 font-lock-keyword-face prepend t) ;cmb
         '(3 obj3-label-face prepend t)       ; [label]
         '(4 font-lock-keyword-face prepend t) ; :
         '(5 obj3-pattern-face prepend t)
         '(6 font-lock-keyword-face prepend t) ; :
         '(7 obj3-element-face prepend t)
         '(8 font-lock-keyword-face prepend t) ; if
         '(9 obj3-pattern-face prepend t)
         '(10 obj3-end-face prepend t))
;;; MODULE * EQUATIONS
   (list (concat "\\(\\<eq\\>\\)\\s-+?" obj3--flk-label obj3--flk-pattern
                 "\\(=\\)")
         '(1 font-lock-keyword-face prepend t) ; eq
         '(3 obj3-label-face prepend t)       ; [label]
         '(4 font-lock-keyword-face prepend t) ; :
         '(5 obj3-pattern-face prepend t)     ; pattern
         '(6 font-lock-keyword-face prepend t) ; =
         )
   (list (concat "\\<\\(ceq\\|cq\\)\\>\\s-+?" obj3--flk-label obj3--flk-pattern
                 "\\(=\\)\\s-+")
         '(1 font-lock-keyword-face prepend t)
         '(3 obj3-label-face prepend t) ; [label]
         '(4 font-lock-keyword-face prepend t) ; :
         '(5 obj3-pattern-face prepend t)
         '(6 font-lock-keyword-face prepend t) ; =
         )
                                        ; Statement Attr (as opposed to attr)
   (list (obj3--flk-attribute "nonexec")
         '(1 obj3-attribute-face prepend t))
   (list (obj3--flk-attribute "owise\\|otherwise")
         '(1 obj3-attribute-face prepend t))
   (list (obj3--flk-attribute-value "metadata" "\\w+")
         '(1 obj3-attribute-face prepend t)
         '(2 obj3-attribute-value-face prepend t))
   (list (obj3--flk-attribute-value "label" "\\w+")
         '(1 obj3-attribute-face prepend t)
         '(2 obj3-attribute-value-face prepend t))
;;; MODULE * RULES
                                        ; rl [rule-name] : pattern => result .
   (list (concat "\\(\\<c?rl\\>\\)\\s-+?" obj3--flk-label)
         '(1 font-lock-keyword-face) ; rl
         '(3 obj3-label-face)       ; [label]
         '(4 font-lock-keyword-face) ; :
         )
    (list "\\s-\\(=>\\)\\s-"
          '(1 font-lock-keyword-face prepend t)) ; =>
;;; END OF CORE Obj3 GRAMMAR
;;; FULL Obj3
   ;; Don't have the full Obj3 grammar here, but try to include something
   (list "(\\(omod\\|fth\\|th\\|oth\\) \\(.+\\) \\(is\\)"
         '(1 obj3-start-face prepend t)
         '(2 obj3-module-name-face prepend t)
         '(3 obj3-start-face prepend t))
   (list  "\\(endom\\|endfth\\|endth\\|endoth\\))"
          '(1 obj3-start-face prepend t))
   (list (concat (obj3--flk-keyword "sort\\|class") obj3--flk-type-name
                 (obj3--flk-keyword "to") obj3--flk-type-name obj3--flk-end)
         '(1 font-lock-keyword-face prepend t) ; sort
         '(2 font-lock-type-face prepend t)    ;
         '(3 font-lock-keyword-face prepend t) ; to
         '(4 font-lock-type-face prepend t)
         '(5 obj3-end-face prepend t)) ; .
   (list (concat (obj3--flk-keyword "op\\|msg") obj3--flk-term
                 (obj3--flk-keyword "to") obj3--flk-term obj3--flk-end)
         '(1 font-lock-keyword-face prepend t) ; op
         '(2 font-lock-function-name-face prepend t) ;
         '(3 font-lock-keyword-face prepend t)       ; to
         '(4 font-lock-function-name-face prepend t)
         '(5 obj3-end-face prepend t)) ; .
   (list (concat (obj3--flk-keyword "attr") obj3--flk-name "\\(\\.\\)\\s-+" obj3--flk-type-name
                 (obj3--flk-keyword "to") obj3--flk-name obj3--flk-end)
         '(1 font-lock-keyword-face prepend t)                 ; attr
         '(2 obj3-attribute-face prepend t)                         ;
         '(3 font-lock-keyword-face prepend t)                 ; .
         '(4 font-lock-type-face prepend t)
         '(5 font-lock-keyword-face prepend t) ; to
         '(6 obj3-attribute-face prepend t)
         '(7 obj3-end-face prepend t)) ; .
   ;; OTHER STUFF
   ;; if then else
   (list (concat (obj3--flk-keyword "if") obj3--flk-pattern
                 (obj3--flk-keyword "then") obj3--flk-pattern
                 "\\(" (obj3--flk-keyword "else") obj3--flk-pattern
                 "\\)?" (obj3--flk-keyword "fi"))
         '(1 font-lock-keyword-face prepend t)
         '(2 obj3-pattern-face prepend t)
         '(3 font-lock-keyword-face prepend t) ;then
         '(6 font-lock-keyword-face prepend t) ; else
         '(8 font-lock-keyword-face prepend t)) ; fi
   ;; if condition for ceq, crl
   (list (concat (obj3--flk-keyword "if") obj3--flk-pattern obj3--flk-end)
         '(1 font-lock-keyword-face)
         '(2 obj3-pattern-face)
         '(3 obj3-end-face))
   ;; COMMENTS
   (list (rx (group (| "***" "---") (* nonl) eol)) '(1 font-lock-comment-face t))
   )
  "Subdued level highlighting for Obj3 mode.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Automatic indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun obj3-current-line ()
  "Return the vertical position of point.  Every mode seems to define this."
  (+ (count-lines (window-start) (point))
     (if (= (current-column) 0) 1 0)))

(defun obj3-start-of-comment ()
  "Return start of comment if point is in a comment, nil otherwise.
Currently handles only monoline comments."
  (interactive)
  (save-excursion
    (let ((answer nil) (decision-made nil))
      (while (not decision-made)
        ;; (message "466 obj3-start-of-comment in while")
        (cond ((looking-at "\\(\\*\\*\\*\\)\\|\\(---\\)")
               (setq answer (point) decision-made t))
              ((bolp)
               (setq answer nil decision-made t))
              ((<= (point) 2)
               (setq answer nil decision-made t)))
        (forward-char -1))
      ;; (if answer (message "Yes") (message "No")) ; Debugging
      answer)))

(defun obj3-indent-line ()
  "Indent current line as obj3 code.  Use the variable `obj3-indent'."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (start-regexp "^\\s-*")
        (not-indented t)
        (indentation 0)
        (start-line (obj3-current-line))
        (seen-object-end nil))
    (save-excursion
      (beginning-of-line)
      (save-excursion
        ;; Go back one char at a time, stopping when not not-indented.
        ;; `indentation' is how many steps to indent
        (while not-indented
          (when (eolp)
            (goto-char (or (obj3-start-of-comment) (point))))
          (cond
           ((<= (point) 1) (setq not-indented nil))
           ((or (looking-at (concat start-regexp "(?[fo]?mod\\>"))
                (looking-at (concat start-regexp "^(")))
            (cl-incf indentation obj3-indent)
            (setq not-indented nil))
           ((looking-at (concat start-regexp "end"))
            (cl-incf indentation 0)
            (setq not-indented nil))
           ((or (looking-at (concat start-regexp "\\<c?\\(rl\\|eq\\|mb\\)\\>"))
                (looking-at (concat start-regexp "\\<\\(var\\|op\\|sort\\|subsort\\)s?\\>"))
                (looking-at (concat start-regexp "\\<\\(protecting\\|pr\\|extending\\|ex\\|including\\|inc\\)\\>")))
            (cl-incf indentation (* 2 obj3-indent))
            (setq not-indented nil))
           ;; Obj3's object-based notation: align attributes after |
           ;; if we did not pass something looking like an object end.
           ;; This is a bit hackish.
           ((looking-at " >")
            (setq seen-object-end t))
           ((and (< (obj3-current-line) start-line)
                 (not seen-object-end)
                 (looking-at "< .+ : .+ |"))
            (cl-incf indentation (save-excursion (1+ (progn (search-forward "|")
                                                         (current-column)))))
            (setq not-indented nil))
           ((or (looking-at "\\s(")
                (looking-at "\\<if\\>"))
            (cl-incf indentation 2))
           ((or (looking-at "\\s)")
                (looking-at "\\<fi\\>"))
            (cl-decf indentation 2))
           ((or (looking-at  "\\s-\\.\\s-*?$")
                (looking-at  "\\s-\\.\\s-+\\*\\*\\*"))
            (cl-decf indentation obj3-indent)))
          (if not-indented (forward-char -1)))) ; eof save-excursion
      ;; (message "512 after while")
      ;; (print indentation)
      (save-excursion
        ;; (message "513")
        (beginning-of-line)
        ;; (message "515")
        (cond
         ((or (looking-at (concat start-regexp "="))
              (looking-at (concat start-regexp "\\<\\(if\\|then\\|else\\|fi\\)\\s-"))
              (looking-at (concat start-regexp "to\\s-"))) ; Full Obj3 views
          (cl-decf indentation 2))
         ((or (looking-at (concat start-regexp "\\<c?\\(rl\\|eq\\|mb\\)\\s-"))
              (looking-at (concat start-regexp "\\<\\(including\\|extending\\|protecting\\)\\s-"))
              (looking-at (concat start-regexp "\\<\\(inc\\|ext\\|pr\\)\\s-"))
              (looking-at (concat start-regexp "\\<c?\\(var\\|op\\|sort\\|subsort\\)s?\\s-")))
          (setq indentation obj3-indent))
         ((or  (looking-at (concat start-regexp "\\<\\(in\\|load\\)\\s-"))
               (looking-at (concat start-regexp "(?\\([fo]?mod\\)\\s-"))
               ;; (looking-at (concat start-regexp "\\<\\(end[fo]?m\\))?\\s-"))
               (looking-at (concat start-regexp "\\<\\(end\\)"))
               (looking-at (concat start-regexp "(?\\<\\(search\\|red\\|reduce\\|rew\\|rewrite\\|trace\\|x?match\\)\\s-")))
          (setq indentation 0)))))
    ;;     (if (looking-at "^\\s-*$") (insert "...."))
    ;;     (print indentation)
    ;;     (insert "X") ; See delete-char 1 down
    (if savep
        (save-excursion (indent-line-to (max 0 indentation)))
      (indent-line-to (max 0 indentation))))
  ;; (delete-char 1) ; Delete the X.  This is so we can indent empty lines
  (cond ((looking-at "^$") ; Ugly hack to fix indent in empty lines.  Doesnt work well between modules.
         (insert (make-string obj3-indent ? ))))
  (if (looking-at "^\\s-*$") (end-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Buffer movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun obj3-beginning-of-defun ()
  "Move point to the beginning of the current definition or buffer."
  ;; TODO: handle commands (in, load, ...)
  (let ((start-regexp "^[[:blank:]]*(?[[:blank:]]*\\(?:[fo]?mod\\|f?th\\|view\\)[[:blank:]]+"))
    (when (and (not (bobp)) (looking-at start-regexp))
      ;; Already at module beginning: move backwards to next beginning
      (backward-char 1))
    (beginning-of-line)
    (while (and (not (bobp))
		(not (looking-at start-regexp)))
      (forward-line -1))))

(defun obj3-end-of-defun ()
  "Move point to the end of the current definition or buffer."
  (re-search-forward
   "^[[:blank:]]*end\\(?:f\\(?:m\\|th\\)\\|om\\|th\\|[mv]\\))?" nil :stay))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; imenu
;;;;; Turn this on with `imenu-add-menubar-index' (can be done by
;;;;; customizing `obj3-mode-hook')
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar obj3-imenu-generic-expression
  '((nil
     "^[[:blank:]]*(?[[:blank:]]*\\(?:[fo]?mod\\|f?th\\|view\\)[[:blank:]]+\\([^[:space:]]+\\)"
     1))
  "Module definitions for `imenu'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Abbreviations
;;;;; Turn this on with (abbrev-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar obj3-mode-abbrev-table nil
  "Abbrev table for obj3 mode")

(defun obj3-mode-join-attributes () "Join operator/statement attributes.  Turn on with abbrev mode"
  (save-excursion                       ; remove ][
    (while (re-search-backward "\\] *\\[" (line-beginning-position) t)
      (replace-match " " nil nil)))
  (save-excursion                       ; Remove [[
    (while (re-search-backward "\\[\\(.*\\)\\[" (line-beginning-position) t)
      (replace-match "[\\1 " nil nil)))
  (save-excursion                       ; Remove ]]
    (end-of-line)         ; re-search-backward only search up to point
    (while (re-search-backward "\\]\\(.*\\)\\]" (line-beginning-position) t)
      (replace-match " \\1]" nil nil)))
  (dotimes (_a 3) ; Remove unneccessary double space.  Why is dotimes neessary despite the while?
    (save-excursion
      (while (re-search-backward "\\[\\(.*\\S-?\\)\\s-\\{2,\\}\\(\\S-?.*\\)\\]" (line-beginning-position) t)
        (replace-match "[\\1 \\2]" nil nil))))
  (save-excursion                       ; Remove [\\s-
    (end-of-line)
    (while (re-search-backward "\\[ +" (line-beginning-position) t)
      (replace-match "[" nil nil)))
  (save-excursion                       ; Remove \\s-]
    (end-of-line)
    (while (re-search-backward " +\\]" (line-beginning-position) t)
      (replace-match "]" nil nil)))
  (save-excursion                       ; Fix ]\\s-.
    (end-of-line)
    (re-search-backward "\\(][ \\.]*\\)" (line-beginning-position) t)
    (replace-match "] ."))
  (end-of-line)
  (re-search-backward "\\]\\s-" (line-beginning-position) t)
  (forward-char 1))

(defun obj3-mode-place-after (string) "Place cursor after last occurence of string before point"
  (search-backward string (line-beginning-position))
  (forward-char (length string)))
;; This doesn't work.  Occurs this is executed before insertion of the space trigging abbrev-mode.
;; Doesnt know how to fix this.
;; 	(save-excursion 
;; 		(backward-char 1)
;; 		(if (looking-at "\\s-")
;; 				(delete-char 1)
;; 			(message "not deleted"))))


(define-abbrev-table 'obj3-mode-abbrev-table 
  '(
    ;; Attr (of operators)
    ("ctor" "[ctor]" obj3-mode-join-attributes 0)
    ("assoc" "[assoc]" obj3-mode-join-attributes 0)
    ("associative" "[assoc]" obj3-mode-join-attributes 0)
    ("comm" "[comm]" obj3-mode-join-attributes 0)
    ("commutative" "[comm]" obj3-mode-join-attributes 0)
    ("left" "[left id:]" (lambda () (obj3-mode-join-attributes) (obj3-mode-place-after "id:")) 0)
    ("right" "[right id:]" (lambda () (obj3-mode-join-attributes) (obj3-mode-place-after "id:")) 0)
    ("id" "[id:]"  (lambda () (obj3-mode-join-attributes) (obj3-mode-place-after "id:"))  0)
    ;; ("identity" "[id:"  (lambda () (obj3-mode-join-attributes) (obj3-mode-place-after "id:"))  0)
    ("idem" "[idem]" obj3-mode-join-attributes 0)
    ("iter" "[iter]" obj3-mode-join-attributes 0)
    ("memo" "[memo]" obj3-mode-join-attributes 0)
    ("ditto" "[ditto]" obj3-mode-join-attributes 0)
    ("poly" "[poly]" obj3-mode-join-attributes 0)
    ("strat" "[strat ()]"  (lambda () (obj3-mode-join-attributes) (obj3-mode-place-after "strat ("))  0)
    ("frozen" "[frozen]" (lambda () (obj3-mode-join-attributes) (obj3-mode-place-after "frozen"))  0)
    ("prec" "[prec]"  (lambda () (obj3-mode-join-attributes) (obj3-mode-place-after "prec"))  0)
    ("gather" "[gather ()]"  (lambda () (obj3-mode-join-attributes) (obj3-mode-place-after "gather ("))  0)	
    ("format" "[format ()]"  (lambda () (obj3-mode-join-attributes) (obj3-mode-place-after "format ("))  0)
    ("special" "[special ()]"  (lambda () (obj3-mode-join-attributes) (obj3-mode-place-after "special ("))  0)
    ;; Statement attr (rules, equations and membership axioms)
    ("nonexec" "[nonexec]" obj3-mode-join-attributes 0)
    ("owise" "[owise]" obj3-mode-join-attributes 0)
    ("otherwise" "[owise]" obj3-mode-join-attributes 0)
    ("metadata" "[metadata]"  (lambda () (obj3-mode-join-attributes) (obj3-mode-place-after "metadata"))  0)
    ("label" "[label]"  (lambda () (obj3-mode-join-attributes) (obj3-mode-place-after "label"))  0)
    ;; ;-) To impress your friend with fast typing
    ("list" "[assoc right id:]"  (lambda () (obj3-mode-join-attributes) (obj3-mode-place-after "id:"))  0)
    ("mset" "[comm assoc id:]"  (lambda () (obj3-mode-join-attributes) (obj3-mode-place-after "id:")) 0)
    ("set" "[comm assoc idem]" obj3-mode-join-attributes 0) ; Dangerous with id: here?
    ("endm" "endm" (lambda () (save-excursion (indent-line-to 0))))
    ("endom" "endom" (lambda () (save-excursion (indent-line-to 0))))
    ("endobj" "endobj" (lambda () (save-excursion (indent-line-to 0))))
    ("endth" "endth" (lambda () (save-excursion (indent-line-to 0))))
    ("endoth" "endoth" (lambda () (save-excursion (indent-line-to 0))))
    ("endfth" "endfth" (lambda () (save-excursion (indent-line-to 0))))
    ("endv" "endv" (lambda () (save-excursion (indent-line-to 0))))
    ("mod" "mod" (lambda () (save-excursion (indent-line-to 0))))
    ("obj" "obj" (lambda () (save-excursion (indent-line-to 0))))
    ("omod" "omod" (lambda () (save-excursion (indent-line-to 0))))
    ("view" "view" (lambda () (save-excursion (indent-line-to 0))))
    ("th" "th" (lambda () (save-excursion (indent-line-to 0))))
    ("fth" "fth" (lambda () (save-excursion (indent-line-to 0))))
    ("oth" "oth" (lambda () (save-excursion (indent-line-to 0))))
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Other stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Claim ownership of `.obj3' extension
;;;###autoload
(unless (assoc "\\.obj3\\'" auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.obj3\\'" . obj3-mode)))

(unless (assoc "\\.obj\\'" auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.obj\\'" . obj3-mode)))

;; Tell emacs about the code
(defvar obj3-mode-syntax-table (make-syntax-table)
  "Syntax table for obj3-mode")

;; ; -> should be treated like a word)
;; (modify-syntax-entry ?- "w" obj3-mode-syntax-table))
;; Start of comment.  * as a first and second character
;; (modify-syntax-entry ?* ". 124b" obj3-mode-syntax-table)
;; (modify-syntax-entry ?( ". 2" obj3-mode-syntax-table)
;; (modify-syntax-entry ?) ". 3" obj3-mode-syntax-table)
;; (modify-syntax-entry ?  ". 4" obj3-mode-syntax-table)
;; ;; End of comment.  Newline.
;; (modify-syntax-entry ?\n ">" obj3-mode-syntax-table)

;; This turns out to do as much bad as good.  Due to Emacs' lack of ability to handle 
;; more than two letters in comments, it can't handle the *** comment of obj3.
;; We don't need this either.  Ellef 2003-02-06

;; ;; Start of comment.  ** (emacs can't handle ***).
;; (modify-syntax-entry ?* ". 12" obj3-mode-syntax-table)
;; ;; End of comment.  Newline.
;; (modify-syntax-entry ?\n ">" obj3-mode-syntax-table)

;; ;; The other comment syntax in obj3, multiline (*** *), can't be handled
;; ;; by Emacs.  However, I have made some colouring.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(define-derived-mode obj3-mode prog-mode "Obj3"
  "Major mode for editing Obj3 files.
Provides syntax highlighting.
\\[obj3-indent-line] indents current line.
\\[run-obj3] starts an interactive obj3 process.
\\[run-full-obj3] starts an interactive full obj3 process.
\\[obj3-send-paragraph] sends current paragraph to the (full)
obj3 process.
\\[obj3-send-region] sends current region to the (full) obj3
process.
\\[obj3-send-buffer] sends the entire buffer to the process.
\\[obj3-switch-to-inferior-obj3] jumps between source buffer
and obj3 process buffer.

If you want certain keywords (try operator attributes) to be
automatically expanded, put
 (add-hook 'obj3-mode-hook 
            '(lambda () 
               (abbrev-mode t)))
in your init file.

If you don't want the red warnings, put
  (add-hook 'obj3-mode-hook
            '(lambda () 
                (setq obj3-warnings nil)))
in your init file.

The following keys are set:
\\{obj3-mode-map}"
  :group 'obj3
  :syntax-table obj3-mode-syntax-table
  (define-key obj3-mode-map (kbd "C-c C-c") 'obj3-next-action)
  (define-key obj3-mode-map (kbd "C-c C-r") 'obj3-send-region)
  (define-key obj3-mode-map (kbd "C-M-x") 'obj3-send-definition)
  (define-key obj3-mode-map (kbd "C-c C-b") 'obj3-send-buffer)
  (define-key obj3-mode-map (kbd "C-c C-z") 'obj3-switch-to-inferior-obj3)
  ;; Set up comments -- make M-; work
  (set (make-local-variable 'comment-start) "***")
  (set (make-local-variable 'comment-start-skip)
       "---+[ \t]*\\|\\*\\*\\*+[ \t]*")
  (set (make-local-variable 'comment-end) "")
  (setq font-lock-defaults '(obj3-font-lock-keywords))
  ;; Indentation
  (set (make-local-variable 'indent-line-function) 'obj3-indent-line)
  ;; Movement
  (set (make-local-variable 'beginning-of-defun-function)
       'obj3-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'obj3-end-of-defun)
  ;; Abbrevs
  (setq local-abbrev-table obj3-mode-abbrev-table)
  ;; imenu
  (setq imenu-generic-expression obj3-imenu-generic-expression)
  ;; speedbar support
  (when (fboundp 'speedbar-add-supported-extension)
    (speedbar-add-supported-extension ".obj3")))

;;; Set up the "Obj3" pull-down menu
(easy-menu-define obj3-mode-menu obj3-mode-map
  "Obj3 mode menu."
  '("Obj3"
    ["Evaluate buffer" obj3-send-buffer t]
    ["Evaluate region" obj3-send-region
     :active (if (boundp 'mark-active)
                 mark-active            ; emacs
               (region-exists-p)        ; xemacs
               )]
    ["Evaluate definition" obj3-send-definition t]
    ["---" nil nil]
    ["Run Obj3" run-obj3 t]
    ["Switch to Obj3" obj3-switch-to-inferior-obj3 t]))

(provide 'obj3-mode)

;;; obj3-mode.el ends here
