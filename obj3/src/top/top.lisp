;; OBJ3 version 2
;; Modified 5/30/2001 by Lutz Hamel for TRIM support
;; Copyright (c) 2000, Joseph Kiniry, Joseph Goguen, Sula Ma
;; Copyright (c) 1988,1991,1993 SRI International
;; All Rights Reserved
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;   o Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;; 
;;   o Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.
;; 
;;   o Neither name of the Joseph Kiniry, Joseph Goguen, Sula Ma, or SRI
;;   International, nor the names of its contributors may be used to
;;   endorse or promote products derived from this software without
;;   specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SRI
;; INTERNATIONAL OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;; $Id: top.lsp,v 206.1 2003/09/26 13:05:36 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; OBJ3 top-level
; WARNING -- this is implementation dependent
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:obj3)

(defun lcl () nil)

(defun bye ()
  (finish-output)
  #+sbcl(sb-ext:quit)
  #-sbcl(quit))

(defvar obj3-load-time)

(defvar *top-level-tag* '(*quit-tag*))

(defvar $$debug nil)
(defvar $$debug_level  1)

(defvar *obj$input_source* nil)
(defvar *obj$input_level* 0)
(defvar *obj$input_nesting_limit* 10)
(defvar *obj$input_quiet* nil)
(defvar *obj$print_errors* t)
(defvar *obj$current_labels* nil) ; for labelled things, rules in particular
(defvar *obj$prompt* "OBJ> ")
(defvar *obj$precmd_hook* nil)
(defvar *obj$postcmd_hook* nil)
(defvar *obj$prompt_hook* nil)

(defvar $)
(defvar *mod_eval$last_before_open* nil)
(defvar *obj$allow_uninstantiated* nil)

; lhh -- convert anything x into a string
(defun convert-to-string (x)
  (let ((str (make-string-output-stream)))
    (princ x str)
    (get-output-stream-string str)
  )
)

;; kiniry -- a unique string based upon the current time and a random
;; value.
(defvar *unique-string*)

; lhh -- uniqe base filename (no extension!)
(defvar *obj-unique-filename*)

(defun sp (n)
  (dotimes (i n) (princ " ")))

(defun top-noshow ()
  (or (and (null *obj$input_source*)
	   (<= *obj$input_level* 0))
      *obj$input_quiet*)
  )

(defun at-top-level ()
  (and (null *obj$input_source*)
       (<= *obj$input_level* 0)))

(defun obj_input (f)
  (if (and (< 4 (length f)) (equal ".obj" (subseq f (- (length f) 4))))
      (obj_input_file f)
    (obj_input_file (concatenate 'string (string f) ".obj"))))

(defun expand_file_name (fname)
  (if (equal "~" fname) (namestring (user-homedir-pathname))
  (if (and (eql #\~ (char fname 0)) (eql #\/ (char fname 1)))
    (concatenate 'string
        (namestring (user-homedir-pathname)) (subseq fname 2))
    fname)))

(defvar *stream*)

(defun obj_input_file (fname)
  ; DANGER this is UNIX inspired
  (when (and (eql #\~ (char fname 0)) (eql #\/ (char fname 1)))
    (setq fname
	(concatenate 'string
	  (namestring (user-homedir-pathname))
	  (subseq fname 2))))
  (when (not (probe-file fname))
    (princ "Cannot find file: ") (princ fname) (terpri)
    (obj3-to-top))
  (let ((*obj$input_source* fname) (*obj$input_level* (1+ *obj$input_level*)))
    (with-open-file (*standard-input* fname :direction :input) 
    (when (< *obj$input_nesting_limit* *obj$input_level*)
      (princ "input nesting is ") (prin1 *obj$input_level*) (terpri)
      (princ "probable input loop (can increase *obj$input_nesting_limit*)")
      (terpri))
    (process_input)
  )))

(defun load_file (fname)                ; DANGER this is UNIX inspired
  (when (and (eql #\~ (char fname 0)) (eql #\/ (char fname 1)))
    (setq fname
	  (concatenate 'string
	               (namestring (user-homedir-pathname))
	               (subseq fname 2))))
  (load fname))

(defun obj3-default-prompt (&optional top-level)
  (print$check)
  (when top-level
    (princ *obj$prompt*))
  (force-output))

(setq *obj$prompt_hook* #'obj3-default-prompt)

(defvar *in-in*)
(defvar *top-level*)

(defun process_input ()
  (let ((reader$$ch 'space)
	(*general_read$$input* *general_read$$void*)
	(*top-level* (at-top-level)))
    ;;(reader$!read_init) ;@@ (take out for obj LISP)
    (obj3-init)
    (memo_rew$create_memo_table)
    (let ((*in-in* nil))
      (catch 'end-process-input
        (loop
          (let ((inp (the-loop)))
            (when *obj$postcmd_hook*
              (funcall *obj$postcmd_hook* *top-level* inp)))
          (when *in-in*
            (setq *obj$print_errors* t)
            (setq *in-in* nil)))))))

(defun the-loop ()
  (let (inp inp1 inp2 inp3 inp4 inp5)
    (flet ((inp1-in (list)
             (member inp1 list :test #'equal))
           (inp1= (string-or-symbol)
             (equal inp1 string-or-symbol)))
      (catch (if *top-level* 'obj3-top-level-error 'obj3-main-error)
        (catch 'obj3-error
          (when (and *top-level*
	             *obj$prompt_hook*)
            (funcall *obj$prompt_hook* *top-level*))
          (setq inp (module_parse$parse) ;; lhh -- does the command parsing
                inp1 (first inp)
                inp2 (second inp)
                inp3 (third inp)
                inp4 (fourth inp)
                inp5 (fifth inp))
          (when *obj$precmd_hook*
            (funcall *obj$precmd_hook* *top-level* inp))
          (when (inp1-in `("eof" "q" "quit" eof inp ,(car *reader$eof_value*)))
            (throw 'end-process-input t))
          (unless (or *top-level* *obj$input_quiet*)
            (unless (inp1-in '("---" "***" "?" "["))
              (fresh-line)
              (u:princn "=========================================="))
            (print_ident inp))
          (cond
            ((inp1-in '("obj" "object" "th" "theory"))
             (ci$!process_definition inp))
            ((inp1-in '("red" "reduce"))
             (ci$perform_reduction inp))
            ((inp1-in '("ev" "eval" "evq" "eval-quiet"))
             (let ((val (eval inp2)))
               (setq $ val)
               (unless (or *obj$input_quiet* (inp1-in '("evq" "eval-quiet")))
                 (u:princn "-> ")
	         (u:prin1n val))))
            ((inp1= "make")             ;just an abbreviation
             (let ((flag (equal "is" inp3)))
               (let ((name (nth 1 inp))
	             (params (if flag nil (list inp3)))
	             (modexp (if flag inp4 inp5)))
	         (ci$!process_definition
	          `("obj" ,name ,@params "is" (("pr" ,modexp ".")) "endo")))))
            ((inp1= "view")
             (mod_eval$view_eval inp))
            ((inp1-in '("rl" "red-loop"))
             (let ((arg inp2))
               (ci$red_loop (if (equal "." arg)
                                *mod_eval$$last_module*
                                arg))))
            ((inp1-in '("in" "input"))
             (let ((filename inp2))
               (unless (or *top-level* *obj$input_quiet*)
                 (format t "Reading in file : ~s~%" filename))
               (setq *in-in* t)
               (obj_input (namestring (u:rel-to-cwd filename)))
               (unless (or *top-level* *obj$input_quiet*)
                 (format t "Done reading in file: ~s~%" filename))))
            ((inp1= "test")
             (ci$perform_test_reduction inp))
            ((inp1= "call-that")
             (if *mod_eval$open_module*
	         (let ((obj$current_module *mod_eval$open_module*)
		       (*mod_eval$$current_module* *mod_eval$open_module*))
	           (module$!mark_as_needs_parse_setup *mod_eval$$current_module*)
	           (mod_eval$$include_BOOL)
	           (mod_eval$$!do_let (list "let" inp2 "=" nil "."))
	           (module$!mark_as_needs_parse_setup *mod_eval$$current_module*)
	           (when (module$is_compiled *mod_eval$$current_module*)
	             (module$!mark_as_needs_compiling *mod_eval$$current_module*)))
	         (progn (princ "Warning: no module open") (terpri))))
            ((inp1= "parse")
             (unless *obj$input_quiet*
               (cond
                 (*mod_eval$$last_module*
	          (mod_eval$$!do_parse_setup *mod_eval$$last_module*)
	          (let ((res (parse$parse *mod_eval$$last_module*
		                          (obj$obj2_term inp2) *obj$sort_Universal*)))
	            (setq $$term res)
	            (unless *top-level* (princ "parse "))
	            (print$short_sort_name (term$sort res))
	            (princ ": ")
	            (let ((*fancy-print* nil))
	              (term$print res) (terpri))))
                 (t
	          (princ "No current module")))))
            ((inp1-in mod_elts)
             (cond (*mod_eval$open_module*
	            (let ((obj$current_module *mod_eval$open_module*)
		          (*mod_eval$$current_module* *mod_eval$open_module*))
	              (mod_eval$!module_element inp)))
	           (t
	            (princ "Warning: no module open")
                    (terpri))))
            ((inp1= "openr")
             (setq *mod_eval$last_before_open* nil)
             (mod_eval$!open inp))
            ((inp1= "open")             ;just an abbreviation
             (when *mod_eval$open_module*
	       (princ "Warning: module already open: ")
	       (print$name *mod_eval$open_module*) (terpri)
	       (princ "Closing this module") (terpri)
	       (close_module inp)
	       (setq *mod_eval$$current_module* *mod_eval$$last_module*))
             (when (not (null inp2))
	       (setq *mod_eval$$last_module*
	             (modexp_eval$top_level_eval
	              inp2 *mod_eval$$current_module*)))
             (setq *mod_eval$last_before_open* *mod_eval$$last_module*)
             (let ((*obj$allow_uninstantiated* t))
               (if (eq 'object (module$kind *mod_eval$$last_module*))
	           (ci$!process_definition
	            `("obj" "%" "is" (("inc" ("THE-LAST-MODULE") ".")) "endo"))
	           (ci$!process_definition
	            `("th" "%" "is" (("inc" ("THE-LAST-MODULE") ".")) "endth"))))
             (let ((obj$current_module *mod_eval$$last_module*))
               (let ((*mod_eval$$last_module*
	               (caar (module$sub_modules
		              *mod_eval$$last_module*))))
                 (mod_eval$$!add_vars_of '("vars-of" "."))))
             (mod_eval$!open nil))
            ((inp1= "close")
             (close_module inp))
                                        ;     ((inp1= "apply") ;@@
                                        ;      (ci$!apply_rule inp))
            ((inp1= "start")            ;@@ put above module_elements?
             (misc$start inp))
                                        ;     ((inp1= "start-term")
                                        ;      (misc$start_term))
            ((inp1= "apply")
             (misc$apply inp))
                                        ;lhh -- insert the compile command
            ((inp1= "compile")
             (misc$compile inp))
                                        ;lhh -- insert the run command
            ((inp1= "run") 
             (misc$run inp))
            ((inp1-in '("show" "sh" "set" "do" "select"))
             (unless (and *obj$input_quiet*
	                  (inp1-in '("show" "sh")))
               (top$commands inp)))
            ((inp1= "[")
             (setq *obj$current_labels* (mod_eval$process_labels inp2)))
            ((inp1= '("?"))
             (obj_top_level_help))
            ((inp1= "pwd")
             (princ (uiop:getcwd))
             (terpri))
            ((inp1= "ls")
             (map nil (lambda (pathname)
                        (u:fmt "~A~%" pathname))
                  (append (directory "*")
                          (uiop:directory-files (uiop:getcwd) "*.*"))))
            ((inp1= "cd")
             (let ((fn (expand_file_name inp2)))
               (if (probe-file fn)
                   (u:chdir fn (string= fn (namestring (uiop:getcwd))))
                   (u:fmt "Directory not found ~S~%" fn)))))
          (setq *obj$print_errors* t)))
      inp)))


(defun close_module (inp)
  (let ((saved_open *mod_eval$open_module*))
    (mod_eval$!close inp)
    (when (and saved_open (equal "%" (module$name saved_open)))
      (modexp_eval$delete_module_all saved_open)
      (setq *mod_eval$$last_module* *mod_eval$last_before_open*)
      ))
  (setq *mod_eval$$current_module* *mod_eval$$last_module*)
  (setq *mod_eval$last_before_open* nil))
  
(defvar mod_elts '(
  "dfn" "define"  "ex" "extending"   "pr" "protecting"  "us" "using"
  "inc" "including"  "sort" "sorts"  "bsort"  "psort" "principal-sort"
  "subsort" "subsorts"  "op" "ops"  "let" "var" "vars"  "vars-of"
  "as"  "op-as"  "eq"  "ceq" "cq"  "beq" "bq"  "cbeq" "cbq"
  ))

(defun print_ident (e)
  (cond
   ((or (equal "red" (car e)) (equal "reduce" (car e))))
   ((or (equal "--->" (car e)) (equal "***>" (car e)))
    (print$simple_princ_open e) (terpri))
   ((or (equal "---" (car e)) (equal "***" (car e))))
   ((or (equal "ev" (car e)) (equal "eval" (car e)))
    (princ (car e)) (princ " ") (prin1 (cadr e)) (terpri))
   ((or (equal "evq" (car e)) (equal "eval-quiet" (car e)))
    (princ (car e)) (terpri))
   ((member (car e) '("show" "sh" "mod" "set" "do" "select" "open" "openr")
	    :test #'equal)
    (princ (car e)) (princ " ")
    (when (cadr e) (print$simple_princ_open (cadr e)))
    (terpri))
   ((equal "." (car e)))
   ((or (equal "apply" (car e)) (equal "start" (car e)))
    ) ; nothing
   ((member (car e) mod_elts :test #'equal)
    (print$simple_princ_flat e) (terpri))
   ((equal "[" (car e)) ; nothing
    )
; lhh - make the compile command print nicely
   ((equal "compile" (car e)) 
    (princ (car e)) (princ " ")
    (when (cadr e) (print$simple_princ_open (cadr e)))
    (terpri)
   )
; lhh - make the run command print nicely
   ((equal "run" (car e)) 
    (princ (car e)) (princ " ")
    (when (cadr e) (print$simple_princ_open (cadr e)))
    (terpri)
   )
   (t
    (princ (car e))
    (when (cadr e) (princ " ") (princ (cadr e)))
    (terpri))))

(defun debug_setup (f l)
  (setq $$debug f  $$debug_level l))

(defun dbg ()
  (debug_setup t 30))

(defun dbn ()
  (debug_setup nil 0))

(defun obj3-init ()
  (reader$!read_init)
  (general_read$$!init))

(defun obj3-break ()
  (terpri)
  (obj3-indicate-position)
  (princ "returning to top level") (terpri)
  (throw 'obj3-top-level-error t))

(defun obj3-to-top ()
  (obj3-indicate-position)
  (princ "returning to top level") (terpri)
  (throw 'obj3-top-level-error t))

; lhh -- something wrong with our trim command:
(defun recover-trim-error ()
  (princ "returning to top level") (terpri)
  (throw 'obj3-top-level-error t))

(defun obj3-return-to-top ()
  (throw 'obj3-top-level-error t))

(defun obj3-indicate-position ()
  (when *obj$input_source* ; nil means may be from terminal
    (princ "filename: ") (princ *obj$input_source*)
  (when (file-position #-CLISP *standard-input* #+CLISP (make-stream :input))
    (princ " in top-level form ending at character position: ")
    (prin1 (file-position #-CLISP *standard-input* #+CLISP (make-stream :input))))
  (terpri)))

