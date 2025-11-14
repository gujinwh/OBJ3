(defvar *in-in*)
(defvar *top-level*)

(defun process_input ()
  (let ((reader$$ch 'space)
	(*general_read$$input* *general_read$$void*)
	(*top-level* (at-top-level)))
    ;;(reader$!read_init) ;@@ (take out for obj LISP)
    (memo_rew$create_memo_table)
    (let (inp (*in-in* nil))
      (loop (the-loop inp)
        (when *obj$postcmd_hook*
          (funcall *obj$postcmd_hook* *top-level* inp)
          )
        (when *in-in*
          (setq *obj$print_errors* t)
          (setq *in-in* nil))))))

(defun the-loop (inp)
  (catch (if *top-level* 'obj3-top-level-error 'obj3-main-error)
  (catch 'obj3-error
    (when (and *top-level*
	       *obj$prompt_hook*)
      (funcall *obj$prompt_hook* *top-level*))

    (setq inp (module_parse$parse)) ;; lhh -- does the command parsing

    (when *obj$precmd_hook*
      (funcall *obj$precmd_hook* *top-level* inp))
   
    (when (or (equal '("eof") inp) (equal '("q") inp) (equal '("quit") inp)
	      (equal '(eof) inp) (equal *reader$eof_value* inp))
      (return-from the-loop))
    (unless (or *top-level* *obj$input_quiet*)
      (unless (or (equal "---" (car inp)) (equal "***" (car inp))
		  (equal "?" (car inp)) (equal "[" (car inp)))
        (fresh-line)
        (princ "==========================================") (terpri))
      (print_ident inp))
    (cond
      ((member (car inp) '("obj" "object" "th" "theory") :test #'equal)
       (ci$!process_definition inp))
      ((member (car inp) '("red" "reduce") :test #'equal)
       (ci$perform_reduction inp))
      ((member (car inp) '("ev" "eval" "evq" "eval-quiet") :test #'equal)
       (let ((val (eval (cadr inp))))
         (setq $ val)
         (unless  (or *obj$input_quiet*
		      (equal "evq" (car inp)) (equal "eval-quiet" (car inp)))
           (princ "-> ") (terpri)
	   (prin1 val) (terpri))))
      ((equal "make" (car inp))         ;just an abbreviation
       (let ((flag (equal "is" (nth 2 inp))))
         (let ((name (nth 1 inp))
	       (params (if flag nil (list (nth 2 inp))))
	       (modexp (nth (if flag 3 4) inp)))
	   (ci$!process_definition
	    `("obj" ,name ,@params "is" (("pr" ,modexp ".")) "endo")))))
      ((equal "view" (car inp))
       (mod_eval$view_eval inp))
      ((member (car inp) '("rl" "red-loop") :test #'equal)
       (let ((arg (cadr inp)))
         (ci$red_loop (if (equal "." arg) *mod_eval$$last_module* arg))))
      ((member (car inp) '("in" "input") :test #'equal)
       (let ((filename (cadr inp)))
         (unless (or *top-level* *obj$input_quiet*)
           (format t "Reading in file : ~s~%" filename))
         (setq *in-in* t)
         (obj_input (namestring (rel-to-cwd filename)))
         (unless (or *top-level* *obj$input_quiet*)
           (format t "Done reading in file: ~s~%" filename))))
      ((equal (car inp) "test")
       (ci$perform_test_reduction inp))
      ((equal "call-that" (car inp))
       (if *mod_eval$open_module*
	   (let ((obj$current_module *mod_eval$open_module*)
		 (*mod_eval$$current_module* *mod_eval$open_module*))
	     (module$!mark_as_needs_parse_setup *mod_eval$$current_module*)
	     (mod_eval$$include_BOOL)
	     (mod_eval$$!do_let (list "let" (cadr inp) "=" nil "."))
	     (module$!mark_as_needs_parse_setup *mod_eval$$current_module*)
	     (when (module$is_compiled *mod_eval$$current_module*)
	       (module$!mark_as_needs_compiling *mod_eval$$current_module*)))
	   (progn (princ "Warning: no module open") (terpri))))
      ((equal "parse" (car inp))
       (unless *obj$input_quiet*
         (if *mod_eval$$last_module*
	     (progn
	       (mod_eval$$!do_parse_setup *mod_eval$$last_module*)
	       (let ((res (parse$parse *mod_eval$$last_module*
		                       (obj$obj2_term (cadr inp)) *obj$sort_Universal*)))
	         (setq $$term res)
	         (unless *top-level* (princ "parse "))
	         (print$short_sort_name (term$sort res))
	         (princ ": ")
	         (let ((*fancy-print* nil))
	           (term$print res)
	           (terpri)
	           )))
	     (progn (princ "No current module") (terpri))
             )))
      ((member (car inp) mod_elts :test #'equal)
       (if *mod_eval$open_module*
	   (let ((obj$current_module *mod_eval$open_module*)
		 (*mod_eval$$current_module* *mod_eval$open_module*))
	     (mod_eval$!module_element inp)
	     )
	   (progn
	     (princ "Warning: no module open") (terpri)
	     ))
       )
      ((equal "openr" (car inp))
       (setq *mod_eval$last_before_open* nil)
       (mod_eval$!open inp))
      ((equal "open" (car inp))         ;just an abbreviation
       (when *mod_eval$open_module*
	 (princ "Warning: module already open: ")
	 (print$name *mod_eval$open_module*) (terpri)
	 (princ "Closing this module") (terpri)
	 (close_module inp)
	 (setq *mod_eval$$current_module* *mod_eval$$last_module*)
	 )
       (when (cadr inp)
	 (setq *mod_eval$$last_module*
	       (modexp_eval$top_level_eval
	        (cadr inp) *mod_eval$$current_module*)))
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
       (mod_eval$!open nil)
       )
      ((equal "close" (car inp))
       (close_module inp))
                                        ;     ((equal "apply" (car inp)) ;@@
                                        ;      (ci$!apply_rule inp))
      ((equal "start" (car inp))        ;@@ put above module_elements?
       (misc$start inp))
                                        ;     ((equal "start-term" (car inp))
                                        ;      (misc$start_term))
      ((equal "apply" (car inp))
       (misc$apply inp))
                                        ;lhh -- insert the compile command
      ((equal "compile" (car inp))
       (misc$compile inp))
                                        ;lhh -- insert the run command
      ((equal "run" (car inp)) 
       (misc$run inp))
      ((member (car inp) '("show" "sh" "set" "do" "select")
	       :test #'equal)
       (unless (and
	        *obj$input_quiet*
	        (or (equal "show" (car inp)) (equal "sh"(car inp))))
         (top$commands inp))
       )
      ((equal "[" (car inp))
       (setq *obj$current_labels* (mod_eval$process_labels (cadr inp))))
      ((equal '("?") inp)
       (obj_top_level_help))
                                        ; DANGER these are system dependent
      ((equal "pwd" (car inp))
       (princ (uiop:getcwd))
       (terpri))
      ((equal "ls" (car inp))
       (map nil (lambda (pathname)
                  (u:fmt "~A~%" pathname))
            (uiop:directory-files (uiop:getcwd))))
      ((equal "cd" (car inp))
       (uiop:chdir "../")
       (let ((fn (expand_file_name (cadr inp))))
         (if (probe-file (concatenate 'string fn "/")) (uiop:chdir fn)
             (print "Directory not found")))))
    (setq *obj$print_errors* t)
    )))
