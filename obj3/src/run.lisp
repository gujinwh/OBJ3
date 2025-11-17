(in-package #:obj3)

(defvar obj3-load-time (get-time-string))

(defparameter *prelude-file*
  (u:rel-to-src "prelude/obj3sp.obj"))

(obj3-init)
(print '-----------------------------------------------------------------------------)
(obj_input (namestring *prelude-file*))

(defun obj3-top-level ()
  (timer)
  (let ((res (catch *top-level-tag* (obj3)
                    'ok-exit)))
    (cond
      ((eq res 'ok-exit) #+c(uiop:quit))
      (t (format t "OBJ3 loading error~%")
         (terpri)))))

(u:fmt "Now type (progn (in-package :obj3) (obj3::obj3-top-level)) to begin.")

(defun start-obj3 ()
  (obj3-main (uiop:command-line-arguments)))

(defun obj3-main (command-line-arguments)
  ;; lhh -- init some global stuff
  (setq *unique-string* (convert-to-string (get-universal-time)))
  (setq *obj-unique-filename* (concatenate 'string "/tmp/OBJ" *unique-string*))

  (obj3-init)
  #+LUCID (buffer-line)
  (obj3-greeting)
  (when (uiop:getenv "OBJ3-TIMING") (setq $$time-red t))
  #+LUCID (when (uiop:getenv "OBJ3-BATCH") (buffer-on))
  
  (flet ((argv (i)
           (nth (1- i) command-line-arguments)))
    (in-package #:obj3)
    ;; (print command-line-arguments)
    (catch *top-level-tag*
      (catch 'obj3-top-level-error
        (catch 'obj3-error
          (do ((argc (length command-line-arguments))
               (i 1 (1+ i)))
              ((>= i argc))
            (u:scase (argv i)
              ("-in"
               (obj_input (argv (incf i))))
              ("-inq"
               (let ((*obj$input_quiet* t))
	         (obj_input (argv (incf i)))))
              ("-evq"
               (load_file (argv (incf i))))))))))
  (obj3::obj3-top-level))

(defun obj3 ()
  (let ((quit-flag nil))
    (loop
      (catch *top-level-tag*
        (with-simple-restart (ignore-error "Ignore error and return to obj3 toplevel")
          (process_input)
          (setq quit-flag t)))
      (when quit-flag (return))))                           
  #+LUCID(finish-output))

(defparameter *version* '(:obj3)) ; used by the macro alt which is similar to #+ #-
(defvar obj3-version "2.11")

(defun obj3-greeting ()
  (unless (uiop:getenv "OBJ3-QUIET")
  (sp 25) (u:princn " \\|||||||||||||||||/")
  (sp 24) (u:princn "--- Welcome to OBJ3 ---")
  (sp 25) (u:princn " /|||||||||||||||||\\")
  (sp 7) (u:fmt "OBJ3 (with TRIM) version ~A built: ~A~%"  obj3-version obj3-load-time)
  (u:princn "OBJ3 2.06,2.08,2.09 Copyright (c) 2000-2003 Joseph Kiniry, Joseph Goguen")
  (sp 10) (u:princn "OBJ3 2.05 (c) 2000 Sula Ma, Joseph Kiniry, Joseph Goguen")
  (sp 12) (u:princn "OBJ3 2.04 Copyright 1988,1989,1991 SRI International")
  (sp 18) (u:princn "TRIM Copyright (c) 1994,2001 Lutz Hamel")
  (sp 24) (u:princn (get-time-string))))

; lhh -- help screen for compile command.
(defun obj_top_level_help ()
  (u:fmt "Top-level definitional forms include: obj, theory, view, make
The top level commands include:
  q; quit --- to exit from OBJ3
  show .... .  --- for further help: show ? .
  set .... . --- for further help: set ? .
  do .... . --- for further help: do ? .
  apply .....  --- for further help: apply ? .
  other commands:
    in <filename>
    red <term> .
    run [verbose|keep] <term> .
    compile [verbose|noopt|keep] [<module-expression>] .
    select <module-expression> .
    cd <directory>; ls; pwd
    start <term> .; show term .
    open [<module-expression>] .; openr [<module-expression>] .; close
    ev <lisp>; evq <lisp>"))

(defun obj3-init-files ()
  (let ((*obj$input_quiet* t))
    (if (probe-file "./.obj3")
        (obj_input_file "./.obj3")
        (let ((home (or (namestring (user-homedir-pathname))
		        (uiop:getenv "HOME"))))
	  (unless (null home)
	    (let ((dot-obj3 (concatenate 'string home "/.obj3")))
	      (when (probe-file dot-obj3) (obj_input_file dot-obj3)))))))
  (let ((val (uiop:getenv "OBJ3INIT")))
    (when (and val (probe-file val)) (obj_input_file val))))
