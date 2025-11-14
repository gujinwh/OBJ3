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
