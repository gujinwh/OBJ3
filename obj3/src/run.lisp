(in-package #:user)

(defvar obj3-load-time (get-time-string))

(defparameter *prelude-file*
  (rel-to-src "prelude/obj3sp.obj"))

(obj3-init)
(print '-----------------------------------------------------------------------------)
(obj_input (namestring *prelude-file*))

(defun obj3-top-level ()
  (timer)
  (let ((res (catch *top-level-tag* (obj3) 'ok-exit)))
    (if (eq res 'ok-exit)
        #+GCL (bye) #+CMU (quit)
        (progn
          (format t "OBJ3 loading error~%")
          (terpri)))))

(fmt "Now type (progn (in-package :user) (user::obj3-top-level)) to begin.")

#+c(swank-repl::repl-eval )
#+c(swank:eval-in-emacs `(slime-repl-set-package "USER"))
#+c(swank:eval-in-emacs `(sl-eval-in-repl nil "(in-package :user)"))

#+notwork(swank:eval-in-emacs `(sl-eval-in-repl nil "(user::obj3-top-level)"))
