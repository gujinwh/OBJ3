(in-package #:user)

(defun fmt (&rest args)
  (apply #'format t args)
  (finish-output t))

(defun rel-to-cwd (filespec)
  (merge-pathnames filespec
                   (uiop:getcwd)))

(defun rel-to-src (filespec)
  (merge-pathnames filespec
                   #.(or *compile-file-truename* *load-truename*)))
