(defpackage #:obj3.util
  (:use :cl)
  (:export
   #:fmt
   #:princn
   #:prin1n
   #:rel-to-cwd
   #:rel-to-src
   #:scase
   #:chdir))

(defpackage :obj3
  (:use :clsafe)
  (:shadow #:sort)
  (:local-nicknames (#:u #:obj3.util))
  (:export ))

(in-package :obj3)
(defvar +version+ "OBJ3 (2.09) + TRIM")
(defparameter *version* '(:obj3)) 

(defun print_char (n ch)           ;; files "obj3-compile" and 
  (dotimes (i n) (princ ch)))      ;; "obj3-load" call this function

(setq *print-case* :downcase)

(setq *load-verbose* t)
          (defvar *load-print-stuff* t)
          (defparameter *block-compile-default* nil)
          (setf *compile-verbose* t)
 (setq *compile-print* t)

