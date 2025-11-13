(defpackage :user
  (:use :cl)
  (:shadow #:sort))

(in-package :user)
(defvar +version+ "OBJ3 (2.09) + TRIM")
(defparameter *version* '(:obj3)) 

(defun print_char (n ch)           ;; files "obj3-compile" and 
  (dotimes (i n) (princ ch)))      ;; "obj3-load" call this function

(setq *print-case* :downcase)

(setq *load-verbose* t)
          (defvar *load-print-stuff* t)
          (setq *block-compile-default* nil)
          (setq *compile-verbose* t)
 (setq *compile-print* t)

