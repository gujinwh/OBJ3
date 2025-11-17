(defpackage #:obj3.util
  (:use :cl)
  (:export
   #:trim-spaces
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

#|
(setq *print-case* :downcase)
(setq *load-verbose* t)
(setf *compile-verbose* t)
(setq *compile-print* t)
|#
