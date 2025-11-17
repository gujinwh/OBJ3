(in-package #:obj3.util)

(defun trim-spaces (string &optional (spaces '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)))
  (string-trim spaces string))

(defun fmt (&rest args)
  (apply #'format t args)
  (finish-output t))

(defun princn (object &optional output-stream)
  (write object :stream output-stream :escape nil :readably nil) (terpri))

(defun prin1n (object &optional output-stream)
  (prin1 object output-stream) (terpri))

(defun rel-to-cwd (filespec)
  (merge-pathnames filespec
                   (uiop:getcwd)))

(defmacro rel-to-current-file (filespec)
  `(merge-pathnames ,filespec
                    ,(or *compile-file-truename* *load-truename*)))

(defun rel-to-src (filespec)
  (merge-pathnames filespec
                   #.(or *compile-file-truename* *load-truename*)))

(defmacro define-string-case-macro (name test &key error)
  `(defmacro ,name (keyform &body clauses)
     (let ((key (gensym "KEY")))
       `(let ((,key ,keyform))
          (cond
            ,@(mapcar
               (lambda (clause)
                 (let ((first (car clause)))
                   (if (member first '(otherwise t))
                       `(,first ,@(cdr clause))
                       `(,(etypecase first
                            ((or string symbol character)
                             `(,',test ,key ',first))
                            (cons
                             `(member ,key ',first :test (function ,',test))))
                         ,@(cdr clause)))))
               (append clauses
                       (when ,error
                         `((t (error "~S fell through ecase expression. Wanted one of ~S"
                                     ,key
                                     ,(apply #'append (mapcar
                                                      (lambda (clause)
                                                        (let ((first (first clause)))
                                                          (if (atom first)
                                                              (list first)
                                                              first)))
                                                      clauses)))))))))))))

(define-string-case-macro scase string=)
;;; (define-string-case-macro iscase string-equal)
(define-string-case-macro escase string= :error t)

#|
(scase "1"
  (("2" "3") 'ok)
("4" 'ok2))

(escase "1"
  (("2" "3") 'ok)
  ("4" 'ok2))

(escase "1"
  (("2" "3") 'ok)
  ("4" 'ok2)
  ("1" 'ok3))
|#

(defun chdir (new-wd &optional (silent? t))
  #+c(when (stringp new-wd)
    (setf new-wd (parse-namestring new-wd)))
  (uiop:chdir new-wd)
  (setf *default-pathname-defaults*
        (parse-namestring (uiop:getcwd)))
					;(break "in perform :after: ~A" *default-pathname-defaults*)
  #-windows (uiop:chdir new-wd)
  (unless silent? (format t "Current working directory changed to: ~A~%"
                          *default-pathname-defaults*)))
