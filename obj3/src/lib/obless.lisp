;; OBJ3 version 2
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

;; $Id: obless.lsp,v 206.1 2003/09/26 13:02:36 kiniry Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Ordering on arbitrary objects
; DANGER -- this is implementation dependent
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; this isn't quite right but the type system of CL is so complex
; that it would be very difficult to get this just right

; type-of values for GCL
;fixnum ratio long-float bignum
;complex
;standard-char string-char character
;symbol keyword
;string vector simple-vector simple-string bit-vector simple-bit-vector
;cons
;array simple_array
;compiled-function random-state pathname hash-table package read-table stream
(in-package #:obj3)
; ordering
;  symbol<cons<number<character<complex<string<sequence<structure

; fixnum/bignum < symbol < cons < othernumber

(defun ob< (x y) (eq 'lt (ob-compare x y)))

(defun define-type-predicate (type)
  (compile (intern (string-upcase (concatenate 'string (symbol-name type) "p")))
           (lambda (object)
             (typep object type))))

(dolist (type '(fixnum bignum ratio short-float long-float sequence))
  (define-type-predicate type))

(defun structurep (object)
  (let ((structure-type #+SBCL 'structure-object
                        #-SBCL 'structure))
    (typep object structure-type)))

(defun ob-compare (x y)
  #-(or GENERIC CMU CLISP GCL SBCL) (error "TODO: Implement it")
  (let ((typex (type-of x))
        (typey (type-of y)))
    (if (and (fixnump x) (fixnump y))
        (if (< (the fixnum x) (the fixnum y))
            'lt
            (if (< (the fixnum y) (the fixnum x))
                'gt
                'eq))
        (if (symbolp x)
            (if (symbolp y)
                (if (string< (string x) (string y)) 'lt
                    (if (string< (string y) (string x)) 'gt
                        'eq))
                'lt)
            (if (symbolp y)
                'gt
                (if (consp x)
                    (if (consp y)
                        (let ((cmp-car (ob-compare (car x) (car y))))
                          (if (eq 'eq cmp-car) (ob-compare (cdr x) (cdr y))
                              cmp-car))
                        'lt)
                    (if (or (fixnump x) (bignump x)
                            (ratiop x) (short-floatp x) (long-floatp x))
                        (if (or  (fixnump y) (bignump y)
                                 (ratiop y) (short-floatp y) (long-floatp y))
                            (if (< x y) 'lt (if (< y x) 'gt 'eq))
                            'lt)
                        (if (or  (fixnump y) (bignump y)
                                 (ratiop y) (short-floatp y) (long-floatp y))
                            'gt
                            (if (characterp x)
                                (if (characterp y) (if (char< x y) 'lt (if (char< y x) 'gt 'eq)) 'lt)
                                (if (characterp y)
                                    'gt
                                    (if (complexp x)
                                        (if (complexp y)
                                            (let ((rx (realpart x)) (ry (realpart y)))
                                              (if (< rx ry) 'lt (if (< ry rx) 'gt
                                                                    (let ((ix (imagpart x)) (iy (imagpart y)))
                                                                      (if (< ix iy) 'lt (if (< iy ix) 'gt 'eq))))))
                                            'lt)
                                        (if (complexp y) 'gt
                                            (if (stringp x)
                                                (if (stringp y) (if (string< x y) 'lt (if (string< y x) 'gt 'eq)) 'lt)
                                                (if (stringp y) 'gt
                                                    (if (typep x 'sequence)
                                                        (if (sequencep y)
                                                            (let ((lenx (length x))  (leny (length y)))
                                                              (dotimes (i (min lenx leny) (ob-compare lenx leny))
                                                                (let ((xi (elt x i))  (yi (elt y i)))
                                                                  (let ((cmp (ob-compare xi yi)))
                                                                    (unless (eq 'eq cmp) (return cmp))))))
                                                            'lt)
                                                        ;; CMU CL and CLISP provides no (obvious) type predicate for structures and
                                                        ;; structures are typep 'symbol, thus this code will never be run.
                                                        #-(or LUCID GCL SBCL LISPWORKS) (assert nil)
                                                        #+(or LUCID GCL SBCL LISPWORKS)
                                                        (if (sequencep y) 'gt
                                                            (if (structurep x)
                                                                (if (structurep y)
                                                                    (let* ((slots-x (object-slots x)
                                                                                    )
                                                                           (slots-y (object-slots y))
                                                                           (lenx (length x))
                                                                           (leny (length y)))
                                                                      (dotimes (i (min lenx leny) (ob-compare lenx leny))
                                                                        (let* ((xi (nth-slot-value i x slots-x))
                                                                               (yi (nth-slot-value i y slots-y))
                                                                               (cmp (ob-compare xi yi)))
                                                                          (unless (eq 'eq cmp)
                                                                            (return cmp)))))
                                                                    'lt)
                                                                (if (structurep y)
                                                                    'gt
                                                                    (if (string-lessp (string typex) (string typey))
                                                                        'lt
                                                                        (if (string-lessp (string typey) (string typex))
                                                                            'gt
                                                                            (let ((xa (addr_of x)) (ya (addr_of y)))
                                                                              (if (< xa ya) 'lt
                                                                                  (if (< ya xa) 'gt
                                                                                      'eq)))))))))))))))))))))))

(defun object-slots (object)
  #-(or GENERIC CMU CLISP GCL SBCL LISPWORKS) (error "TODO: Implement it")
  #+LUCID (sys:objecture-length x nil)
  #+GCL (length
         #-AKCL (get (type-of object) 'si:slot-descriptions)
         #+AKCL (system::s-data-slot-descriptions
                 (get (type-of object) 'system:s-data)))
  #+(or SBCL LISPWORKS)
  (let ((class (find-class (type-of object))))
    (#+SBCL sb-mop:class-direct-slots
     #+LISPWORKS structure:structure-class-slot-names
     class)))

(defun number-of-slots (object)
  (length (object-slots object)))

(defun nth-slot-value (n object slots)
  #-(or LUCID GCL SBCL LISPWORKS) (error "TODO: Implement it")
  #+LUCID (sys:structure-ref x i nil)
  #+GCL (si:structure-ref x typex i)
  #+(or SBCL LISPWORKS)(slot-value object (nth n slots)))
