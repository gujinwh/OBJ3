(in-package #:obj3)

;;; variable.lisp
; Returns the initial sort of the variable (gived by the user)
; For the rules given by the user, the variable appearing in the left hand 
; side have only one sort.

; op variable$initial_sort Variable -> Sort .
(defmacro variable$initial_sort (var)
  `(car (variable$sorts ,var))
  )

; Returns the list of sorts of the variable "var"

; op variable$sorts: Variable -> List[Sort] .
(defmacro variable$sorts (var)
  `(caddr ,var)
  )

; op variable$name: Variable -> Name
(defmacro variable$name (var)
  `(car ,var)
  )

;;; term.lisp
;;; op term$built_in_value : Term -> Value
; op term$is_var = term -> bool
;(defun term$is_var (term)
;  (atom (car term))
;  )
(defmacro term$is_var (term)
  `(atom (car ,term)))

; op term$is_constant : term -> bool
(defmacro term$is_constant (term)
  `(null (cdr ,term)))

;% returns a term f(t1,...,tn) from the operator "f" and the
;% list of terms (t1,...,tn).
;% It is very important to notice that f is supposed to be THE operator
;% such that f(t1,...tn) is well lowest parsed.
;% --- "free%maketerm" in obj2

; op term$make_term : operator {subterms}list[term] -> term
(defmacro term$make_term (f subterms)
  `(cons (cons ,f 1) ,subterms))

;% returns the top operator of a term if it is a non variable one.
;% --- "%top" in obj2

; op term$head : term -> operator; signals(is_variable)
;(defun term$head (term)
;  (caar term)
;  )
(defmacro term$head (term)
  `(caar ,term))

;% returns the list of subterms of the term t. For example
;% if t = f(t1,...,tn) then it returns the list t1,..., tn.

; op term$subterms  : term -> list[term]; signals(is_a_variable)
;(defun term$subterms (term)
;  (cdr term)
;  )
(defmacro term$subterms (term)
  `(cdr ,term))


(defmacro term$built_in_value (term)
  `(car (cdar ,term)))

; macro version of term$is_built_in_constant
(defmacro term$is_built_in_constant_fast (term)
  `(and (consp (car ,term))
	(consp (cdar ,term))))

(defmacro term$equal_built_in_fast (t1 t2)
  `(let ((v1 (car ,t1)) (v2 (car ,t2)))
    (and (eq (car v1) (car v2))
	 (equal (cadr v1) (cadr v2))))
  )

;% returns the first son of the term t. Example if t = f(t1,...,tn)
;% then it returns t1

; op term$arg_1 : term -> term; signals(no_subterm)
(defmacro term$arg_1 (term)
  `(cadr ,term)
  )
  
;% Same thing with the second component

; op term$arg_2 : term -> term; signals(no_subterm)
(defmacro term$arg_2 (term)
  `(caddr ,term)
  )


;;; operator.lisp
;;; access to information
;;; op operator$name: Operator -> Name .
(defmacro operator$name (op)
 `(operator-name ,op)
  )

; op operator$arity: Operator -> List[Sort]
(defmacro operator$arity (op)
  `(operator-arity ,op)
  )

; op operator$coarity: Operator -> Sort .
(defmacro operator$coarity (op)
  `(operator-coarity ,op)
  )

; op operator$module: Operator -> Module .
(defmacro operator$module (op)
  `(operator-module ,op))

; op operator$intrinsic: Operator -> Value .
(defmacro operator$intrinsic (op)
  `(operator-intrinsic ,op))

; op operator$!update_intrinsic : Operator Operator-Info -> {Operator}
(defun operator$!update_intrinsic (op opinfo)
  (setf (operator$intrinsic op) opinfo)
  op
  )

; op operator$intrinsic_module: Operator -> Value .
(defmacro operator$intrinsic_module (op)
  `(operator-intrinsic_module ,op))

; op operator$status: Operator -> Value .
(defmacro operator$status (op)
  `(operator-status ,op))

(defmacro operator$$chk (mod) mod) ;24 Feb 88

; op operator$is_associative : Operator -> Bool .
(defmacro operator$is_associative (op)
  `(theory$contains_associativity (operator$theory ,op))
  )

; op operator$is_identity : Operator -> Bool .
(defmacro operator$is_identity (op)
  `(theory$contains_identity (operator$theory ,op))
  )

; specially optimized version of last for use in rew$!reduce
(defmacro operator$is_associative_fast (op)
  `(test-flag A_flag
    (theory_info$code
     (theory$name
      (operator_info$equational_theory
       (optable$operator_info
	(module$operator_table obj$current_module) ,op))))))

(defmacro operator$is_same_operator_fast (opx opy)
  `(and (eq (operator$name ,opx) (operator$name ,opy))
       (let ((opxnm (operator$name ,opx)))
       (or (not (eq (car opxnm) 'retract))
	   (and
	    (eq (operator$coarity ,opx) (operator$coarity ,opy))
	    (eq (car (operator$arity ,opx)) (car (operator$arity ,opy)))
	    ))))
  )


;;; ../match/theory_name.lisp
(defmacro theory_name$is_empty (th_name)
  `(eq the_empty_property ,th_name))
       
(defmacro theory_name$is_A (th_name)
  `(eq the_A_property ,th_name))

(defmacro theory_name$is_C (th_name)
  `(eq the_C_property ,th_name))

(defmacro theory_name$is_I (th_name)
  `(eq the_I_property ,th_name))

(defmacro theory_name$is_Z (th_name)
  `(eq the_Z_property ,th_name))

(defmacro theory_name$is_AC (th_name)
  `(eq the_AC_property ,th_name))

(defmacro theory_name$is_AI (th_name)
  `(eq the_AI_property ,th_name))

(defmacro theory_name$is_AZ (th_name)
  `(eq the_AZ_property ,th_name))

(defmacro theory_name$is_CI (th_name)
  `(eq the_CI_property ,th_name))

(defmacro theory_name$is_CZ (th_name)
  `(eq the_CZ_property ,th_name))

(defmacro theory_name$is_IZ (th_name)
  `(eq the_IZ_property ,th_name))

(defmacro theory_name$is_ACI (th_name)
  `(eq the_ACI_property ,th_name))

(defmacro theory_name$is_ACZ (th_name)
  `(eq the_ACZ_property ,th_name))

(defmacro theory_name$is_AIZ (th_name)
  `(eq the_AIZ_property ,th_name))

(defmacro theory_name$is_CIZ (th_name)
  `(eq the_CIZ_property ,th_name))

(defmacro theory_name$is_ACIZ (th_name)
  `(eq the_ACIZ_property ,th_name))

;;; match_equation.lisp
(defmacro match_equation$t1 (eq)
  `(car ,eq))

(defmacro match_equation$t2 (eq)
  `(cdr ,eq))



