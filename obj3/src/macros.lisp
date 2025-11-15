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

;;; ../match/environment.lisp
;;; returns a copy of one level of the environment
;;; op environment$copy1: Environment -> Environment .
(defmacro environment$copy1 (env)
  `(copy-list ,env))

;;; ../match/acz.lisp
;; small utility.  Side effect.
(defmacro ACZ$$Rotate-Left (array m)
"; shifts the element one bit to the left"
  `(setf (aref ,array ,m)
	 (* 2 (aref ,array ,m))))

;; @note kiniry 25 Sept 2003 - delete-one-term defined in ac.lsp already.
;; (defun delete-one-term (x y)
;;   (if (term$equational_equal x (caar y)) (cdr y)
;;       (let ((last y) (rest (cdr y)))
;;         (loop
;;            (when (null rest) (return 'none))
;;            (when (term$equational_equal x (caar rest))
;;              (rplacd last (cdr rest))
;;              (return y))
;;            (setq last rest  rest (cdr rest))))))

(defmacro ACZ$$note-repeats (mset array max gcd)
"; puts all repeated terms together in the list, and bashes the array
 ; (into numbers) in locations corresponding to the duplicate terms. 
 ; returns the newly grouped permutation of list.
 ; e.g. for input (a b c c c d d e f f) and #(0 0 0 0 0 0 0 0 0),
 ; this should make the array into #(0 0 3 2 1 2 1 0 2 1)."
  `(let* ((list2 nil)
	 (counter (array-dimension ,array 0)) )
    (dolist (element ,mset)
	    (let ((n (cdr element)))
	      (declare (fixnum n))
	    (when (> n ,max)
		  (setq ,max n))
	    (setq ,gcd (gcd ,gcd n))
	    (if (> n 1) ; if it is repeated at all
		(dotimes-fixnum (x n)
			 (push (first element) list2)
			 (setq counter (1- counter))
			 (setf (aref ,array counter) (1+ x)))
	        (progn (push (first element) list2)
		       (setq counter (1- counter))
		       (setf (aref ,array counter) 0))))) ; this line optional
    list2))                                     ; (if 0'd array is guaranteed)

(defmacro ACZ$$eq-member (term list)
 "predicate. true if term is term$equational_equal some element of list"
  `(dolist (term2 ,list)
      (when (term$equational_equal ,term (car ,list))
	    (return t))))

;; don't even think of using this again
(defmacro ACZ$$collapse-one-array-internal (rhs-sol rhs-array)
  `(dotimes-fixnum (j (array-dimension ,rhs-sol 0))
	    (when (> (logand (aref ,rhs-sol j) term-code) 0)
		  (push  (car (aref ,rhs-array j)) rhs-subterms))))

;; don't even think of using this again
(defmacro ACZ$$collapse-arrays-internal (lhs skip) 
  `(dotimes-fixnum (i (array-dimension ,lhs 0))
      (if (< i ,skip)
          nil
          (progn 
	(setq rhs-subterms nil)
	(setq term-code (* 2 term-code))
	(ACZ$$collapse-one-array-internal rhs-c-sol rhs-c)
	(ACZ$$collapse-one-array-internal rhs-f-sol rhs-f)
;	(print$brief (car (aref ,lhs i)))
;       (map nil #'print$brief rhs-subterms)
	(system$add_eq 
	 new-sys 
	 (match_equation$create (car (aref ,lhs i))
                  (if (null rhs-subterms)
		    (term$make_zero (aref ops (cdr (aref ,lhs i))))
		  (if (cdr rhs-subterms) ; implies length is greater than 1
		      (term$make_right_assoc_normal_form_with_sort_check
		          (aref ops (cdr (aref ,lhs i))) rhs-subterms)
		      (first rhs-subterms)))))))))

;;; ../match/match_methods/lisp

;;;(defmacro term$is_var (x) `(atom (car ,x)))
;;; the following is okay only because of the simplicity of the uses of
;;; this function below
;;; % makes sure that this definition will only be used where chosen
  (defmacro term$%is_built_in_constant(x)
    `(and (consp (car ,x))
	  (consp (cdar ,x))))
;;;(defmacro term$head (x) `(caar ,x))
;;;(defmacro term$subterms (x) `(cdr ,x))

;&&&& delete failed
;(eval-when (eval)
;(defun substitution$%lookup (teta term)
;  (let ((val (assoc term teta)))
;    (if val (cdr val) nil)))
;)

(eval-when (compile eval)
(defmacro substitution$%lookup (teta term)
  (let ((temp (gensym)))
  `(let ((,temp (assoc ,term ,teta)))
     (if ,temp (cdr ,temp) nil))))
)

;;; ../match/match_equation.lisp
(defmacro match_equation$t1 (eq)
  `(car ,eq))

(defmacro match_equation$t2 (eq)
  `(cdr ,eq))

;;; ../match/theory.lisp

; multiple bits can be tested
(defmacro test-flag (x y)
  `(not (= 0 (logand ,x ,y))))



;;; ../match/state.lisp

; returns a new state
; op state$create : Match_System System Theory_name Theory_State -> State
(defmacro state$create (m_sys sys_to_solve th_name theory_state)
  `(make_state t ,m_sys ,sys_to_solve ,th_name ,theory_state))

;;; ../top/misc.lisp
(defmacro call-that (x)
  `(progn (setq ,x (term$copy_and_returns_list_variable $$norm)) 'done))

;;; ../top/saver.lisp

(defmacro save-var (var)
  `(progn (defvar ,var) (setq ,var ',(symbol-value var))))

(defmacro obj3-save-vars ()
 `(progn
(save-var obj3-load-time)
(save-var obj3-version)
(save-var obj3_status_env)

(save-var *modexp_eval$canon_env*)
(save-var *modexp_eval$env*)
(save-var *modexp_eval$view_env*)

(save-var *operator$name_table*)
(save-var *operator$retract_table*)
(save-var mod_eval$creation_counter)

(save-var the_empty_property)
(save-var the_Z_property)
(save-var the_I_property)
(save-var the_IZ_property)
(save-var the_C_property)
(save-var the_CZ_property)
(save-var the_CI_property)
(save-var the_CIZ_property)
(save-var the_A_property)
(save-var the_AZ_property)
(save-var the_AI_property)
(save-var the_AIZ_property)
(save-var the_AC_property)
(save-var the_ACZ_property)
(save-var the_ACI_property)
(save-var the_ACIZ_property)
(save-var theory$the_empty_theory)

(save-var obj_BUILT-IN$keyword)
(save-var obj_LISP$keyword)
(re-install-prelude)
  ; end of save-vars
  ))

;;; ../top/reader.lisp

(defmacro reader$!set_syntax (ch val)
  `(setf (aref *reader$$read_table* (char-code ,ch)) ,val))

(defmacro reader$get_syntax (ch)
  `(aref *reader$$read_table* (char-code ,ch)))

;;; note duplication of n
(defmacro reader$$valid_char_code (n)
  `(and (<= 0 ,n) (<= ,n reader$$char_code_limit)))

;;; ../match/ac.lisp
(defmacro dotimes-fixnum (&rest body)
  (let ((var (car (car body)))
        (lim (cadr (car body)))
        (res (cddr (car body)))
        (acts (cdr body))
        (limvar (gensym))
        (lab (gensym)))
    `(block ()
       (let* ((,limvar ,lim) (,var 0))
         (declare (fixnum ,var ,limvar))
         (tagbody
            ,lab
            (if (>= ,var ,limvar) (return (progn ,@res)))
            (tagbody ,@acts)
            (setq ,var (1+ ,var))
            (go ,lab))))))

(defmacro incfa (x) `(setf ,x (1+ ,x)))

(defmacro make-AC-state () 
  `(make-array 26))

(defmacro AC-state-operators (state)
  `(aref ,state 0))

(defmacro AC-state-lhs-f (state)
  `(aref ,state 1))

(defmacro AC-state-lhs-v (state)
  `(aref ,state 2))

(defmacro AC-state-rhs-c (state)
  `(aref ,state 3))

(defmacro AC-state-rhs-f (state)
  `(aref ,state 4))

(defmacro AC-state-lhs-f-r (state)
  `(aref ,state 5))

(defmacro AC-state-lhs-v-r (state)
  `(aref ,state 6))

(defmacro AC-state-rhs-c-r (state)
  `(aref ,state 7))

(defmacro AC-state-rhs-f-r (state)
  `(aref ,state 8))

(defmacro AC-state-lhs-mask (state)
  `(aref ,state 9))

(defmacro AC-state-lhs-f-mask (state)
  `(aref ,state 10))

(defmacro AC-state-lhs-r-mask (state)
  `(aref ,state 11))

(defmacro AC-state-rhs-c-sol (state)
  `(aref ,state 12 ))

(defmacro AC-state-rhs-c-max (state)
  `(aref ,state 13))

(defmacro AC-state-rhs-f-sol (state)
  `(aref ,state 14))

(defmacro AC-state-rhs-f-max (state)
  `(aref ,state 15))

(defmacro AC-state-rhs-full-bits (state)
  `(aref ,state 16))

(defmacro AC-state-rhs-c-compat (state)
  `(aref ,state 17))

(defmacro AC-state-rhs-f-compat (state)
  `(aref ,state 18))

(defmacro AC-state-lhs-c-count (state)
  `(aref ,state 19))

(defmacro AC-state-lhs-f-count (state)
  `(aref ,state 20))

(defmacro AC-state-lhs-v-count (state)
  `(aref ,state 21))

(defmacro AC-state-rhs-c-count (state)
  `(aref ,state 22))

(defmacro AC-state-rhs-f-count (state)
  `(aref ,state 23))

(defmacro AC-state-no-more (state)
  `(aref ,state 24))

(defmacro AC-state-ac-state-p (state)
  `(aref ,state 25))

;; small utility.  Side effect.
(defmacro AC$$Rotate-Left (array m)
"; shifts the element one bit to the left"
  `(setf (aref ,array ,m)
      (* 2 (aref ,array ,m))))

(defmacro AC$$note-repeats (mset array max gcd)
"; puts all repeated terms together in the list, and bashes the array
 ; (into numbers) in locations corresponding to the duplicate terms. 
 ; returns the newly grouped permutation of list.
 ; e.g. for input (a b c c c d d e f f) and #(0 0 0 0 0 0 0 0 0),
 ; this should make the array into #(0 0 3 2 1 2 1 0 2 1)."
  `(let* ((list2 nil)
  (counter (array-dimension ,array 0)) )
    (dolist (element ,mset)
          (let ((n (cdr element)))
          (declare (fixnum n))
          (when (> n ,max)
              (setq ,max n))
            (setq ,gcd (gcd ,gcd n))
        (if (> n 1) ; if it is repeated at all
              (dotimes-fixnum (x n)
                    (push (first element) list2)
                    (setq counter (1- counter))
                     (setf (aref ,array counter) (1+ x)))
           (progn (push (first element) list2)
                    (setq counter (1- counter))
                     (setf (aref ,array counter) 0))))) ; this line optional
    list2))                                     ; (if 0'd array is guaranteed)

(defmacro AC$$eq-member (term list)
; "predicate. true if term is term$equational_equal some element of list"
  `(dolist (term2 ,list)
      (when (term$equational_equal ,term (car ,list))
       (return t))))

;; don't even think of using this again
(defmacro AC$$collapse-one-array-internal (rhs-sol rhs-array)
  `(dotimes-fixnum (j (array-dimension ,rhs-sol 0))
           (when (> (logand (aref ,rhs-sol j) term-code) 0)
              (push  (car (aref ,rhs-array j)) rhs-subterms))))

;; don't even think of using this again
(defmacro AC$$collapse-arrays-internal (lhs skip) 
  `(dotimes-fixnum (i (array-dimension ,lhs 0))
      (if (< i ,skip)
          nil
          (progn 
  (setq rhs-subterms nil)
 (setq term-code (* 2 term-code))
        (AC$$collapse-one-array-internal rhs-c-sol rhs-c)
       (AC$$collapse-one-array-internal rhs-f-sol rhs-f)
;      (print$brief (car (aref ,lhs i)))
;       (map nil #'print$brief rhs-subterms)
   (system$add_eq 
  new-sys 
        (match_equation$create (car (aref ,lhs i))
           (if (cdr rhs-subterms) ; implies length is greater than 1
           (term$make_right_assoc_normal_form_with_sort_check
                (aref ops (cdr (aref ,lhs i))) rhs-subterms)
                  (first rhs-subterms))))))))


;;; ../rew/substitution.lisp

;; op substitution$new: -> Substitution.
(defmacro substitution$new ()
  '())

;; op substitution$create: Variable Term -> Substitution
; not used
(defmacro substitution$create (var term)
  `(list (cons ,var ,term)) ;18 Feb 88 added list
  )

;; op substitution$add: Substitution var term -> Substitution
(defmacro substitution$add (subst var term)
;  `(acons ,var ,term ,subst)
  `(cons (cons ,var ,term) ,subst))

;;; ../modexp/modexp.lisp

(defmacro defrepr (module_prefix name tag elts)
  `(defstruct (,tag
                (:conc-name ,module_prefix)
                (:constructor 
                 ,(intern (string-upcase 
                   (concatenate 'string (string module_prefix) (concatenate 'string "make_" (string name)))))
                 ,elts)
                (:type list)
                :named
                )
     ,@elts))

;;; ../tools/obj_trace.lisp
(defmacro obj_trace (&rest l)
  `(obj_trace_fn ',l))

(defmacro obj_untrace (&rest l)
  `(obj_untrace_fn ',l))

;;; quiet versions produce no output, but are recorded in the context
(defmacro obj_trace_quiet (&rest l)
  `(obj_trace_quiet_fn ',l))

;;; ../tools/tools.lisp
;;; easily look at function definition
(defmacro sf (x) `(symbol-function ',x))

;;; useful for finding location of error in a file
;;;   -- replace all defun's by %defun
;;;   -- reload to locate error
(defmacro %defun (&rest fm)
  (princ (car fm) *terminal-io*) (terpri *terminal-io*) (cons 'defun fm))

;;; evaluate with output to a file
(defmacro to-file (file &rest forms)
  `(with-open-file (*standard-output* ,file :direction :output)
      ,@forms))
