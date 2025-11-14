kQuickstart 

add to .sbclrc:

(asdf:initialize-source-registry
 `(:source-registry (:tree (:home "code/lisp/obj3/"))
                    #+nil(:directory ,(home "/code/lisp2/sicl/SICL/Grammar"))
                    :inherit-configuration))
then launch sbcl and execute
(asdf:load-system :obj3)
(progn (in-package :obj3) (user::obj3-top-level))

To create an executable:
sbcl --load obj3/deploy.lisp

To install the emacs editor plugin add "obj3/emacs plugin/.emacs" contents to your ~/.emacs file.
M-x (alt-x) run-obj3 
then C-x C-f (control x then  control f) and open a file with the extension .obj

Use F12 to evaluate a code paragraph and F5 to evaluate a whole file.

When coding it is useful to use "eof" to tell it that you don't want anything afterwards to be loaded. 

For example:

obj LIST[X :: TRIV] is
  sorts List NeList .
  op nil : -> List .
  subsorts Elt < NeList < List .
  op __ : List List -> List [assoc id: nil] .
  op __ : NeList List -> NeList .
  op __ : NeList NeList -> NeList .
  protecting NAT .
  op |_| : List -> Nat .
  eq | nil | = 0 .
  var E : Elt .   var L : List .
  eq | E L | = 1 + | L | .
  op tail_ : NeList -> List [prec 120] .
  var E : Elt .   var L : List .
  eq tail E L = L .
endo

eof *** F5 then delete or comment this line then again F5 to evaluate the whole file
    
obj NatList is
pr LIST[NAT] .
endo

red 1 2 .

Other useful emacs shortcuts:
C-x o     go to the other split
You can bind this to alt-left arrow and alt-right arrow using this code (add it to ~/.emacs):
(global-set-key (kbd "<M-left>") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "<M-right>") (lambda () (interactive) (other-window 1)))

C-x 2 split horizontally
C-x 3 split vertically
C-0 0 delete split 

(global-set-key (kbd "H-<left>") 'previous-buffer)
(global-set-key (kbd "H-<right>") 'next-buffer)
H is the context-menu key (which can for convenience be remapped to caps-lock if one wishes)

OBJ3F1 2.11
F1 means "fork 1" (this fork) which does not intend to remain compatible with the old Common Lisp implementations.
OBJ3 is the ancestor of CafeOBJ and Maude. It is coded in Common Lisp and has a couple of features (eg. Default Views) which make it very attractive for exploratory programming.

Changes

OBJ3F1 2.11  reorganized the code, now it uses ASDF and runs on SBCL (tested with SBCL 2.5.10 on Windows)

OBJ3
2.10 - OBJ3 and TRIM compiler update to compile under GCL 2.6.12

The 2.09 release updates OBJ3 to recent versions of gcl (2.5.3), CMU Common Lisp (18e), glibc (2.3), gcc (3.2), and provides a pre-built executable for a mainstream modern Linux distribution (RedHat 8.0).
Changes made to OBJ3 2.08

The 2.08 release is different than the 2.06 release in two ways:
-Lutz Hamel's TRIM compiler has been integrated with the release.
-The release has been tested with GCL 2.3.6 and GCL 2.4.0.

Changes made to OBJ3 2.06

The 2.06 release is different from the 2.05 release in two ways:
-Several small changes have been made to the documentation to increase clarity and accuracy.
-The release now does not include any binary files, it is only a source release. A separate download is now available to obtain pre-built executable(s).

Changes made to OBJ3 2.05

The 2.05 release of OBJ3 only has three major changes:
-OBJ3 has been ported to GCL and currently compiles only under GCL 2.2.2 due to bugs in GCL 2.2.3.
-The entire OBJ3 distribution and been rebuilt and reorganized. Documentation is now much more complete, the distribution uses modern standard makefiles and web-based documentation.
-OBJ3 now has a BSD-based license.

------
OBJ3 (version 2) is our latest version of OBJ, a wide spectrum functional programming language that is rigorously based upon order sorted equational logic.  This logic provides a notion of subsort that rigorously supports multiple inheritance, exception handling and overloading.  This rigorous semantic basis allows a declarative, specificational style of programming, facilitates program verification, and even allows OBJ to be used as a theorem prover. Although OBJ3 executable code normally consists of equations that are
interpreted as rewrite rules, it can also encapsulate Lisp code, e.g., to provide efficient built-in data types.  OBJ provides rewriting modulo associative, commutative and/or identity equations, as well as user-definable evaluation strategies that allow lazy, eager, and mixed evaluation strategies on an operation-by-operation basis; memoization
is also available on an operation-by-operation basis.

OBJ3 supports parameterized programming, a powerful technique for software design, production, reuse and maintenance.  This approach involves abstraction through two kinds of module: objects to encapsulate executable code, and in particular to define abstract data types; and theories to specify both syntactic structure and semantic properties of modules and module interfaces.  Each kind of module can be parameterized, where actual parameters are modules. Modules can also import other modules, thus supporting multiple
inheritance at the module level.  For parameter instantiation, a view binds the formal entities in an interface theory to actual entities in a module, and also asserts that the module satisfies the semantic requirements of the theory.  This integration of objects, theories and views provides a powerful wide spectrum capability.
Module expressions allow complex combinations of already defined modules, including sums, instantiations and transformations; moreover, evaluating a module expression actually constructs a software (sub)system from the given components.  Default views can greatly reduce the effort of instantiating modules.

OBJ3 has a facility for controlled rewriting which can be used for equational theorem proving.

TODO list for OBJ3
    Clean up source code (remove unused/redundant code, add more documentation).
    Re-investigate old bug reports for the 2.04 release.
    Integrate FOOPS into the release.
    Integrate TOOR into the release.
    Integrate EqLog into the release.
    Integrate 2OBJ into the release.
