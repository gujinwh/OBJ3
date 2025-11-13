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

    Lutz Hamel's TRIM compiler has been integrated with the release.
    The release has been tested with GCL 2.3.6 and GCL 2.4.0.

Changes made to OBJ3 2.06

The 2.06 release is different from the 2.05 release in two ways:

    Several small changes have been made to the documentation to increase clarity and accuracy.
    The release now does not include any binary files, it is only a source release. A separate download is now available to obtain pre-built executable(s).

Changes made to OBJ3 2.05

The 2.05 release of OBJ3 only has three major changes:

    OBJ3 has been ported to GCL and currently compiles only under GCL 2.2.2 due to bugs in GCL 2.2.3.
    The entire OBJ3 distribution and been rebuilt and reorganized. Documentation is now much more complete, the distribution uses modern standard makefiles and web-based documentation.
    OBJ3 now has a BSD-based license.

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
