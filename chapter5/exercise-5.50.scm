#lang sicp

(#%require "common.scm")

;   Exercise 5.50
;   =============
;   
;   Use the compiler to compile the metacircular evaluator of section [4.1]
;   and run this program using the register-machine simulator.  (To compile
;   more than one definition at a time, you can package the definitions in a
;   begin.) The resulting interpreter will run very slowly because of the
;   multiple levels of interpretation, but getting all the details to work
;   is an instructive exercise.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.50]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.50
;   [Section 4.1]:   http://sicp-book.com/book-Z-H-26.html#%_sec_4.1
;   5.5.7 Interfacing Compiled Code to the Evaluator - p610
;   ------------------------------------------------------------------------

(-start- "5.50")

(#%require "machine-50.scm")
(#%require "compiler-50.scm")
(#%require "ec-evaluator-50.scm")
(#%require "mc-evaluator-50-exp.scm")

(println
 "
Metacircular Evaluator Expression
=================================

In an attempt to keep things simple I used the most basic implementation of
the metacircular evaluator from Section 4.1.  As the question says this
needs to be put in one big begin statement, which is then quoted to provide
an expression that can be compiled.

A few changes were needed to the content of the expression:

  1. add a map procedure so that when map is called by the metacircular
     evaluator its procedure calls are evaluated at the right level (i.e.,
     within the metacircular evaluator and not by a primitive map in the
     explicit-control procedures).  See Exercise 4.14.

  2. add any primitive-procedures that will be needed by the programs
     evaluated by the metacircular evaluator.

  3. add (driver-loop) to the end of the metaciruclar evaluator so the REPL
     starts automatically.


Metaciruclar Evaluator Code:
----------------------------

  (define mc-evaluator-exp
    '(begin

       (define (eval exp env)
         (cond ((self-evaluating? exp) exp)
               ((variable? exp) (lookup-variable-value exp env))
               ((quoted? exp) (text-of-quotation exp))
               ...

       ... LOADS MORE ...  

       (map proc items)
         (if (null? items)
             '()
             (cons (proc (car items))
                   (map proc (cdr items)))))

       (define primitive-procedures
         (list (list '= =)
               (list '* *)
               (list '- -)
               (list 'cons cons)
               (list 'list list)
               (list 'equal? equal?)
               ))

       ... BIT MORE ...  

       (driver-loop)
       
       ))


\"Explicit-Control\" Procedures
=============================

We don't need the Explicit Control Evaluator itself, but we do need its 
primitive operations and primitive procedures.

I started with the most recent ec-evaluator (ex 5.48) and made the following
changes:

  1. Add primitive procedures.  The explict control's table of primitive
     procedures needs to include:

       a. all primitive procedures used by the metacircular evaluator's
          implementation, e.g., cadddr, cddr, set-car!, etc ...

       b. any primitive procedures in the metacircular evalutors table of
          primitive procedures, e.g., =, *, -, etc ...

  2. Add apply-in-underlying-scheme to the explicit-control procedures and
     include it in the explicit-control's table of primitive-procedures.

     Primitive procedures in the metacircular are double tagged. E.g., the
     cons procedures will be:

         (primitive (primitive <underlying-cons>))

     The outer tag is added by the metacircular evaluator, the inner tag is
     added by the ec-evaluator's primitive-procedure-objects procedure.

     So the apply-in-underlying-scheme provided to the metacircular
     evaluator needs to remove this inner tag before passing it to
     the scheme that the explicit-control procedures are implemented on.

  3. Removed checking around primitive procedures (Exercise 5.30).  This is
     not functionally necessary, but it helped debugging while getting the
     metacircular evaluator working.  Without checking the machine crashes
     as soon a primitive-procedure encounters a problem.  With checking the
     machine doesn't crash until the result of the bad primitive procedure
     call is used.  (If we want primitive checking of the interpreted code
     then we need to implement that in the metacircular evaluator, not in
     the compiled code).


\"Explicit Control\" Procedure Code:
----------------------------------

  (define (primitive-implementation proc) (cadr proc))

  (define (apply-in-underlying-scheme proc args)
    (apply (primitive-implementation proc) args))

  (define primitive-procedures
    ;; Primitives used by the metacirculator evaluator's implementation
    (list (list 'car car)
          (list 'cdr cdr)
          (list 'cons cons)
          ...
          (list 'set-car! set-car!)
          (list 'set-cdr! set-cdr!)
          (list 'cadddr cadddr)
          (list 'cdddr cdddr)
          (list 'apply-in-underlying-scheme apply-in-underlying-scheme)
          ;; Additional primitive procedures installed in the MC-evaluator
          (list '- -)
          (list '* *)
          (list 'equal? equal?)
          ))


Compiler
========

Started with the most recent compiler (Ex 5.48).  We do need internal
definitions to work, and I think that requires having scan-out-defines
installed in the compiler (Exercise 5.43).  Additional changes:

  1. The metaciruclar evaluator's implementation uses let.  Therefore we
     need to add support for let to the compiler (or rewrite the
     metacircular evaluator replacing let statements with lambda calls).
     Support for let can be added to the compiler the same way it was added
     to the metacircular evaluator (Exercise 4.6) as a derived expression.

  2. Added a convenience procedure, statements-with-next, that compiles an
     expression, (in this case mc-evaluator-exp), with the 'next linkage and
     extracts the statements.


Compiler Code:
--------------

  ;; install let->combination

  (define (compile exp ctenv target linkage)
    (cond ((self-evaluating? exp)
           (compile-self-evaluating exp ctenv target linkage))
          ((quoted? exp) (compile-quoted exp ctenv target linkage))
          ...
          ((cond? exp) (compile (cond->if exp) ctenv target linkage))
          ((let? exp) (compile (let->combination exp) ctenv target linkage))
          ...
          ((application? exp)
           (compile-application exp ctenv target linkage))
          (else
           (error \"Unknown expression type -- COMPILE\" exp))))


  ;; let->combination

  (define (let-body exp)
    (cddr exp))

  (define (let-pairs exp)
    (cadr exp))

  (define let-pair-id car)

  (define let-pair-value cadr)

  (define (let-params exp)
    (map let-pair-id
         (let-pairs exp)))

  (define (let-values exp)
    (map let-pair-value
         (let-pairs exp)))

  (define (let? exp) (tagged-list? exp 'let))

  (define (let->combination exp)
    (make-call
     (make-lambda (let-params exp)
                  (let-body exp))
     (let-values exp)))


  ;; convenience procedure

  (define (statements-with-next exp)
    (statements-with exp 'next))

  (define (statements-with exp linkage)
    (statements
     (compile exp empty-ctenv 'val linkage)))


Running the Metacircular Evaluator
==================================

To run the evaluator we just need to prepend an instruction to load the
global environment.  Because there is a call to (driver-loop) at the end of
the evaluator the REPL starts automatically:

  (define mc-eval-code
    (statements-with-next mc-evaluator-exp))

  (define program
    (cons '(assign env (op get-global-environment))
          mc-eval-code))

  (define machine (make-machine eceval-operations program))

  (start machine)


Sample Output:
--------------

  ;;; M-Eval input:
    (define (factorial n)
      (if (= n 1)
          1
          (* n (factorial (- n 1)))))

  ;;; M-Eval value:
  ok

  ;;; M-Eval input:
    (factorial 5)

  ;;; M-Eval value:
  120

  ;;; M-Eval input:


Demo:
=====
Compiling metacircular-evaluator source ...")

(define mc-eval-code
  (statements-with-next mc-evaluator-exp))

(define program
  (cons '(assign env (op get-global-environment))
        mc-eval-code))

(println "Making machine ...")
(define machine (make-machine eceval-operations program))

(println "Starting matacirculator evaluator REPL ...")

(start machine)

#|
============================================================================
Snippets To Use In The REPL
----------------------------------------------------------------------------


(define (pick-fruit)
  (define trace '())
  (define (get-apple)
    (set! trace (cons "'getting apple'" trace))
    "apple")
  (define (get-cherry)
    (set! trace (cons "'getting cherry'" trace))
    "cherry")
  (define (first-or-second first second which)
    (cond ((equal? which 'first) (first))
          (else (second))))
  (list
   (list
    (first-or-second get-apple get-cherry 'first)
    (first-or-second get-apple get-cherry 'not-first))
   trace))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))


(pick-fruit)

(factorial 5)


============================================================================
|#

(--end-- "5.50")