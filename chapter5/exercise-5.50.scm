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
#|
  1 test, mc   code->exp->scan-out-let->code->test->exp
  2 statements
  3 compile


  * Add Text, get it working (e.g. move prim-procs above prim-names and
    prim-objts)
      - set-underlying-apply!
      - Actually, this can just be provided as prim-proc

  * Add some helpers:
      - try-eval test eval without having to pass any args
      - mc-eval eval w/o getting and supplying the global env

  * Test test-exp, make sure test expression (e.g. factorial) works in
    regular mc-evaluator bofore trying to get it working in the compiled
    expression.  Will need to add extra primitive (= * - for factorial)

  * Put Mc-evaluator into a big begin expression

  * Add Ec-evaluator support.  Don't need the ec-evaluator, but do need its:
      - primitive-operations, these are need by compiled statments.
      - primitive-procedures.  Any procedure used by the Mc-evaluator goes
        here.

  * Remove checking from primitive-procedures (if present), so we fail in
    the right place (rather than failing when the error-object is used).
    The compiler could add checking code for objects, but I don't think that
    is useful unless and until we use the REPL.
    ACTUALLY - only going to pick up error for compiling mc-ev

  * Fix let

  * Fix map (apply)

  * Add prims used by mc-eval and prims implemented by mc-eval

  * Rremove lexical lookup

  * statements-with-next

  * remove-lexical-lookup

  * scan-out-defs to support recursivce calls

|#

(#%require "compiler-50.scm")
(#%require "ec-evaluator-50.scm")
(#%require "machine-50.scm")
(#%require "mc-evaluator-50.scm")
(#%require "mc-evaluator-50-exp.scm")

(println
 "
Using Orignal EC-Evaluator:
===========================")
(set-underlying-apply! apply)
(try-eval)

(mc-eval
 '(begin
    (define (factorial n)
      (if (= n 1)
          1
          (* n (factorial (- n 1)))))
    (factorial 5)
    ))

(println
 "
And now...
")

(define mc-statements (statements-with-next mc-evaluator-exp))

(define head
  '(
    (assign env (op get-global-environment))
    ))

(define statements
  (append head mc-statements))

(define operations
  eceval-operations)

(define machine (make-machine operations statements))

;(trace-on! machine println)
;(set-breakpoint machine 'primitive-branch1171 1)

(start machine)

#|
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

|#

(--end-- "5.50")

