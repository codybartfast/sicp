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
  * Add Text, get it working (e.g. move prim-procs above prim-names and
    prim-objts)
      - set-underlying-apply!

  * Add some helpers:
      - try-eval test eval without having to pass any args
      - mc-eval eval w/o getting and supplying the global env

  * Test test-exp, make sure test expression (e.g. factorial) works in
    regular mc-evaluator bofore trying to get it working in the compiled
    expression.  Will need to add extra primitive (= * - for factorial)
  

|#

(#%require "mc-evaluator-50.scm")

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

(--end-- "5.50")

