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

(println "Compiling metacircular-evaluator source ...")
(define mc-eval-code
  (statements-with-next mc-evaluator-exp))

(define program
  (append '((assign env (op get-global-environment)))
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