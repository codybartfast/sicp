#lang sicp

(#%require "common.scm")

;   Exercise 4.22
;   =============
;   
;   Extend the evaluator in this section to support the special form let.
;   (See exercise [4.6].)
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.22]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.22
;   [Exercise 4.6]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.6
;   4.1.7 Separating Syntactic Analysis from Execution - p398
;   ------------------------------------------------------------------------

(-start- "4.22")

(#%require "ea-analyzing-22.scm")
(put-analyzers)
;; ea-analyzing-22 has updated versions of all the extensions from the
;; previous exercises.

(println "
This is cool.  The only thing that needs to change is the code that calls
let->combination.  Instead of evaling the result we analyze it:

(define (analyze-let exp)
  (analyze (let->combination exp)))
(put 'analyze 'let analyze-let)

Once this change is made we can run the original test: ")

(define expression '(let ((operand1 3)
                          (operator +)
                          (operand2 2))
                      'first-line
                      (+ 1 1)
                      (operator operand1 operand2)))

(println "
Evaluating expression:
    " expression "
Expect: 5
Got: "
      (eval
       expression
       the-global-environment))

(--end-- "4.22")

