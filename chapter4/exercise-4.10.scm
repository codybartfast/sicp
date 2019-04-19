#lang sicp

(#%require "common.scm")

;   Exercise 4.10
;   =============
;   
;   By using data abstraction, we were able to write an eval procedure that
;   is independent of the particular syntax of the language to be evaluated.
;   To illustrate this, design and implement a new syntax for Scheme by
;   modifying the procedures in this section, without changing eval or
;   apply.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.10]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.10
;   4.1.2 Representing Expressions - p376
;   ------------------------------------------------------------------------

(-start- "4.10")

(println "
Below is an example of changing the name of a proc (let -> with) and it's
straight forward.  One could quite easily reorder paramaters e.g., change
named let from (let name pairs body) to (let pairs name body).  But apart
from renaming and reodering stuff, other changes my limited imagination can
summon just seem to make the language less usable and harder to implement.")

(#%require "ea-data-directed.scm")
(put-evaluators)

;; Rename let to 'with' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-call cons)
(define with-pairs cadr)
(define with-pair-id car)
(define with-pair-value cadr)

(define (with-params exp)
  (map with-pair-id (with-pairs exp)))
(define (with-values exp)
  (map with-pair-value (with-pairs exp)))
(define with-body cddr)

(define (with->combination exp)
  (make-call
   (make-lambda (with-params exp)
                (with-body exp))
   (with-values exp)))

(define (eval-with exp env)
  (eval (with->combination exp) env))

(put 'eval 'with eval-with)

;;  ;;;;;;;

(define (last list)
  (if (equal? '() (cdr list))
      (car list)
      (last (cdr list))))

;; Use it ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define with-expr '(with ((operand1 3)
                          (operator +)
                          (operand2 2))
                      'first-line
                      (+ 1 1)
                      (operator operand1 operand2)))

(println "
Evaluating expression:
    " with-expr "
Expect: 5
Got: "
      (eval
       with-expr
       the-global-environment))

(--end-- "4.10")

