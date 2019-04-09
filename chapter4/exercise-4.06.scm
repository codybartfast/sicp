#lang sicp

(#%require "common.scm")

;   Exercise 4.6
;   ============
;   
;   Let expressions are derived expressions, because
;   
;   (let ((<var₁> <exp₁>) ... (<var_(n)> <exp_(n)>))
;     <body>)
;   
;   is equivalent to
;   
;   ((lambda (<var₁> ... <var_(n)>)
;      <body>)
;    <exp₁>
;    ...
;    <exp_(n)>)
;   
;   Implement a syntactic transformation let->combination that reduces
;   evaluating let expressions to evaluating combinations of the type shown
;   above, and add the appropriate clause to eval to handle let expressions.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.6]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.6
;   4.1.2 Representing Expressions - p375
;   ------------------------------------------------------------------------

(-start- "4.6")

(#%require "ea-data-directed.scm")
(put-evaluators)

(define make-call cons)
(define let-pairs cadr)
(define let-pair-id car)
(define let-pair-value cadr)
(define (let-params exp)
  (map let-pair-id (let-pairs exp)))
(define (let-values exp)
  (map let-pair-value (let-pairs exp)))
(define let-body cddr)

(define (let->lambda exp)
  (make-call
   (make-lambda (let-params exp)
                (let-body exp))
   (let-values exp)))

(define (eval-let exp env)
  (eval (let->lambda exp) env))

(put 'eval 'let eval-let)

(define expression '(let ((operand1 3)
                          (operator +)
                          (operand2 2))
                      (operator operand1 operand2)))

(println "
Evaluating expression:
    " expression "
Expect: 5
Got: "
      (eval
       expression
       the-global-environment))

(--end-- "4.6")

