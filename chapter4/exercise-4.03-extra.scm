#lang sicp

(#%require "common.scm")

;   Exercise 4.3
;   ============
;   
;   Rewrite eval so that the dispatch is done in data-directed style. 
;   Compare this with the data-directed differentiation procedure of
;   exercise [2.73]. (You may use the car of a compound expression as the
;   type of the expression, as is appropriate for the syntax implemented in
;   this section.) .
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.3]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.3
;   [Exercise 2.73]: http://sicp-book.com/book-Z-H-26.html#%_thm_2.73
;   4.1.2 Representing Expressions - p374
;   ------------------------------------------------------------------------

(-start- "4.3-extra")

(#%require "ea-eval-apply.scm")
(#%require "ea-pick-fruit-expression.scm")

;(define (eval exp env)
;  (cond
;    ((self-evaluating? exp) exp)
;    ((variable? exp) (lookup-variable-value exp env))
;    ((quoted? exp) (text-of-quotation exp))
;    ((get 'eval (expression-type exp)) (expression-body exp)
;                                       env)))

;(define (eval exp env)
;  ;(display "Evaling: ")(display exp)(newline)
;  (cond ((self-evaluating? exp) exp)
;        ((variable? exp) (lookup-variable-value exp env))
;        ((quoted? exp) (text-of-quotation exp))
;        ((assignment? exp) (eval-assignment exp env))
;        ((definition? exp) (eval-definition exp env))
;        ((if? exp) (eval-if exp env))
;        ((lambda? exp)
;         (make-procedure (lambda-parameters exp)
;                         (lambda-body exp)
;                         env))
;        ((begin? exp)
;         (eval-sequence (begin-actions exp) env))
;        ((cond? exp) (eval (cond->if exp) env))
;        ((application? exp)
;         (apply (eval (operator exp) env)
;                (list-of-values (operands exp) env)))
;        (else
;         (error "Unknown expression type -- EVAL" exp))))

;(check-fruit
; (eval
;  pick-fruit
;  the-global-environment))

(display (apply (eval pick-fruit the-global-environment) '()))
(newline)
(--end-- "4.3-extra")

