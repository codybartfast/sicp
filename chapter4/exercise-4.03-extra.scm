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
(#%require "ea-evaluators.scm")
(#%require "ea-pick-fruit-expression.scm")

;(println
; "Checking with orignal eval:")
;(check-fruit
; (apply (ea-eval
;         pick-fruit
;         the-global-environment)
;        '()))
;(println "")

(define (eval-lambda exp env)
  (display "evaling lambda")(newline)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))

(define (populate-evaluators)
  (define (eval-begin exp env)
    (eval-sequence (begin-actions exp) env))
  (define (eval-cond exp env)
    (eval (cond->if exp) env))
  (define (eval-application exp env)
    (apply (eval (operator exp) env)
           (list-of-values (operands exp) env)))    
  (put 'eval 'assignment eval-assignment)
  (put 'eval 'definition eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda eval-lambda)
  (put 'eval 'begin eval-begin)
  (put 'eval 'cond eval-cond)
  (put 'eval 'call eval-application))
(populate-evaluators)

(define (expression-type exp)
  (let ((type
        (cond
          ((assignment? exp) 'assignment)
          ((definition? exp) 'definition)
          ((if? exp) 'if)
          ((lambda? exp) 'lambda)
          ((begin? exp) 'begin)
          ((cond? exp) 'cond)
          ((pair? exp) 'call))))
    (display "got type: ")(display type)(newline)
    type))

(define (eval exp env)
  (display "evaluating:") (display exp)(newline)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((get 'eval (expression-type exp))
     ((get 'eval (expression-type exp)) exp env))
    (else (error "Unknown expression type -- EVAL" exp))))

(println
 "Checking with local eval:")
(check-fruit
 (apply (eval
         pick-fruit
         the-global-environment)
        '()))
(println "")

;(apply (eval pick-fruit the-global-environment) '())

(--end-- "4.3-extra")

