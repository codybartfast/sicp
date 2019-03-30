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

(println
 "Checking with orignal eval:")
(check-fruit
 (apply (ea-eval
         pick-fruit
         the-global-environment)
        '()))
(println "")

(define (expression-type exp)
  (error "Unable to determine expression type: " exp))

(define (expression-body exp)
  (error "not implemented"))

(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((get 'eval (expression-type exp)) (expression-body exp)
                                       env)))

(println
 "Checking with local eval:")
(check-fruit
 (apply (eval
         pick-fruit
         the-global-environment)
        '()))
(println "")

(--end-- "4.3-extra")

