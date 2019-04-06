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

(println "
This a full implementation of eval/apply based on the books text but with a
data-directed eval required by the question.  

(It was necessary to copy the content from ea-eval-apply instead of just
'requiring' it to ensure the eval defined here is the same eval used by all
the supporting functions.)

")

(#%require "ea-data-directed.scm")
(#%require "ea-pick-fruit-expression.scm")


(define (populate-evaluators)
  (define (eval-lambda exp env)
  ;(display "evaling lambda")(newline)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))
  (define (eval-begin exp env)
    (eval-sequence (begin-actions exp) env))
  (define (eval-cond exp env)
    (eval (cond->if exp) env))
  (define (eval-application exp env)
    (apply (eval (operator exp) env)
           (list-of-values (operands exp) env)))    
  (put 'eval 'call eval-application)
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda eval-lambda)
  (put 'eval 'begin eval-begin)
  (put 'eval 'cond eval-cond))
(populate-evaluators)

;; Try it ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(println
 "Checking expression with data-directed eval:")
(check-fruit
 (apply (eval
         pick-fruit
         the-global-environment)
        '()))
(println "")

(--end-- "4.3-extra")

