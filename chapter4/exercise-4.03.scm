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

(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    (else
     (if (pair? exp)
         (let ((evaluator (get 'eval (expression-type exp))))
           (if evaluator
               (evaluator exp env)               
               (apply (eval (operator exp) env)
                      (list-of-values (operands exp) env))))
         ((error \"Unknown expression type -- EVAL\" exp))))))

This is verified below where a data-direct eval apply is used to evaluate a
test expression.

Comparing it to Ex 2.73 I'm primarily struck by the similarity. Simple cases
are handled specifically and then more complicated cases are handled in a
standard way which allows the data directed approach.  With 2.73 the data /
operands is a well defined list of values, here though the data / exp can
have any structure so long as it makes sense to the specific evaluator.
")


;; 'ea-data-directed' contains a full eval/apply implementaion with a
;; data-directed eval.  It's in a separated file so it can be used with
;; other exercises.

(#%require "ea-data-directed-03.scm")
(#%require "ea-pick-fruit-expression.scm")

(put-evaluators)

;; Try it ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(println "Checking with data-directed eval:")
(check-fruit
 (apply (eval
         pick-fruit
         the-global-environment)
        '()))
(println "")

(--end-- "4.3-extra")

