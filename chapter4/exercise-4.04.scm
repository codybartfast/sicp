#lang sicp

(#%require "common.scm")

;   Exercise 4.4
;   ============
;   
;   Recall the definitions of the special forms and and or from chapter 1:
;   
;   * and: The expressions are evaluated from left to right.  If any
;   expression evaluates to false, false is returned; any remaining
;   expressions are not evaluated.  If all the expressions evaluate to true
;   values, the value of the last expression is returned.  If there are no
;   expressions then true is returned.
;   
;   * or: The expressions are evaluated from left to right.  If any
;   expression evaluates to a true value, that value is returned; any
;   remaining expressions are not evaluated.  If all expressions evaluate to
;   false, or if there are no expressions, then false is returned.
;   
;   Install and and or as new special forms for the evaluator by defining
;   appropriate syntax procedures and evaluation procedures eval-and and
;   eval-or.  Alternatively, show how to implement and and or as derived
;   expressions.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.4]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.4
;   4.1.2 Representing Expressions - p374
;   ------------------------------------------------------------------------

(-start- "4.4")

(#%require "ea-data-directed.scm")
;(#%require "ea-pick-fruit-expression.scm")

(populate-evaluators)

;; Eval twice because product of 'derived expression' is an expression.
(define (evl exp)
  (eval
   (eval exp the-global-environment)
   the-global-environment))

(define (display-and-or)
  (println "  AND:")
  (println "    false AND false: " (evl '(and false false)))
  (println "    false AND true: " (evl '(and false true)))
  (println "    true AND false: " (evl '(and true false)))
  (println "    true AND true: " (evl '(and true true)))
  (println "  OR:")
  (println "    false OR false: " (evl '(or false false)))
  (println "    false OR true: " (evl '(or false true)))
  (println "    true OR false: " (evl '(or true false)))
  (println "    true OR true: " (evl '(or true true))))

(define first-predicate cadr)
(define second-predicate caddr)

(put 'eval 'and
     (lambda (exp env)
       (if (true? (eval (first-predicate exp) env))
           (true? (eval (second-predicate exp) env))
           false)))

(put 'eval 'or
     (lambda (exp env)
       (if (true? (eval (first-predicate exp) env))
           true
           (true? (eval (second-predicate exp) env)))))

(println "Using EVALUATOR:")
(display-and-or)

(put 'eval 'and
     (lambda (exp env)
       (make-if (first-predicate exp)
                (make-if (second-predicate exp)
                         true
                         false)
                false)))

(put 'eval 'or
     (lambda (exp env)
       (make-if (first-predicate exp)
                true
                (make-if (second-predicate exp)
                         true
                         false))))


(println "
Using DERIVED expressions:")
(display-and-or)

(--end-- "4.4")

