#lang sicp

(#%require "common.scm")

;   Exercise 4.1
;   ============
;   
;   Notice that we cannot tell whether the metacircular evaluator evaluates
;   operands from left to right or from right to left.  Its evaluation order
;   is inherited from the underlying Lisp: If the arguments to cons in
;   list-of-values are evaluated from left to right, then list-of-values
;   will evaluate operands from left to right; and if the arguments to cons
;   are evaluated from right to left, then list-of-values will evaluate
;   operands from right to left.
;   
;   Write a version of list-of-values that evaluates operands from left to
;   right regardless of the order of evaluation in the underlying Lisp. 
;   Also write a version of list-of-values that evaluates operands from
;   right to left.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.1]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.1
;   4.1.1 The Core of the Evaluator - p368
;   ------------------------------------------------------------------------

(-start- "4.1")

;(define (list-of-values exps env)
;  (if (no-operands? exps)
;      '()
;      (cons (eval (first-operand exps) env)
;            (list-of-values (rest-operands exps) env))))

(println "
To evaluate from Left to Right:

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      ((lambda (operand)
         (cons operand
               (list-of-values (rest-operands exps) env)))
       (eval (first-operand exps) env))))


To evaluate from Right to Left:

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      ((lambda (rest)
         (cons (eval (first-operand exps) env)
               rest))
       (list-of-values (rest-operands exps) env))))
")

(--end-- "4.1")

