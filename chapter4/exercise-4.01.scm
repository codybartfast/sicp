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



(--end-- "4.1")

