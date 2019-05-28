#lang sicp

(#%require "common.scm")

;   Exercise 4.29
;   =============
;   
;   Exhibit a program that you would expect to run much more slowly without
;   memoization than with memoization.  Also, consider the following
;   interaction, where the id procedure is defined as in exercise [4.27] and
;   count starts at 0:
;   
;   (define (square x)
;     (* x x))
;   ;;; L-Eval input:
;   (square (id 10))
;   ;;; L-Eval value:
;   <response>
;   ;;; L-Eval input:
;   count
;   ;;; L-Eval value:
;   <response>
;   
;   Give the responses both when the evaluator memoizes and when it does
;   not.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.29]: http://sicp-book.com/book-Z-H-27.html#%_thm_4.29
;   [Exercise 4.27]: http://sicp-book.com/book-Z-H-27.html#%_thm_4.27
;   4.2.2 An Interpreter with Lazy Evaluation - p407
;   ------------------------------------------------------------------------

(-start- "4.29")



(--end-- "4.29")

