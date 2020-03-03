#lang sicp

(#%require "common.scm")

;   Exercise 5.46
;   =============
;   
;   Carry out an analysis like the one in exercise [5.45] to determine the
;   effectiveness of compiling the tree-recursive Fibonacci procedure
;   
;   (define (fib n)
;     (if (< n 2)
;         n
;         (+ (fib (- n 1)) (fib (- n 2)))))
;   
;   compared to the effectiveness of using the special-purpose Fibonacci
;   machine of figure [5.12].  (For measurement of the interpreted
;   performance, see exercise [5.29].) For Fibonacci, the time resource used
;   is not linear in n; hence the ratios of stack operations will not
;   approach a limiting value that is independent of n.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.46]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.46
;   [Exercise 5.45]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.45
;   [Exercise 5.29]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.29
;   [Figure 5.12]:   http://sicp-book.com/book-Z-H-31.html#%_fig_5.12
;   5.5.7 Interfacing Compiled Code to the Evaluator - p609
;   ------------------------------------------------------------------------

(-start- "5.46")



(--end-- "5.46")

