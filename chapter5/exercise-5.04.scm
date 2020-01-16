#lang sicp

(#%require "common.scm")

;   Exercise 5.4
;   ============
;   
;   Specify register machines that implement each of the following
;   procedures.  For each machine, write a controller instruction sequence
;   and draw a diagram showing the data paths.
;   
;   a. Recursive exponentiation:
;   
;   (define (expt b n)
;     (if (= n 0)
;         1
;         (* b (expt b (- n 1)))))
;   
;   b. Iterative exponentiation:
;   
;   (define (expt b n)
;     (define (expt-iter counter product)
;       (if (= counter 0)
;           product
;           (expt-iter (- counter 1) (* b product))))
;     (expt-iter n 1))
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.4]:  http://sicp-book.com/book-Z-H-31.html#%_thm_5.4
;   5.1.4 Using a Stack to Implement Recursion - p510
;   ------------------------------------------------------------------------

(-start- "5.4")



(--end-- "5.4")

