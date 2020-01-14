#lang sicp

(#%require "common.scm")

;   Exercise 5.3
;   ============
;   
;   Design a machine to compute square roots using Newton's method, as
;   described in section [1.1.7]:
;   
;   (define (sqrt x)
;     (define (good-enough? guess)
;       (< (abs (- (square guess) x)) 0.001))
;     (define (improve guess)
;       (average guess (/ x guess)))
;     (define (sqrt-iter guess)
;       (if (good-enough? guess)
;           guess
;           (sqrt-iter (improve guess))))
;     (sqrt-iter 1.0))
;   
;   Begin by assuming that good-enough? and improve operations are available
;   as primitives.  Then show how to expand these in terms of arithmetic
;   operations.  Describe each version of the sqrt machine design by drawing
;   a data-path diagram and writing a controller definition in the
;   register-machine language.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.3]:  http://sicp-book.com/book-Z-H-31.html#%_thm_5.3
;   [Section 1.1.7]: http://sicp-book.com/book-Z-H-10.html#%_sec_1.1.7
;   5.1.2 Abstraction in Machine Design - p502
;   ------------------------------------------------------------------------

(-start- "5.3")



(--end-- "5.3")

