#lang sicp

(#%require "common.scm")

;   Exercise 4.25
;   =============
;   
;   Suppose that (in ordinary applicative-order Scheme) we define unless as
;   shown above and then define factorial in terms of unless as
;   
;   (define (factorial n)
;     (unless (= n 1)
;             (* n (factorial (- n 1)))
;             1))
;   
;   What happens if we attempt to evaluate (factorial 5)?  Will our
;   definitions work in a normal-order language?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.25]: http://sicp-book.com/book-Z-H-27.html#%_thm_4.25
;   4.2.1 Normal Order and Applicative Order - p400
;   ------------------------------------------------------------------------

(-start- "4.25")



(--end-- "4.25")

