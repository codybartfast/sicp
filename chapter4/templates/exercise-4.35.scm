#lang sicp

(#%require "common.scm")

;   Exercise 4.35
;   =============
;   
;   Write a procedure an-integer-between that returns an integer between two
;   given bounds.  This can be used to implement a procedure that finds
;   Pythagorean triples, i.e., triples of integers (i,j,k) between the given
;   bounds such that i ≤ j and i² + j² = k², as follows:
;   
;   (define (a-pythagorean-triple-between low high)
;     (let ((i (an-integer-between low high)))
;       (let ((j (an-integer-between i high)))
;         (let ((k (an-integer-between j high)))
;           (require (= (+ (* i i) (* j j)) (* k k)))
;           (list i j k)))))
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.35]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.35
;   4.3.1 Amb and Search - p417
;   ------------------------------------------------------------------------

(-start- "4.35")



(--end-- "4.35")

