#lang sicp

(#%require "common.scm")

;   Exercise 4.37
;   =============
;   
;   Ben Bitdiddle claims that the following method for generating
;   Pythagorean triples is more efficient than the one in exercise [4.35]. 
;   Is he correct?  (Hint: Consider the number of possibilities that must be
;   explored.)
;   
;   (define (a-pythagorean-triple-between low high)
;     (let ((i (an-integer-between low high))
;           (hsq (* high high)))
;       (let ((j (an-integer-between i high)))
;         (let ((ksq (+ (* i i) (* j j))))
;           (require (>= hsq ksq))
;           (let ((k (sqrt ksq)))
;             (require (integer? k))
;             (list i j k))))))
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.37]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.37
;   [Exercise 4.35]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.35
;   4.3.1 Amb and Search - p418
;   ------------------------------------------------------------------------

(-start- "4.37")



(--end-- "4.37")

