#lang sicp

(#%require "common.scm")

;   Exercise 4.53
;   =============
;   
;   With permanent-set! as described in exercise [4.51] and if-fail as in
;   exercise [4.52], what will be the result of evaluating
;   
;   (let ((pairs '()))
;     (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
;                (permanent-set! pairs (cons p pairs))
;                (amb))
;              pairs))
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.53]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.53
;   [Exercise 4.51]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.51
;   [Exercise 4.52]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.52
;   4.3.3 Implementing the <tt>Amb</tt> Evaluator - p437
;   ------------------------------------------------------------------------

(-start- "4.53")



(--end-- "4.53")

