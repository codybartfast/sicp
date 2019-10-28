#lang sicp

(#%require "common.scm")

;   Exercise 4.70
;   =============
;   
;   What is the purpose of the let bindings in the procedures add-assertion!
;   and add-rule! ?  What would be wrong with the following implementation
;   of add-assertion! ? Hint: Recall the definition of the infinite stream
;   of ones in section [3.5.2]: (define ones (cons-stream 1 ones)).
;   
;   (define (add-assertion! assertion)
;     (store-assertion-in-index assertion)
;     (set! THE-ASSERTIONS
;           (cons-stream assertion THE-ASSERTIONS))
;     'ok)
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.70]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.70
;   [Section 3.5.2]: http://sicp-book.com/book-Z-H-24.html#%_sec_3.5.2
;   4.4.4 Implementing the Query System - p482
;   ------------------------------------------------------------------------

(-start- "4.70")



(--end-- "4.70")

