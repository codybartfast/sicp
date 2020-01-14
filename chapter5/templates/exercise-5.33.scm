#lang sicp

(#%require "common.scm")

;   Exercise 5.33
;   =============
;   
;   Consider the following definition of a factorial procedure, which is
;   slightly different from the one given above:
;   
;   (define (factorial-alt n)
;     (if (= n 1)
;         1
;         (* n (factorial-alt (- n 1)))))
;   
;   Compile this procedure and compare the resulting code with that produced
;   for factorial.  Explain any differences you find.  Does either program
;   execute more efficiently than the other?
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.33]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.33
;   5.5.5 An Example of Compiled Code - p594
;   ------------------------------------------------------------------------

(-start- "5.33")



(--end-- "5.33")

