#lang sicp

(#%require "common.scm")

;   Exercise 4.15
;   =============
;   
;   Given a one-argument procedure p and an object a, p is said to "halt" on
;   a if evaluating the expression (p a) returns a value (as opposed to
;   terminating with an error message or running forever).  Show that it is
;   impossible to write a procedure halts? that correctly determines whether
;   p halts on a for any procedure p and object a.  Use the following
;   reasoning: If you had such a procedure halts?, you could implement the
;   following program:
;   
;   (define (run-forever) (run-forever))
;   
;   (define (try p)
;     (if (halts? p p)
;         (run-forever)
;         'halted))
;   
;   Now consider evaluating the expression (try try) and show that any
;   possible outcome (either halting or running forever) violates the
;   intended behavior of halts?.⁽²³⁾
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.15]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.15
;   [Footnote 23]:   http://sicp-book.com/book-Z-H-26.html#footnote_Temp_558
;   4.1.5 Data as Programs - p387
;   ------------------------------------------------------------------------

(-start- "4.15")



(--end-- "4.15")

