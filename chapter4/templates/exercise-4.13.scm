#lang sicp

(#%require "common.scm")

;   Exercise 4.13
;   =============
;   
;   Scheme allows us to create new bindings for variables by means of
;   define, but provides no way to get rid of bindings.  Implement for the
;   evaluator a special form make-unbound! that removes the binding of a
;   given symbol from the environment in which the make-unbound! expression
;   is evaluated. This problem is not completely specified.  For example,
;   should we remove only the binding in the first frame of the environment?
;   Complete the specification and justify any choices you make.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.13]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.13
;   4.1.3 Evaluator Data Structures - p380
;   ------------------------------------------------------------------------

(-start- "4.13")



(--end-- "4.13")

