#lang sicp

(#%require "common.scm")

;   Exercise 4.16
;   =============
;   
;   In this exercise we implement the method just described for interpreting
;   internal definitions. We assume that the evaluator supports let (see
;   exercise [4.6]).
;   
;   a.  Change lookup-variable-value (section [4.1.3]) to signal an error if
;   the value it finds is the symbol *unassigned*.
;   
;   b.  Write a procedure scan-out-defines that takes a procedure body and
;   returns an equivalent one that has no internal definitions, by making
;   the transformation described above.
;   
;   c.  Install scan-out-defines in the interpreter, either in
;   make-procedure or in procedure-body (see section [4.1.3]).  Which place
;   is better? Why?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.16]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.16
;   [Section 4.1.3]: http://sicp-book.com/book-Z-H-26.html#%_sec_4.1.3
;   [Exercise 4.6]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.6
;   4.1.6 Internal Definitions - p390
;   ------------------------------------------------------------------------

(-start- "4.16")



(--end-- "4.16")

