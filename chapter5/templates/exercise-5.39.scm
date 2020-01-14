#lang sicp

(#%require "common.scm")

;   Exercise 5.39
;   =============
;   
;   Write a procedure lexical-address-lookup that implements the new lookup
;   operation.  It should take two arguments -- a lexical address and a
;   run-time environment -- and return the value of the variable stored at
;   the specified lexical address.  Lexical-address-lookup should signal an
;   error if the value of the variable is the symbol *unassigned*.⁽⁴⁶⁾ Also
;   write a procedure lexical-address-set! that implements the operation
;   that changes the value of the variable at a specified lexical address.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.39]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.39
;   [Footnote 46]:   http://sicp-book.com/book-Z-H-35.html#footnote_Temp_826
;   5.5.6 Lexical Addressing - p602
;   ------------------------------------------------------------------------

(-start- "5.39")



(--end-- "5.39")

