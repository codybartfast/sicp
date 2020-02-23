#lang sicp

(#%require "common.scm")

;   Exercise 5.37
;   =============
;   
;   One way to understand the compiler's preserving mechanism for optimizing
;   stack usage is to see what extra operations would be generated if we did
;   not use this idea.  Modify preserving so that it always generates the
;   save and restore operations. Compile some simple expressions and
;   identify the unnecessary stack operations that are generated. Compare
;   the code to that generated with the preserving mechanism intact.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.37]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.37
;   5.5.5 An Example of Compiled Code - p595
;   ------------------------------------------------------------------------

(-start- "5.37")



(--end-- "5.37")

