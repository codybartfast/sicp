#lang sicp

(#%require "common.scm")

;   Exercise 5.43
;   =============
;   
;   We argued in section [4.1.6] that internal definitions for block
;   structure should not be considered "real" defines.  Rather, a procedure
;   body should be interpreted as if the internal variables being defined
;   were installed as ordinary lambda variables initialized to their correct
;   values using set!.  Section [4.1.6] and exercise [4.16] showed how to
;   modify the metacircular interpreter to accomplish this by scanning out
;   internal definitions.  Modify the compiler to perform the same
;   transformation before it compiles a procedure body.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.43]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.43
;   [Section 4.1.6]: http://sicp-book.com/book-Z-H-26.html#%_sec_4.1.6
;   [Exercise 4.16]: http://sicp-book.com/book-Z-H-35.html#%_thm_4.16
;   5.5.6 Lexical Addressing - p603
;   ------------------------------------------------------------------------

(-start- "5.43")



(--end-- "5.43")

