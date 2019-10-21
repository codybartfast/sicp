#lang sicp

(#%require "common.scm")

;   Exercise 4.67
;   =============
;   
;   Devise a way to install a loop detector in the query system so as to
;   avoid the kinds of simple loops illustrated in the text and in exercise
;   [4.64].  The general idea is that the system should maintain some sort
;   of history of its current chain of deductions and should not begin
;   processing a query that it is already working on.  Describe what kind of
;   information (patterns and frames) is included in this history, and how
;   the check should be made.  (After you study the details of the
;   query-system implementation in section [4.4.4], you may want to modify
;   the system to include your loop detector.)
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.67]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.67
;   [Section 4.4.4]: http://sicp-book.com/book-Z-H-29.html#%_sec_4.4.4
;   [Exercise 4.64]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.64
;   4.4.3 Is Logic Programming Mathematical Logic? - p467
;   ------------------------------------------------------------------------

(-start- "4.67")



(--end-- "4.67")

