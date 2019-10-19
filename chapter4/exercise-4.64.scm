#lang sicp

(#%require "common.scm")

;   Exercise 4.64
;   =============
;   
;   Louis Reasoner mistakenly deletes the outranked-by rule (section
;   [4.4.1]) from the data base.  When he realizes this, he quickly
;   reinstalls it.  Unfortunately, he makes a slight change in the rule, and
;   types it in as
;   
;   (rule (outranked-by ?staff-person ?boss)
;         (or (supervisor ?staff-person ?boss)
;             (and (outranked-by ?middle-manager ?boss)
;                  (supervisor ?staff-person ?middle-manager))))
;   
;   Just after Louis types this information into the system, DeWitt Aull
;   comes by to find out who outranks Ben Bitdiddle. He issues the query
;   
;   (outranked-by (Bitdiddle Ben) ?who)
;   
;   After answering, the system goes into an infinite loop.  Explain why.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.64]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.64
;   [Section 4.4.1]: http://sicp-book.com/book-Z-H-29.html#%_sec_4.4.1
;   4.4.3 Is Logic Programming Mathematical Logic? - p466
;   ------------------------------------------------------------------------

(-start- "4.64")



(--end-- "4.64")

