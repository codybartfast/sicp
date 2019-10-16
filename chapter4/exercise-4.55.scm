#lang sicp

(#%require "common.scm")

;   Exercise 4.55
;   =============
;   
;   Give simple queries that retrieve the following information from the
;   data base:
;   
;   a. all people supervised by Ben Bitdiddle;
;   
;   b. the names and jobs of all people in the accounting division;
;   
;   c. the names and addresses of all people who live in Slumerville.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.55]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.55
;   4.4.1 Deductive Information Retrieval - p446
;   ------------------------------------------------------------------------

(-start- "4.55")

(println "
    a. (supervisor ?person (Bitdiddle Ben))
    b. (job ?person (accounting . ?title))
    c. (address ?person (Slumerville . ?street-address))
")

(--end-- "4.55")

