#lang sicp

(#%require "common.scm")

;   Exercise 4.38
;   =============
;   
;   Modify the multiple-dwelling procedure to omit the requirement that
;   Smith and Fletcher do not live on adjacent floors.  How many solutions
;   are there to this modified puzzle?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.38]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.38
;   4.3.2 Examples of Nondeterministic Programs - p419
;   ------------------------------------------------------------------------

(-start- "4.38")

(println "
To remove the requirement that Smith and Fletcher not be adjacent we simply
need to remove the line:

    (require (not (= (abs (- smith fletcher)) 1)))

Without this requirement there is a total of five solutions:

  M    M    M    M    S
  F    F    C    C    F
  B    S    S    B    M
  C    C    F    F    C
  S    B    B    S    B

")

(--end-- "4.38")

