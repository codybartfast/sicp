#lang sicp

(#%require "common.scm")

;   Exercise 4.3
;   ============
;   
;   Rewrite eval so that the dispatch is done in data-directed style. 
;   Compare this with the data-directed differentiation procedure of
;   exercise [2.73]. (You may use the car of a compound expression as the
;   type of the expression, as is appropriate for the syntax implemented in
;   this section.) .
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.3]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.3
;   [Exercise 2.73]: http://sicp-book.com/book-Z-H-26.html#%_thm_2.73
;   4.1.2 Representing Expressions - p374
;   ------------------------------------------------------------------------

(-start- "4.3-extra")

(println "
'ea-data-directed' contains a full eval/apply implementaion with a
data-directed eval.  It's in a separated file so it can be used with other
exercises.
")

(#%require "ea-data-directed.scm")
(#%require "ea-pick-fruit-expression.scm")

(put-evaluators)

;; Try it ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(println
 "Checking expression with data-directed eval:")
(check-fruit
 (apply (eval
         pick-fruit
         the-global-environment)
        '()))
(println "")

(--end-- "4.3-extra")

