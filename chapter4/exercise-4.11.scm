#lang sicp

(#%require "common.scm")

;   Exercise 4.11
;   =============
;   
;   Instead of representing a frame as a pair of lists, we can represent a
;   frame as a list of bindings, where each binding is a name-value pair.
;   Rewrite the environment operations to use this alternative
;   representation.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.11]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.11
;   4.1.3 Evaluator Data Structures - p380
;   ------------------------------------------------------------------------

(-start- "4.11")

(#%require "ea-data-directed-11.scm")
(#%require "ea-pick-fruit-expression.scm")

(put-evaluators)

;; Try it ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(println "Checking with data-directed eval:")
(check-fruit
 (apply (eval
         pick-fruit
         the-global-environment)
        '()))
(println "")

(--end-- "4.11")

