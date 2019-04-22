#lang sicp

(#%require "common.scm")

;   Exercise 4.12
;   =============
;   
;   The procedures set-variable-value!, define-variable!, and
;   lookup-variable-value can be expressed in terms of more abstract
;   procedures for traversing the environment structure. Define abstractions
;   that capture the common patterns and redefine the three procedures in
;   terms of these abstractions.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.12]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.12
;   4.1.3 Evaluator Data Structures - p380
;   ------------------------------------------------------------------------

(-start- "4.12")

(#%require "ea-data-directed-11.scm")
(#%require "ea-pick-fruit-expression.scm")
(put-evaluators)


(println "Checking with data-directed eval '12a':")
(check-fruit
 (apply (eval
         pick-fruit
         the-global-environment)
        '()))
(println "")


(--end-- "4.12")

