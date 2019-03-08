#lang sicp

(#%require "common.scm")

;   Exercise 4.74
;   =============
;   
;   Alyssa P. Hacker proposes to use a simpler version of stream-flatmap in
;   negate, lisp-value, and find-assertions. She observes that the procedure
;   that is mapped over the frame stream in these cases always produces
;   either the empty stream or a singleton stream, so no interleaving is
;   needed when combining these streams.
;   
;   a. Fill in the missing expressions in Alyssa's program.
;   
;   (define (simple-stream-flatmap proc s)
;     (simple-flatten (stream-map proc s)))
;   
;   (define (simple-flatten stream)
;     (stream-map <??>
;                 (stream-filter <??> stream)))
;   
;   b. Does the query system's behavior change if we change it in this way?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.74]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.74
;   4.4.4 Implementing the Query System - p487
;   ------------------------------------------------------------------------

(-start- "4.74")



(--end-- "4.74")

