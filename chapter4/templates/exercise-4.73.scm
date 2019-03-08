#lang sicp

(#%require "common.scm")

;   Exercise 4.73
;   =============
;   
;   Why does flatten-stream use delay explicitly? What would be wrong with
;   defining it as follows:
;   
;   (define (flatten-stream stream)
;     (if (stream-null? stream)
;         the-empty-stream
;         (interleave
;          (stream-car stream)
;          (flatten-stream (stream-cdr stream)))))
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.73]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.73
;   4.4.4 Implementing the Query System - p487
;   ------------------------------------------------------------------------

(-start- "4.73")



(--end-- "4.73")

