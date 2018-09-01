#lang sicp

(#%require "common.scm")

;   Exercise 3.55
;   =============
;   
;   Define a procedure partial-sums that takes as argument a stream S and
;   returns the stream whose elements are S₀, S₀ + S₁, S₀ + S₁ + S₂, .... 
;   For example, (partial-sums integers) should be the stream 1, 3, 6, 10,
;   15, ....
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.55]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.55
;   3.5.2 Infinite Streams - p331
;   ------------------------------------------------------------------------

(-start- "3.55")

(prn "
(define (partial-sums S)
  (cons-stream
   (stream-car S)
   (stream (add-streams
            (stream-cdr S)
            (partial-sums S)))))
")

(--end-- "3.55")

