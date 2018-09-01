#lang sicp

(#%require "common.scm")

;   Exercise 3.53
;   =============
;   
;   Without running the program, describe the elements of the stream defined
;   by
;   
;   (define s (cons-stream 1 (add-streams s s)))
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.53]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.53
;   3.5.2 Infinite Streams - p330
;   ------------------------------------------------------------------------

(-start- "3.53")

(prn "At first guess  would expect:
  1, 2, 4, 8, 16
I.e, n -> 2^n

Thank you Hal and Gerald for not asking for the working.")

(--end-- "3.53")

