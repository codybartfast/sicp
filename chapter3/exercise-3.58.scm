#lang sicp

(#%require "common.scm")

;   Exercise 3.58
;   =============
;   
;   Give an interpretation of the stream computed by the following
;   procedure:
;   
;   (define (expand num den radix)
;     (cons-stream
;      (quotient (* num radix) den)
;      (expand (remainder (* num radix) den) den radix)))
;   
;   (Quotient is a primitive that returns the integer quotient of two
;   integers.) What are the successive elements produced by (expand 1 7 10)
;   ?  What is produced by (expand 3 8 10) ?
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.58]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.58
;   3.5.2 Infinite Streams - p332
;   ------------------------------------------------------------------------

(-start- "3.58")

(prn
"This function appears to return the 'decimal' or base expansion (less the
decimal point) of a fraction in the base of the radix.  E.g., I would expect
(expand 1 7 10) to be the decimal expansion of 1/7, i.e.,

  *         *
  1 4 2 8 5 7 ...

and (expand 3 8 10) to be:

        *
  3 7 5 0 ...

(where * indicates the digit sequence that is repeated.)")

(--end-- "3.58")

