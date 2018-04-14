#lang sicp

(#%require "common.scm")

;   Exercise 3.34
;   =============
;   
;   Louis Reasoner wants to build a squarer, a constraint device with two
;   terminals such that the value of connector b on the second terminal will
;   always be the square of the value a on the first terminal.  He proposes
;   the following simple device made from a multiplier:
;   
;   (define (squarer a b)
;     (multiplier a a b))
;   
;   There is a serious flaw in this idea.  Explain.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.34]: http://sicp-book.com/book-Z-H-22.html#%_thm_3.34
;   3.3.5 Propagation of Constraints - p295
;   ------------------------------------------------------------------------

(-start- "3.34")

(prn "I think this will work to square numbers.  E.g., if a is set ot 3,
then b will become 9.  But it will not work in the reverse direction.  We
cannot set b to 16 and expect a to become 4. Practically, the multiplier
only acts when two inputs are set (generally n-1?)so it will never 'act'
when only b is set.  Fundamentally, though a simple division (used by
multiplier) can't be used to calculate a square root.")

(--end-- "3.34")

