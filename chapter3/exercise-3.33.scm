#lang sicp

(#%require "common.scm")

;   Exercise 3.33
;   =============
;   
;   Using primitive multiplier, adder, and constant constraints, define a
;   procedure averager that takes three connectors a, b, and c as inputs and
;   establishes the constraint that the value of c is the average of the
;   values of a and b.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.33]: http://sicp-book.com/book-Z-H-22.html#%_thm_3.33
;   3.3.5 Propagation of Constraints - p295
;   ------------------------------------------------------------------------

(-start- "3.33")
(prn"
          ┌────────┐
     a ───┤a1      │      
          │    +  s├──┐
     b ───┤a2      │  │
          └────────┘  │
                      │u
  ┌───┐ v ┌────────┐  │
  │ 2 ├───┤m1      │  │    
  └───┘   │    *  p├──┘
     c ───┤m2      │
          └────────┘

(define (averager a b c)
  (let ((u (make-connector)
        (v (make-connector))
    (adder a b u)
    (multiplier v c u)
    (constant 2 v)
    'ok))

  ")
