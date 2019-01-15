#lang sicp

(#%require "common.scm")

;   Exercise 3.79
;   =============
;   
;   Generalize the solve-2nd procedure of exercise [3.78] so that it can be
;   used to solve general second-order differential equations d² y/dt² =
;   f(dy/dt, y).
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.79]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.79
;   [Exercise 3.78]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.78
;   3.5.4 Streams and Delayed Evaluation - p349
;   ------------------------------------------------------------------------

(-start- "3.79")

(prn "
The definition of differentials and integrals remains the same so we still
know:

   y = (integral of dy) + (y initial)
  dy = (integral of ddy) + (dy initial)

I.e. from ddy we can get dy, and from dy we can get y.

Fortunately we are told how get ddy from these two:

 ddy = f(dy/dt, y)

Putting these circular definitions together:

(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))    
  y)
")

(--end-- "3.79")

