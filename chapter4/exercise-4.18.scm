#lang sicp

(#%require "common.scm")

;   Exercise 4.18
;   =============
;   
;   Consider an alternative strategy for scanning out definitions that
;   translates the example in the text to
;   
;   (lambda <vars>
;     (let ((u '*unassigned*)
;           (v '*unassigned*))
;       (let ((a <e1>)
;             (b <e2>))
;         (set! u a)
;         (set! v b))
;       <e3>))
;   
;   Here a and b are meant to represent new variable names, created by the
;   interpreter, that do not appear in the user's program. Consider the
;   solve procedure from section [3.5.4]:
;   
;   (define (solve f y0 dt)
;     (define y (integral (delay dy) y0 dt))
;     (define dy (stream-map f y))
;     y)
;   
;   Will this procedure work if internal definitions are scanned out as
;   shown in this exercise?  What if they are scanned out as shown in the
;   text?  Explain.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.18]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.18
;   [Section 3.5.4]: http://sicp-book.com/book-Z-H-24.html#%_sec_3.5.4
;   4.1.6 Internal Definitions - p390
;   ------------------------------------------------------------------------

(-start- "4.18")

(println "
Yes, I believe this will work.  I think the key factor is that variable y
and dy exist in the environment in which the expressions (integral ...) and
(stream-map ...) are evaluated.  (dy and y are not referenced until delay is
forced or the second element of dy is forced).

With this implementation the enclosing evironment, which just contains u and
v is explicit.

I think this would also work if we didn't scan-out as (referring to the
previoius exercise) u and v would be defined in EnvA which is also the
environment that y and dy are evaluated in.

But the original scan-out won't work.  There y and dy are evaluated in EnvA
but u and v are defined in EnvB.

Note: I'm out of sync with most sicp-ers who conclude that it wouldn't work 
because dy is unassigned when y is defined.

")

(--end-- "4.18")

