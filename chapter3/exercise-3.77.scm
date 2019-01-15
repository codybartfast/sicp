#lang sicp

(#%require "common.scm")

;   Exercise 3.77
;   =============
;   
;   The integral procedure used above was analogous to the "implicit"
;   definition of the infinite stream of integers in section [3.5.2]. 
;   Alternatively, we can give a definition of integral that is more like
;   integers-starting-from (also in section [3.5.2]):
;   
;   (define (integral integrand initial-value dt)
;     (cons-stream initial-value
;                  (if (stream-null? integrand)
;                      the-empty-stream
;                      (integral (stream-cdr integrand)
;                                (+ (* dt (stream-car integrand))
;                                   initial-value)
;                                dt))))
;   
;   When used in systems with loops, this procedure has the same problem as
;   does our original version of integral.  Modify the procedure so that it
;   expects the integrand as a delayed argument and hence can be used in the
;   solve procedure shown above.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.77]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.77
;   [Section 3.5.2]: http://sicp-book.com/book-Z-H-24.html#%_sec_3.5.2
;   3.5.4 Streams and Delayed Evaluation - p348
;   ------------------------------------------------------------------------

(-start- "3.77")

(prn "Despite the (to me) complex context we have, e.g., mutually defined
streams, calculus, RC circuits etc... all we need to do here is avoid a
direct reference to 'integrand' because 1) we have applicative order
evaluation of arguments and 2) when using 'solve' the integrand is not yet
defined.  So we just need to make sure integrand isn't referenced
until after the first item is returned from the stream.

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (stream-cdr integrand)
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

But integral is now expecting a deley-ed function (rather than a cons
pair). The correct way to do this porbably to use delay: 

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

However, if '(cons-stream <a> <b>)' is precisely equivalent to
'(cons <a> (delay <b>))' then perhaps the following might also work:

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (cdr integrand)
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))")
(--end-- "3.77")

