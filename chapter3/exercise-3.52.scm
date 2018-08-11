#lang sicp

(#%require "common.scm")

;   Exercise 3.52
;   =============
;   
;   Consider the sequence of expressions
;   
;   (define sum 0)
;   (define (accum x)
;     (set! sum (+ x sum))
;     sum)
;   (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;   (define y (stream-filter even? seq))
;   (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;                            seq))
;   (stream-ref y 7)
;   (display-stream z)
;   
;   What is the value of sum after each of the above expressions is
;   evaluated?  What is the printed response to evaluating the stream-ref
;   and display-stream expressions?  Would these responses differ if we had
;   implemented (delay <exp>) simply as (lambda () <exp>) without using the
;   optimization provided by memo-proc ?  Explain.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.52]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.52
;   3.5.1 Streams Are Delayed Lists - p325
;   ------------------------------------------------------------------------

(-start- "3.52")
(prn "
(define sum 0)
sum = 0

(define (accum x)
  (set! sum (+ x sum))
  sum)
sum = 0

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
sum = 0

(define y (stream-filter even? seq))
sum = 0

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
sum = 0

(stream-ref y 7)
sum = 136

(display-stream z)
sum = 210  (346 without memoization)

The valus of sequences are

interal:  1 2 3 4  5  6  7  8  9  10 11 12 13 14  15  16
seq:      1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136
y (even):     6 10       28 36       66 78        120 136

Yes it would differ without memoization.  The final sum would be 346 (136 +
210) becaue when z is evaluated seq would be re-evaulated from the beginning
and 1 to 16 would be aggregated twice.")

(--end-- "3.52")

