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


First use of Seq for 'even':
============================

sum = 0

interval: 1 2 3  4  5  6  7  8  9 10 11 12 13  14  15  16 | 
seq:      1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 |

y (even): - - 6 10  -  - 28 36  -  - 66 78  -   - 120 136 |
ref:      - - 0  1  -  -  2  3  -  -  4  5  -   -   6   7 | (end)

sum = 136


Second use with mem-proc:
=========================

sum = 136

interval:                                                 |  17  18  19  20
mem-proc: 1 2 3  4  5  6  7  8  9 10 11 12 13  14  15  16 |  
seq:      1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 | 153 171 190 210
z (%5=0): - - - 10 15  -  -  - 45 55  -  -  - 105 120   - |   -   - 190 210

sum = 210

Second use without mem-proc:
============================

sum = 136

interval:   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
seq:      137 139 142 146 151 157 164 172 181 191 202 214 227 241 256 272

          ...  17  18  19  20
          ... 289 307 326 346

z (%5=0):   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

                -   -   -   -

sum = 346

Yes it would differ without memoization.  Infact I believe it returns
different results, so it doesn't just affect performance.  The final sum
would be 346 (136 + 210) becaue when z is evaluated seq would be
re-evaulated from the beginning and 1 to 16 would be accumulated twice.")

(--end-- "3.52")

