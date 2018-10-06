#lang sicp

(#%require "common.scm")

;   Exercise 3.59
;   =============
;   
;   In section [2.5.3] we saw how to implement a polynomial arithmetic
;   system representing polynomials as lists of terms.  In a similar way, we
;   can work with power series, such as
;   
;                x²    x³     x⁴
;   eˣ = 1 + x + ── + ─── + ───── + ···,
;                2    3·2   4·3·2
;   
;               r²     r⁴
;   cos x = 1 - ── + ───── - ···,
;               2    4·3·2
;   
;                r³       r⁵
;   sin x = r - ───  + ─────── - ···,
;               3.2    5.4·3·2
;   
;   represented as infinite streams. We will represent the series a₀ + a₁ x
;   + a₂ x² + a₃ x³ + ··· as the stream whose elements are the coefficients
;   a₀, a₁, a₂, a₃, ....
;   
;   a. The integral of the series a₀ + a₁ x + a₂ x² + a₃ x³ + ··· is the
;   series
;   
;             1        1        1
;   c = a₀x + ─ a₁x² + ─ a₂x3 + ─ a₃x⁴ + ···,
;             2        3        4
;   
;   where c is any constant. Define a procedure integrate-series that takes
;   as input a stream a₀, a₁, a₂, ... representing a power series and
;   returns the stream a₀, (1/2)a₁, (1/3)a₂, ... of coefficients of the
;   non-constant terms of the integral of the series. (Since the result has
;   no constant term, it doesn't represent a power series; when we use
;   integrate-series, we will cons on the appropriate constant.)
;   
;   b. The function x → e^x is its own derivative.  This implies that e^x
;   and the integral of e^x are the same series, except for the constant
;   term, which is e⁰ = 1. Accordingly, we can generate the series for e^x
;   as
;   
;   (define exp-series
;     (cons-stream 1 (integrate-series exp-series)))
;   
;   Show how to generate the series for sine and cosine, starting from the
;   facts that the derivative of sine is cosine and the derivative of cosine
;   is the negative of sine:
;   
;   (define cosine-series
;     (cons-stream 1 <??>))
;   (define sine-series
;     (cons-stream 0 <??>))
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.59]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.59
;   [Section 2.5.3]: http://sicp-book.com/book-Z-H-18.html#%_sec_2.5.3
;   3.5.2 Infinite Streams - p332
;   ------------------------------------------------------------------------

(-start- "3.59")

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (integrate-series S)
  (stream-map *
              (stream-map (lambda (n) (/ 1 n)) integers)
              S))


(prn "Part a
======
Ok, I'll be honest I didn't use stream-map on my first attempt (doh!), but
checking other folks answers clearly I should be using stream-map!

Initial attempt:

(define (integrate-series S)
  (define (integrate S power)
    (if (stream-null? S)
        the-empty-stream
        (cons-stream
         (* (/ 1 (+ power 1)) (stream-car S))
         (integrate (stream-cdr S) (+ power 1)))))
  (integrate S 0))

So, using stream-map:

(define (integrate-series S)
  (stream-map *
              (stream-map (lambda (n) (/ 1 n)) integers)
              S))

Part b
======
(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))

(define sine-series
  (cons-stream
   0
   (stream-map
    (lambda (n) (- n))
    (integrate-series cosine-series))))
")



(--end-- "3.59")

