#lang sicp

(#%require "common.scm")

;   Exercise 3.62
;   =============
;   
;   Use the results of exercises [3.60] and [3.61] to define a procedure
;   div-series that divides two power series.  Div-series should work for
;   any two series, provided that the denominator series begins with a
;   nonzero constant term.  (If the denominator has a zero constant term,
;   then div-series should signal an error.) Show how to use div-series
;   together with the result of exercise [3.59] to generate the power series
;   for tangent.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.62]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.62
;   [Exercise 3.60]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.60
;   [Exercise 3.61]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.61
;   [Exercise 3.59]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.59
;   3.5.2 Infinite Streams - p334
;   ------------------------------------------------------------------------

(-start- "3.62")

;; =========
;; From 3.60
;; =========
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams
                (scale-stream (stream-cdr s2) (stream-car s1))
                (mul-series (stream-cdr s1) s2))))

(define (integrate-series S)
  (define (integrate S power)
    (if (stream-null? S)
        the-empty-stream
        (cons-stream
         (* (/ 1 (+ power 1)) (stream-car S))
         (integrate (stream-cdr S) (+ power 1)))))
  (integrate S 0))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define cosine-series
  (cons-stream
   1
   (stream-map
    (lambda (n) (- n))
    (integrate-series sine-series))))

;; =========
;; From 3.61
;; =========

(define (invert-unit-series S)
  (if (= 1 (stream-car S))
         (cons-stream 1
                      (scale-stream
                       (mul-series (stream-cdr S) (invert-unit-series S))
                       -1))
         (error "Constant term of series must be 1")))

;; ==========
;; And now...
;; ==========

(define (div-series N D)
  (define D-constant (stream-car D))
  (if (= 0 D-constant)
      error "Denominator series cannot have constant term of zero")
  (define unit-series
    (scale-stream D (/ 1 D-constant)))
  (define inverted-unit-series
    (invert-unit-series unit-series))
  (scale-stream
   (mul-series N inverted-unit-series)
   D-constant))
  
(define tan-series (div-series sine-series cosine-series))


;; ==========
;; Let's Test
;; ==========


; DEFINE EXPONENTS AS SERIES THEN MUL-SERIES!
(define (eval-power-series series terms x)
  (define (iter S exp x-exp acc)
    (if (>= exp terms)
        acc
        (iter (stream-cdr S)
              (+ exp 1)
              (* x-exp x)
              (+ acc (* (stream-car S) x-exp)))))
  (iter (stream-cdr series)
        1
        x
        (stream-car series)))

(define (tan x)
  (eval-power-series tan-series 150 x))

(prn
 (str "tan 0.5 (expect  0.5463): " (tan 0.5))
 (str "tan 1.0 (expect  1.5574): " (tan 1.0))
 (str "tan 1.5 (expect 14.1014): " (tan 1.5)))

(--end-- "3.62")

