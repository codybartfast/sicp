#lang sicp

(#%require "common.scm")

;   Exercise 3.60
;   =============
;   
;   With power series represented as streams of coefficients as in exercise
;   [3.59], adding series is implemented by add-streams.  Complete the
;   definition of the following procedure for multiplying series:
;   
;   (define (mul-series s1 s2)
;     (cons-stream <??> (add-streams <??> <??>)))
;   
;   You can test your procedure by verifying that sin² x + cos² x = 1, using
;   the series from exercise [3.59].
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.60]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.60
;   [Exercise 3.59]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.59
;   3.5.2 Infinite Streams - p333
;   ------------------------------------------------------------------------

(-start- "3.60")

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

(define the-series
  (add-streams
   (mul-series sine-series sine-series)
   (mul-series cosine-series cosine-series)))

(define (take S n)
  (define (iter S n list)
    (if (or
         (= n 0)
         (stream-null? S))
        list
        (iter (stream-cdr S) (- n 1) (cons (stream-car S) list))))
  (reverse (iter S n nil)))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define C cosine-series)
(define S sine-series)
(define CS (mul-series cosine-series cosine-series))
(define SS (mul-series sine-series sine-series))
(define T the-series)


(prn "cosine series:")
(display (take C 7))
(prn "\n\nsquare of cosine series:")
(display (take CS 7))
(prn "\n\nsine series:")
(display (take S 7))
(prn "\n\nsquare of sine series:")
(display (take SS 7))
(prn "\n\nsum of square series")
(display (take T 7))

(--end-- "3.60")

