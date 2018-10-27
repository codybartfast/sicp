#lang sicp

(#%require "common.scm")

;   Exercise 3.64
;   =============
;   
;   Write a procedure stream-limit that takes as arguments a stream and a
;   number (the tolerance).  It should examine the stream until it finds two
;   successive elements that differ in absolute value by less than the
;   tolerance, and return the second of the two elements.  Using this, we
;   could compute square roots up to a given tolerance by
;   
;   (define (sqrt x tolerance)
;     (stream-limit (sqrt-stream x) tolerance))
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.64]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.64
;   3.5.3 Exploiting the Stream Paradigm - p338
;   ------------------------------------------------------------------------

(-start- "3.64")

(define (stream-limit S tolerance)
  (let ((a (stream-car S))
        (b (stream-car (stream-cdr S))))
    (if (< (abs (- a b)) tolerance)
        b
        (stream-limit (stream-cdr S) tolerance))))
    
        



(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(prn
 "Expect: 1.41421356237"
 (str "got:    " (sqrt 2 0.000000001)))


(--end-- "3.64")

