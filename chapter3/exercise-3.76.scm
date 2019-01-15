#lang sicp

(#%require "common.scm")

;   Exercise 3.76
;   =============
;   
;   Eva Lu Ator has a criticism of Louis's approach in exercise [3.75].  The
;   program he wrote is not modular, because it intermixes the operation of
;   smoothing with the zero-crossing extraction.  For example, the extractor
;   should not have to be changed if Alyssa finds a better way to condition
;   her input signal.  Help Louis by writing a procedure smooth that takes a
;   stream as input and produces a stream in which each element is the
;   average of two successive input stream elements.  Then use smooth as a
;   component to implement the zero-crossing detector in a more modular
;   style.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.76]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.76
;   [Exercise 3.75]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.75
;   3.5.3 Exploiting the Stream Paradigm - p346
;   ------------------------------------------------------------------------

(-start- "3.76")

;; to help verify

(define (display-line x)
  (display "    ")
  (display x)
  (newline))

(define (display-list l)
  (for-each display-line l))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (take n S)
  (define (iter S n list)
    (if (or
         (= n 0)
         (stream-null? S))
        list
        (iter (stream-cdr S) (- n 1) (cons (stream-car S) list))))
  (reverse (iter S n nil)))

;; dependencies

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;; smooth

(define (smooth1 stream last-value)
  (define (avg a b) (/ (+ a b) 2))
  (define (iter s prev)
    (cons-stream
     (avg (stream-car s) prev)
     (iter (stream-cdr s) (stream-car s))))
  (iter stream last-value))

;; or better using the 3.50's stream-map:

(define (smooth stream last-value)
  (stream-map
   (lambda (a b) (/ (+ a b) 2)) 
   (cons-stream last-value stream)
   stream))

;; verify smooth

(prn "Check smooth:")
(display-list (take 10 (smooth integers 0)))

(prn "

The cross deteection with smoothing can be achieved based on
funntion composition:

    (make-zero-crossing (smooth stream) last-value)

or it it could be passed in as a parameter:

(define (make-zero-crossings input-filter input-stream last-value)
  (let ((stream (input-filter input-stream last-value)))
    (cons-stream
     (sign-change-detector (stream-car stream) last-value)
     (make-zero-crossings (stream-cdr stream)
                          (stream-car stream)))))
")

(--end-- "3.76")

