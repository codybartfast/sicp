#lang sicp

(#%require "common.scm")

;   Exercise 3.69
;   =============
;   
;   Write a procedure triples that takes three infinite streams, S, T, and
;   U, and produces the stream of triples (Sᵢ,T_(j),U_(k)) such that i ≤ j ≤
;   k. Use triples to generate the stream of all Pythagorean triples of
;   positive integers, i.e., the triples (i,j,k) such that i ≤ j and i² + j²
;   = k².
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.69]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.69
;   3.5.3 Exploiting the Stream Paradigm - p342
;   ------------------------------------------------------------------------

(-start- "3.69")

;; functions to help verify

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


;; used by 'triples'

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1))))) 

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; triples

(define (triples r s t)
  (cons-stream
   (list (stream-car r) (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (stream-cdr (pairs s t)))
    (triples (stream-cdr r) (stream-cdr s) (stream-cdr t)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (pythagorean? list)
  (let ((i (car list))
        (j (cadr list))
        (k (caddr list)))    
  (= (* k k)
     (+ (* i i)
        (* j j)))))


(define pythagoreans
  (stream-filter pythagorean?
                 (triples integers integers integers)))

(prn "Pythagorean triples")
(display-list (take 5 pythagoreans))

(--end-- "3.69")

