#lang sicp

(#%require "common.scm")

;   Exercise 3.67
;   =============
;   
;   Modify the pairs procedure so that (pairs integers integers) will
;   produce the stream of all pairs of integers (i,j) (without the condition
;   i ≤ j).  Hint: You will need to mix in an additional stream.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.67]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.67
;   3.5.3 Exploiting the Stream Paradigm - p341
;   ------------------------------------------------------------------------

(-start- "3.67")

;; Functions to verify 'pairs' works as expected.

(define (display-line x)
  (newline)
  (display x))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;; used by 'pairs'

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
;; pairs

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
     (stream-map (lambda (x) (list x (stream-car t)))
                 (stream-cdr s)))
    (pairs (stream-cdr s) (stream-cdr t))))) 

;; verify get expected output

(display-stream
 (let ((i (stream-enumerate-interval 1 5))
       (j (stream-enumerate-interval 1 5)))
   (pairs i j)))

(--end-- "3.67")

