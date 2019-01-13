#lang sicp

(#%require "common.scm")

;   Exercise 3.71
;   =============
;   
;   Numbers that can be expressed as the sum of two cubes in more than one
;   way are sometimes called Ramanujan numbers, in honor of the
;   mathematician Srinivasa Ramanujan.⁽⁷⁰⁾ Ordered streams of pairs provide
;   an elegant solution to the problem of computing these numbers.  To find
;   a number that can be written as the sum of two cubes in two different
;   ways, we need only generate the stream of pairs of integers (i,j)
;   weighted according to the sum i³ + j³ (see exercise [3.70]), then search
;   the stream for two consecutive pairs with the same weight.  Write a
;   procedure to generate the Ramanujan numbers.  The first such number is
;   1,729.  What are the next five?
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.71]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.71
;   [Exercise 3.70]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.70
;   [Footnote 70]:   http://sicp-book.com/book-Z-H-24.html#footnote_Temp_487
;   3.5.3 Exploiting the Stream Paradigm - p342
;   ------------------------------------------------------------------------

(-start- "3.71")

;; Procs to help display and verify

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

;; Supporting procs

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;; Weighted Pairs

(define (merge-weighted weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((s1car (stream-car s1))
                (s2car (stream-car s2))
                (ws1car (weight s1car))
                (ws2car (weight s2car)))
           (cond ((<= ws1car ws2car)
                  (cons-stream s1car
                               (merge-weighted weight (stream-cdr s1) s2)))
                 ((> ws1car ws2car)
                  (cons-stream s2car
                               (merge-weighted weight s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge-weighted weight (stream-cdr s1)
                                               (stream-cdr s2)))))))))
(define (pairs-weighted weight s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted weight
       (stream-map (lambda (x) (list (stream-car s) x))
                   (stream-cdr t))
       (pairs-weighted weight (stream-cdr s) (stream-cdr t)))))

;; Ramanujan

(define (rama-weight pair)
    (let ((i (car pair))
        (j (cadr pair)))
    (+ (* i i i)
       (* j j j))))

(define weighted-pairs (pairs-weighted rama-weight integers integers))

(define (find-consec s)   
  (define (iter s prev)
    (if (= (rama-weight (stream-car s)) (rama-weight prev))
        (cons-stream
         (list (rama-weight prev) prev (stream-car s))
         (iter (stream-cdr s) (list -1 -1)))
        (iter (stream-cdr s) (stream-car s))))
  (iter s (list -1 -1)))

(define rama-nums (find-consec weighted-pairs))

(prn "Ramanujan numbers:")
(display-list (take 6 rama-nums))


(--end-- "3.71")

