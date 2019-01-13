#lang sicp

(#%require "common.scm")

;   Exercise 3.72
;   =============
;   
;   In a similar way to exercise [3.71] generate a stream of all numbers
;   that can be written as the sum of two squares in three different ways
;   (showing how they can be so written).
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.72]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.72
;   [Exercise 3.71]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.71
;   3.5.3 Exploiting the Stream Paradigm - p343
;   ------------------------------------------------------------------------

(-start- "3.72")

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

;; Three way squares

(define (sumSqr-weight pair)
    (let ((i (car pair))
        (j (cadr pair)))
    (+ (* i i)
       (* j j))))

(define weighted-pairs (pairs-weighted sumSqr-weight integers integers))

(define (find-consec s)
  (define non-pair (list 0 0))
  (define (iter s prev prev-prev)
    (if (and (= (sumSqr-weight (stream-car s)) (sumSqr-weight prev))
             (= (sumSqr-weight (stream-car s)) (sumSqr-weight prev-prev)))
        (cons-stream
         (list (sumSqr-weight prev) (stream-car s) prev prev-prev)
         (iter (stream-cdr s) non-pair non-pair))
        (iter (stream-cdr s) (stream-car s) prev)))
  (iter s non-pair non-pair))

(define rama-nums (find-consec weighted-pairs))

(prn "3Way Sum Square numbers:")
(display-list (take 6 rama-nums))

(--end-- "3.72")

