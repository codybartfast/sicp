#lang sicp

(#%require "common.scm")

;   Exercise 3.70
;   =============
;   
;   It would be nice to be able to generate streams in which the pairs
;   appear in some useful order, rather than in the order that results from
;   an ad hoc interleaving process.  We can use a technique similar to the
;   merge procedure of exercise [3.56], if we define a way to say that one
;   pair of integers is "less than" another.  One way to do this is to
;   define a "weighting function" W(i,j) and stipulate that (i₁,j₁) is less
;   than (i₂,j₂) if W(i₁,j₁) < W(i₂,j₂).  Write a procedure merge-weighted
;   that is like merge, except that merge-weighted takes an additional
;   argument weight, which is a procedure that computes the weight of a
;   pair, and is used to determine the order in which elements should appear
;   in the resulting merged stream.⁽⁶⁹⁾ Using this, generalize pairs to a
;   procedure weighted-pairs that takes two streams, together with a
;   procedure that computes a weighting function, and generates the stream
;   of pairs, ordered according to weight.  Use your procedure to generate
;   
;   a. the stream of all pairs of positive integers (i,j) with i ≤ j ordered
;   according to the sum i + j
;   
;   b.  the stream of all pairs of positive integers (i,j) with i ≤ j, where
;   neither i nor j is divisible by 2, 3, or 5, and the pairs are ordered
;   according to the sum 2 i + 3 j + 5 i j.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.70]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.70
;   [Exercise 3.56]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.56
;   [Footnote 69]:   http://sicp-book.com/book-Z-H-24.html#footnote_Temp_485
;   3.5.3 Exploiting the Stream Paradigm - p342
;   ------------------------------------------------------------------------

(-start- "3.70")

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

;; Sorted by sum

(define (sum-weight pair)
  (+ (car pair) (cadr pair)))

(define by-sum (pairs-weighted sum-weight integers integers))

;; 2,3,5 pairs

(define (235-weight pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))

(define (235coprime? pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (< 0
       (* (modulo i 2)
          (modulo i 3)
          (modulo i 5)
          (modulo j 2)
          (modulo j 3)
          (modulo j 5)))))

(define 235s
  (stream-filter 235coprime?
                 (pairs-weighted 235-weight integers integers)))

;; display

(prn "Pairs sorted by sum")
(display-list (take 15 by-sum))

(prn "" "i,j coprime with 2,3,5 ordered by 2i+3j+5ij")
(display-list (take 15 235s))

(--end-- "3.70")

