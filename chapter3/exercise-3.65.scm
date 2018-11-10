#lang sicp

(#%require "common.scm")

;   Exercise 3.65
;   =============
;   
;   Use the series
;   
;             1   1   1
;   ln2 = 1 - ─ + — - — + ···
;             1   3   4
;   
;   to compute three sequences of approximations to the natural logarithm of
;   2, in the same way we did above for π. How rapidly do these sequences
;   converge?
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.65]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.65
;   3.5.3 Exploiting the Stream Paradigm - p338
;   ------------------------------------------------------------------------

(-start- "3.65")

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (ln2-summands n)
  (cons-stream
   (/ 1 n)
   (stream-map - (ln2-summands (+ n 1)))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (partial-sums S)
  (cons-stream
   (stream-car S)
   (add-streams
            (stream-cdr S)
            (partial-sums S))))

(define ln2-approximations
  (partial-sums (ln2-summands 1.0)))

(define (stream-limit S tolerance)
  (let ((a (stream-car S))
        (b (stream-car (stream-cdr S))))
    (if (< (abs (- a b)) tolerance)
        b
        (stream-limit (stream-cdr S) tolerance))))

(prn (str "Expect: 1, got: "  (stream-limit ln2-approximations .5)))
(prn (str "Expect: 0.7, got: "  (stream-limit ln2-approximations .05)))
(prn (str "Expect: 0.69, got: "  (stream-limit ln2-approximations .005)))
(prn (str "Expect: 0.693, got: "  (stream-limit ln2-approximations .0005)))
(prn (str "Expect: 0.6931, got: "
          (stream-limit ln2-approximations .00005)))

(prn "
Convergence is very slow compared to the square root sequence.  Getting
an answer within a tolerance of 0.00005 takes longer than the tolerance of
this student.  I guess it's time taken grows exponentially.  Roughly, to get
from n to n+1 places of accuracy 10^(n+1) extra steps are needed.")
                  
;(define (take S n)
;  (define (iter S n list)
;    (if (or
;         (= n 0)
;         (stream-null? S))
;        list
;        (iter (stream-cdr S) (- n 1) (cons (stream-car S) list))))
;  (reverse (iter S n nil)))
;  
;(take ln2-approximations 50)
(--end-- "3.65")

