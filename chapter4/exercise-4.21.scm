#lang sicp

(#%require "common.scm")

;   Exercise 4.21
;   =============
;   
;   Amazingly, Louis's intuition in exercise [4.20] is correct.  It is
;   indeed possible to specify recursive procedures without using letrec (or
;   even define), although the method for accomplishing this is much more
;   subtle than Louis imagined.  The following expression computes 10
;   factorial by applying a recursive factorial procedure:⁽²⁷⁾
;   
;   ((lambda (n)
;      ((lambda (fact)
;         (fact fact n))
;       (lambda (ft k)
;         (if (= k 1)
;             1
;             (* k (ft ft (- k 1)))))))
;    10)
;   
;   a. Check (by evaluating the expression) that this really does compute
;   factorials.  Devise an analogous expression for computing Fibonacci
;   numbers.
;   
;   b. Consider the following procedure, which includes mutually recursive
;   internal definitions:
;   
;   (define (f x)
;     (define (even? n)
;       (if (= n 0)
;           true
;           (odd? (- n 1))))
;     (define (odd? n)
;       (if (= n 0)
;           false
;           (even? (- n 1))))
;     (even? x))
;   
;   Fill in the missing expressions to complete an alternative definition of
;   f, which uses neither internal definitions nor letrec:
;   
;   (define (f x)
;     ((lambda (even? odd?)
;        (even? even? odd? x))
;      (lambda (ev? od? n)
;        (if (= n 0) true (od? <??> <??> <??>)))
;      (lambda (ev? od? n)
;        (if (= n 0) false (ev? <??> <??> <??>)))))
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.21]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.21
;   [Exercise 4.20]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.20
;   [Footnote 27]:   http://sicp-book.com/book-Z-H-26.html#footnote_Temp_568
;   4.1.6 Internal Definitions - p392
;   ------------------------------------------------------------------------

(-start- "4.21")

(println "
Part A
======")
(define (factorial n)
  ((lambda (fact)
     (fact fact n))
   (lambda (ft k)
     (if (= k 1)
         1
         (* k (ft ft (- k 1)))))))

(println "
Factorial 10, expect 3628800, got: "
         (factorial 10))

(define (fibonacci n)
  ((lambda (fib)
    (fib fib 1 0 n))
  (lambda (fb a b count)
    (if (= count 0)
        b
        (fb fb (+ a b) a (- count 1))))))

(println "
10th Fibonacci, expect 55, got: "
         (fibonacci 10))

(println "
Part B
======")

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(println "
even? 5, expect #f, got: "
         (f 5))

(--end-- "4.21")

