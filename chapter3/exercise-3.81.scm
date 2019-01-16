#lang sicp

(#%require "common.scm")

;   Exercise 3.81
;   =============
;   
;   Exercise [3.6] discussed generalizing the random-number generator to
;   allow one to reset the random-number sequence so as to produce
;   repeatable sequences of "random" numbers.  Produce a stream formulation
;   of this same generator that operates on an input stream of requests to
;   generate a new random number or to reset the sequence to a specified
;   value and that produces the desired stream of random numbers.  Don't use
;   assignment in your solution.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.81]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.81
;   [Exercise 3.6]:  http://sicp-book.com/book-Z-H-24.html#%_thm_3.6
;   3.5.5 Modularity of Functional Programs and Modularity of Objects - p353
;   ------------------------------------------------------------------------

(-start- "3.81")

;; to help verify

(define (display-line x)
  (display "    ")
  (display x)
  (newline))

(define (display-stream s)
  (cond ((not (stream-null? s))
         (display-line (stream-car s))
         (display-stream (stream-cdr s)))))
      

;; from 3.06

(define rand-size (expt 2 32))

(define rand-update
  (lambda (x)
    (let ((a 1664525)
          (b 1013904223)
          (m rand-size))
      (modulo
       (+ (* x a) b)
       m))))

;; random stream

(define (rand-stream inputs prev)
  (if (stream-null? inputs)
      '()
      (let ((input (stream-car inputs))
            (method (car (stream-car inputs))))
        (cond
          ((eq? method 'generate)
           (let ((rand (rand-update prev)))
             (cons-stream rand
                          (rand-stream (stream-cdr inputs) rand))))
          ((eq? method 'reset)
           (let ((seed (cadr input)))
             (cons-stream '()
                          (rand-stream (stream-cdr inputs) seed))))
          (else ((error "Unknown Method - " method)))))))

;; demonstrate

(define inputs
  (cons-stream (list 'generate) (cons-stream (list 'generate)
  (cons-stream (list 'generate) (cons-stream (list 'reset 351181)
  (cons-stream (list 'generate) (cons-stream (list 'generate)
  (cons-stream (list 'generate) (cons-stream (list 'generate)
  (cons-stream (list 'generate) (cons-stream (list 'generate)
  (cons-stream (list 'reset 351181) (cons-stream (list 'generate)
  (cons-stream (list 'generate)
  (cons-stream (list 'generate) '() )))))))))))))))
  
(define rands (rand-stream inputs 0))

(prn
 "Performing the same series as steps as we did in Ex 3.6 we would expect
the same output.

Expected:
    1013904223
    1196435762
    3519870697
    (Reset 351181)
    1447905992
    3081727879
    1526097722
    482168145
    2881693308
    2026459051
    (Reset 351181)
    1447905992
    3081727879
    1526097722

Actual:")

(display-stream rands)

(--end-- "3.81")

