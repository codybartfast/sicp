#lang sicp

(#%require "common.scm")

;   Exercise 4.29
;   =============
;   
;   Exhibit a program that you would expect to run much more slowly without
;   memoization than with memoization.  Also, consider the following
;   interaction, where the id procedure is defined as in exercise [4.27] and
;   count starts at 0:
;   
;   (define (square x)
;     (* x x))
;   ;;; L-Eval input:
;   (square (id 10))
;   ;;; L-Eval value:
;   <response>
;   ;;; L-Eval input:
;   count
;   ;;; L-Eval value:
;   <response>
;   
;   Give the responses both when the evaluator memoizes and when it does
;   not.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.29]: http://sicp-book.com/book-Z-H-27.html#%_thm_4.29
;   [Exercise 4.27]: http://sicp-book.com/book-Z-H-27.html#%_thm_4.27
;   4.2.2 An Interpreter with Lazy Evaluation - p407
;   ------------------------------------------------------------------------

(-start- "4.29")

(define program
  '(begin

     (define (fib n)
       (if (> 2 n)
           n
           (+ (fib (- n 1)) (fib (- n 2)))))

     (define (add-repeatedly value count)
       (define (iter remaining)
         (if (= 0 remaining)
             0
             (+ value (iter (- remaining 1)))))
       (iter count))

     (add-repeatedly (fib 16) 100)

     ))

;(#%require "ea-data-directed-27.scm")
;(put-evaluators)
;(time (eval program the-global-environment))

(println"
An example of a program that would benefit from memoization is:

     (define (fib n)
       (if (> 2 n)
           n
           (+ (fib (- n 1)) (fib (- n 2)))))

     (define (add-repeatedly value count)
       (define (iter remaining)
         (if (= 0 remaining)
             0
             (+ value (iter (- remaining 1)))))
       (iter count))

     (add-repeatedly (fib 16) 100)

This will evaluate the value of (fib 16) 100 times if the argument is not
memoized.

   (define (square x)
     (* x x))
   ;;; L-Eval input:
   (square (id 10))
   ;;; L-Eval value:
   <response>          100 with or without memoization
   ;;; L-Eval input:
   count
   ;;; L-Eval value:
   <response>          1 with, 2 without memoization

Because x is referred to twice in the body of square it is evaluated twice.
Therefore, without memoization (id 10) is called twice.  With memoization it
is only called one.
")

;(#%require "ea-data-directed-27.scm")
;(put-evaluators)
;(println "Starting mc-evaluator...")
;(driver-loop)

(--end-- "4.29")

