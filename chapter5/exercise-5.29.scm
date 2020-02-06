#lang sicp

(#%require "common.scm")

;   Exercise 5.29
;   =============
;   
;   Monitor the stack operations in the tree-recursive Fibonacci
;   computation:
;   
;   (define (fib n)
;     (if (< n 2)
;         n
;         (+ (fib (- n 1)) (fib (- n 2)))))
;   
;   a.  Give a formula in terms of n for the maximum depth of the stack
;   required to compute Fib(n) for n ≥ 2.  Hint: In section [1.2.2] we
;   argued that the space used by this process grows linearly with n.
;   
;   b.  Give a formula for the total number of pushes used to compute Fib(n)
;   for n ≥ 2.  You should find that the number of pushes (which correlates
;   well with the time used) grows exponentially with n.  Hint: Let S(n) be
;   the number of pushes used in computing Fib(n).  You should be able to
;   argue that there is a formula that expresses S(n) in terms of S(n - 1),
;   S(n - 2), and some fixed "overhead" constant k that is independent of n.
;   Give the formula, and say what k is.  Then show that S(n) can be
;   expressed as a Fib(n + 1) + b and give the values of a and b.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.29]: http://sicp-book.com/book-Z-H-34.html#%_thm_5.29
;   [Section 1.2.2]: http://sicp-book.com/book-Z-H-11.html#%_sec_1.2.2
;   5.4.4 Running the Evaluator - p565
;   ------------------------------------------------------------------------

(-start- "5.29")

(println
 "
Results
=======

┌────┬───────────┬───────────────┬──────────────────┐
│  n │ Fibonacci │ Maximum Depth │ Number of Pushes │
├────┼───────────┼───────────────┼──────────────────┤
│  1 │      1    │        8      │          22      │
├────┼───────────┼───────────────┼──────────────────┤
│  2 │      1    │       13      │          78      │
├────┼───────────┼───────────────┼──────────────────┤
│  3 │      2    │       18      │         134      │
├────┼───────────┼───────────────┼──────────────────┤
│  4 │      3    │       23      │         246      │
├────┼───────────┼───────────────┼──────────────────┤
│  5 │      5    │       28      │         414      │
├────┼───────────┼───────────────┼──────────────────┤
│  6 │      8    │       33      │         694      │
├────┼───────────┼───────────────┼──────────────────┤
│  7 │     13    │       38      │       1,142      │
├────┼───────────┼───────────────┼──────────────────┤
│  8 │     21    │       43      │       1,870      │
├────┼───────────┼───────────────┼──────────────────┤
│  9 │     34    │       48      │       3,046      │
├────┼───────────┼───────────────┼──────────────────┤
│ 10 │     55    │       53      │       4,950      │
├────┼───────────┼───────────────┼──────────────────┤
│ 11 │     89    │       58      │       8,030      │
└────┴───────────┴───────────────┴──────────────────┘


Part A
======

  Max pushes = 5n + 3


Part B
======

Formula for Number of Pushes wrt previous values:

  S(n) = S(n-1) + S(n-2) + 34

Formula for Number of Pushes wrt next Fibonacci number:

  S(n) = 56 Fib(n+1) + -34


Calculation:
============

  S(2):  78 = a.2 + b
  S(3): 134 = a.3 + b

  S(3) - S(2):
    (134 - 78) = (3a + b) - (2b + b)
          56 = (3a - 2a) + (b - b)
           a = 56

  S(2):  78 = a.2 + b
         78 = (56 * 2) + b
   78 - 112 = b 
          b = -34


Without Tail Recursion:
=======================

  Max pushes = 8n + 6

  S(n) = 60 Fib(n+1) + -34


Output:
=======
")

(#%require "machine-19.scm")
(#%require "ec-evaluator-24.scm")

(define (fib-prog n)
  (let ((prog-start
         '(begin
            (define (fib n)
              (if (< n 2)
                  n
                  (+ (fib (- n 1)) (fib (- n 2)))))
         )))
  (append prog-start (list (list 'fib n)))))

(define (run prog)
  (define (printReg reg before after)
    (println "--reg--: " reg ": " before " --> " after))
  (let ((eceval
         (make-machine
          eceval-operations
          explicit-control-evaluator)))

    (set-register-contents! eceval 'exp prog)
    (set-register-contents! eceval 'env the-global-environment)
;    (trace-on! eceval println)
;    (reg-trace-on! eceval 'exp printReg)
;    (reg-trace-on! eceval 'proc printReg)
;    (reg-trace-on! eceval 'argl printReg)
;    (reg-trace-on! eceval 'env printReg)
;    (reg-trace-on! eceval 'val printReg)
;    (reg-trace-on! eceval 'unev printReg)

    (ignore (start eceval))
    (println (stack-stats eceval))
    ))

(ignore
 (map (lambda (n)
        (println "")
        (println n)
        (run (fib-prog n)))
      '(1 2 3 4 5 6 7 8 9 10 11)))

(--end-- "5.29")

