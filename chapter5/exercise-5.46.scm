#lang sicp

(#%require "common.scm")

;   Exercise 5.46
;   =============
;
;   Carry out an analysis like the one in exercise [5.45] to determine the
;   effectiveness of compiling the tree-recursive Fibonacci procedure
;
;   (define (fib n)
;     (if (< n 2)
;         n
;         (+ (fib (- n 1)) (fib (- n 2)))))
;
;   compared to the effectiveness of using the special-purpose Fibonacci
;   machine of figure [5.12].  (For measurement of the interpreted
;   performance, see exercise [5.29].) For Fibonacci, the time resource used
;   is not linear in n; hence the ratios of stack operations will not
;   approach a limiting value that is independent of n.
;
;   ------------------------------------------------------------------------
;   [Exercise 5.46]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.46
;   [Exercise 5.45]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.45
;   [Exercise 5.29]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.29
;   [Figure 5.12]:   http://sicp-book.com/book-Z-H-31.html#%_fig_5.12
;   5.5.7 Interfacing Compiled Code to the Evaluator - p609
;   ------------------------------------------------------------------------

(-start- "5.46")

(println
 "
Summary
=======

┌─────────────┬──────────────────┬──────────────────┐
│   Fibonacci │   Maximum Depth  │ Number of Pushes │
├─────────────┼──────────────────┼──────────────────┤
│ Interpreted │      5n + 3      │  56*(fib n+1)-34 │
├─────────────┼──────────────────┼──────────────────┤
│    Compiled │      2n + 0      │   7*(fib n+1)-2  │
├─────────────┼──────────────────┼──────────────────┤
│     Machine │      2n - 2      │   4*(fib n+1)-4  │
└─────────────┴──────────────────┴──────────────────┘


Machine
=======

The stats for (fib 4) are from the hand-sumulation in Ex 5.5

┌────┬───────────┬───────────────┬──────────────────┐
│  n │ Fibonacci │ Maximum Depth │ Number of Pushes │
├────┼───────────┼───────────────┼──────────────────┤
│  1 │      1    │        0      │           0      │
├────┼───────────┼───────────────┼──────────────────┤
│  2 │      1    │        2      │           4      │
├────┼───────────┼───────────────┼──────────────────┤
│  3 │      2    │      ...      │         ...      │
├────┼───────────┼───────────────┼──────────────────┤
│  4 │      3    │        6      │          16      │
├────┼───────────┼───────────────┼──────────────────┤
│  5 │      5    │      ...      │         ...      │
├────┴───────────┴───────────────┼──────────────────┤
│                                │   4*(fib n+1)-4  │
└────────────────────────────────┴──────────────────┘

If using the optimizaton from Ex. 5.6 the number of pushes will be smaller,
i.e.: 3*(fib n+1)+3


Iterpreted & Compiled
=====================

┌────┬───────────┬────────────────────────────┬──────────────────────────┐
│    │           │       Interpreted          │        Compiled          │
│    │ Fibonacci ├───────────┬────────────────┼───────────┬──────────────┤
│    │           │ Max Depth │  No of Pushes  │ Max Depth │ No of Pushes │
├────┼───────────┼───────────┼────────────────┼───────────┼──────────────┤
│  1 │      1    │      8    │         22     │      3    │         5    │
├────┼───────────┼───────────┼────────────────┼───────────┼──────────────┤
│  2 │      1    │     13    │         78     │      4    │        12    │
├────┼───────────┼───────────┼────────────────┼───────────┼──────────────┤
│  3 │      2    │     18    │        134     │      6    │        19    │
├────┼───────────┼───────────┼────────────────┼───────────┼──────────────┤
│  4 │      3    │     23    │        246     │      8    │        33    │
├────┼───────────┼───────────┼────────────────┼───────────┼──────────────┤
│  5 │      5    │     28    │        414     │     10    │        54    │
├────┼───────────┼───────────┼────────────────┼───────────┼──────────────┤
│  6 │      8    │     33    │        694     │     12    │        89    │
├────┼───────────┼───────────┼────────────────┼───────────┼──────────────┤
│  7 │     13    │     38    │      1,142     │     14    │       145    │
├────┼───────────┼───────────┼────────────────┼───────────┼──────────────┤
│  8 │     21    │     43    │      1,870     │     16    │       236    │
├────┼───────────┼───────────┼────────────────┼───────────┼──────────────┤
│  9 │     34    │     48    │      3,046     │     18    │       383    │
├────┼───────────┼───────────┼────────────────┼───────────┼──────────────┤
│ 10 │     55    │     53    │      4,950     │     20    │       621    │
├────┼───────────┼───────────┼────────────────┼───────────┼──────────────┤
│ 11 │     89    │     58    │      8,030     │     22    │     1,006    │
├────┴───────────┴───────────┼────────────────┼───────────┼──────────────┤
│                            │ 56*(fib n+1)-34│           │ 7*(fib n+1)-2│
└────────────────────────────┴────────────────┴───────────┴──────────────┘

Comiled Code Stats:
===================")

(#%require "machine-45.scm")
(#%require "compiler-45.scm")
(#%require "ec-evaluator-45.scm")

(define (compile-and-go source commands)
  (let* ((eceval
          (make-machine
           eceval-operations
           explicit-control-evaluator))
         (instructions
          (assemble-instructions
           (assemble (statements
                      (compile source empty-ctenv 'val 'return))
                     eceval))))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'exp commands)
    (set-register-contents! eceval 'flag true)
    ;(trace-on! eceval println)
    ;(reg-trace-on! eceval 'argl print-reg)
    ;(set-breakpoint eceval 'external-entry 1)
    (start eceval)
    (println (stack-stats eceval))))

(define source
  '(define (fib n)
     (if (< n 2)
         n
         (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib n)
  (let ((commands `(fib ,n)))
    (compile-and-go source commands)))

(ignore
 (map (lambda (n)
        (println "")
        (println n)
        (fib n))
      '(1 2 3 4 5 6 7 8 9 10 11)))

(--end-- "5.46")

