#lang sicp

(#%require "common.scm")

;   Exercise 5.26
;   =============
;   
;   Use the monitored stack to explore the tail-recursive property of the
;   evaluator (section [5.4.2]).  Start the evaluator and define the
;   iterative factorial procedure from section [1.2.1]:
;   
;   (define (factorial n)
;     (define (iter product counter)
;       (if (> counter n)
;           product
;           (iter (* counter product)
;                 (+ counter 1))))
;     (iter 1 1))
;   
;   Run the procedure with some small values of n.  Record the maximum stack
;   depth and the number of pushes required to compute n! for each of these
;   values.
;   
;   a.  You will find that the maximum depth required to evaluate n! is
;   independent of n.  What is that depth?
;   
;   b.  Determine from your data a formula in terms of n for the total
;   number of push operations used in evaluating n! for any n ≥ 1. Note that
;   the number of operations used is a linear function of n and is thus
;   determined by two constants.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.26]: http://sicp-book.com/book-Z-H-34.html#%_thm_5.26
;   [Section 5.4.2]: http://sicp-book.com/book-Z-H-34.html#%_sec_5.4.2
;   [Section 1.2.1]: http://sicp-book.com/book-Z-H-11.html#%_sec_1.2.1
;   5.4.4 Running the Evaluator - p564
;   ------------------------------------------------------------------------

(-start- "5.26")

(println
 "
Maximum stack depth is: 10

Total stack pushes is: 35n + 35

i.e., a = 35, b = 35

The constant bit is different from other interweb answers, probably because
I made changes so as not to use a repl.
")

(#%require "machine-19.scm")
(#%require "ec-evaluator-24.scm")

(define (prog n)
  (let ((prog-start
         '(begin
            (define (factorial n)
              (define (iter product counter)
                (if (> counter n)
                    product
                    (iter (* counter product)
                          (+ counter 1))))
              (iter 1 1)))))
    (append prog-start (list (list 'factorial n)))))


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
        (run (prog n)))
      '(1 2 3 4 5 10 11)))

(--end-- "5.26")

