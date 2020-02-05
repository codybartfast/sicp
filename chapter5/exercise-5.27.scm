#lang sicp

(#%require "common.scm")

;   Exercise 5.27
;   =============
;   
;   For comparison with exercise [5.26], explore the behavior of the
;   following procedure for computing factorials recursively:
;   
;   (define (factorial n)
;     (if (= n 1)
;         1
;         (* (factorial (- n 1)) n)))
;   
;   By running this procedure with the monitored stack, determine, as a
;   function of n, the maximum depth of the stack and the total number of
;   pushes used in evaluating n! for n ≥ 1.  (Again, these functions will be
;   linear.) Summarize your experiments by filling in the following table
;   with the appropriate expressions in terms of n:
;   
;             Maximum depth Number of pushes
;   Recursive                               
;   factorial                               
;   Iterative                               
;   factorial                               
;   
;   The maximum depth is a measure of the amount of space used by the
;   evaluator in carrying out the computation, and the number of pushes
;   correlates well with the time required.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.27]: http://sicp-book.com/book-Z-H-34.html#%_thm_5.27
;   [Exercise 5.26]: http://sicp-book.com/book-Z-H-34.html#%_thm_5.26
;   5.4.4 Running the Evaluator - p564
;   ------------------------------------------------------------------------

(-start- "5.27")

(println
 "
┌─────────────────────┬──────────────────┬──────────────────┐
│                     │   Maximum Depth  │ Number of Pushes │
├─────────────────────┼──────────────────┼──────────────────┤
│ Recursive Factorial │      5n +  3     │     32n + -10    │
├─────────────────────┼──────────────────┼──────────────────┤
│ Iterative Factorial │      0n + 10     │     35n +  35    │
└─────────────────────┴──────────────────┴──────────────────┘

")

(#%require "machine-19.scm")
(#%require "ec-evaluator-24.scm")

(define (progi n)
  (let ((prog-start
         '(begin
            (define (factorial n)
              (define (iter product counter)
                (if (> counter n)
                    product
                    (iter (* counter product)
                          (+ counter 1))))
              (iter 1 1))
            )))
    (append prog-start (list (list 'factorial n)))))

(define (progr n)
  (let ((prog-start
         '(begin
            (define (factorial n)
              (if (= n 1)
                  1
                  (* (factorial (- n 1)) n)))
            )))
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
        (run (progi n))
        (run (progr n)))
      '(1 2 3 4 5 10 11)))

(--end-- "5.27")

