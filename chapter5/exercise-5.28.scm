#lang sicp

(#%require "common.scm")

;   Exercise 5.28
;   =============
;   
;   Modify the definition of the evaluator by changing eval-sequence as
;   described in section [5.4.2] so that the evaluator is no longer
;   tail-recursive.  Rerun your experiments from exercises [5.26] and [5.27]
;   to demonstrate that both versions of the factorial procedure now require
;   space that grows linearly with their input.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.28]: http://sicp-book.com/book-Z-H-34.html#%_thm_5.28
;   [Section 5.4.2]: http://sicp-book.com/book-Z-H-34.html#%_sec_5.4.2
;   [Exercise 5.26]: http://sicp-book.com/book-Z-H-34.html#%_thm_5.26
;   [Exercise 5.27]: http://sicp-book.com/book-Z-H-34.html#%_thm_5.27
;   5.4.4 Running the Evaluator - p565
;   ------------------------------------------------------------------------

(-start- "5.28")

(println
 "
┌─────────────────────┬──────────────────┬──────────────────┐
│                     │   Maximum Depth  │ Number of Pushes │
├─────────────────────┼──────────────────┼──────────────────┤
│ Recursive Factorial │     34n +  -8    │      8n +   6    │
├─────────────────────┼──────────────────┼──────────────────┤
│ Iterative Factorial │     37n +  41    │      3n +  17    │
└─────────────────────┴──────────────────┴──────────────────┘

")

(#%require "machine-19.scm")
(#%require "ec-evaluator-28.scm")

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

(--end-- "5.28")

