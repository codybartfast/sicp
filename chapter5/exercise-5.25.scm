#lang sicp

(#%require "common.scm")

;   Exercise 5.25
;   =============
;   
;   Modify the evaluator so that it uses normal-order evaluation, based on
;   the lazy evaluator of section [4.2].
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.25]: http://sicp-book.com/book-Z-H-34.html#%_thm_5.25
;   [Section 4.2]:   http://sicp-book.com/book-Z-H-27.html#%_sec_4.2
;   5.4.3 Conditionals, Assignments, and Definitions - p560
;   ------------------------------------------------------------------------

(-start- "5.25")

(#%require "machine-19.scm")
(#%require "ec-evaluator-25.scm")

(define prog-1
  '(begin
     (define (on-dice? n)
       (if (< n 1)
           false)
           (< n (+ 1 1 2 3)))
     (cond
       ((on-dice? 1) "Hello from First clause")
       ((on-dice? 2) "Hello from Second clause")
       (else "Hello from Else clause"))))

(define (run prog)
  (let ((eceval
         (make-machine
          eceval-operations
          explicit-control-evaluator)))

    (set-register-contents! eceval 'exp prog)
    (set-register-contents! eceval 'env the-global-environment)
    ;(trace-on! eceval println)
    (ignore (start eceval))))

(run prog-1)
(--end-- "5.25")

