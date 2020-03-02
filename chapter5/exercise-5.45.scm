#lang sicp

(#%require "common.scm")

;   Exercise 5.45
;   =============
;   
;   By comparing the stack operations used by compiled code to the stack
;   operations used by the evaluator for the same computation, we can
;   determine the extent to which the compiler optimizes use of the stack,
;   both in speed (reducing the total number of stack operations) and in
;   space (reducing the maximum stack depth).  Comparing this optimized
;   stack use to the performance of a special-purpose machine for the same
;   computation gives some indication of the quality of the compiler.
;   
;   a. Exercise [5.27] asked you to determine, as a function of n, the
;   number of pushes and the maximum stack depth needed by the evaluator to
;   compute n! using the recursive factorial procedure given above. 
;   Exercise [5.14] asked you to do the same measurements for the
;   special-purpose factorial machine shown in figure [5.11]. Now perform
;   the same analysis using the compiled factorial procedure.
;   
;   Take the ratio of the number of pushes in the compiled version to the
;   number of pushes in the interpreted version, and do the same for the
;   maximum stack depth.  Since the number of operations and the stack depth
;   used to compute n! are linear in n, these ratios should approach
;   constants as n becomes large.  What are these constants? Similarly, find
;   the ratios of the stack usage in the special-purpose machine to the
;   usage in the interpreted version.
;   
;   Compare the ratios for special-purpose versus interpreted code to the
;   ratios for compiled versus interpreted code.  You should find that the
;   special-purpose machine does much better than the compiled code, since
;   the hand-tailored controller code should be much better than what is
;   produced by our rudimentary general-purpose compiler.
;   
;   b. Can you suggest improvements to the compiler that would help it
;   generate code that would come closer in performance to the hand-tailored
;   version?
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.45]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.45
;   [Exercise 5.27]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.27
;   [Exercise 5.14]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.14
;   [Figure 5.11]:   http://sicp-book.com/book-Z-H-31.html#%_fig_5.11
;   5.5.7 Interfacing Compiled Code to the Evaluator - p608
;   ------------------------------------------------------------------------

(-start- "5.45")

(#%require "machine-45.scm")
(#%require "compiler-45.scm")
(#%require "ec-evaluator-45.scm")

(define source
  '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

(define eceval
  (make-machine
   eceval-operations
   explicit-control-evaluator))

(define sts (statements (compile source empty-ctenv 'val 'return)))
(define insts (assemble sts eceval))



(define (compile-and-go source script)
  (let* ((eceval
          (make-machine
           eceval-operations
           explicit-control-evaluator))
         (instructions
          (assemble-instructions
           (assemble (statements
                      (compile source empty-ctenv 'val 'return))
                     eceval))))
    ;(set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'exp script)
    (set-register-contents! eceval 'flag true)
    (trace-on! eceval println)
    (set-breakpoint eceval 'after-lambda11 1)
    (start eceval)))
(compile-and-go
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n)))
 '(factorial 5))


;(define (run prog)
;  (define (printReg reg before after)
;    (println "--reg--: " reg ": " before " --> " after))
;  (let ((eceval
;         (make-machine
;          eceval-operations
;          explicit-control-evaluator)))
;
;    (set-register-contents! eceval 'exp prog)
;    (set-register-contents! eceval 'env (the-global-environment))
;    (set-register-contents! eceval 'flag #f)
;    ;(trace-on! eceval println)
;    (ignore (start eceval))))
;
;(define prog1
;  '(begin
;     (* 3 ((lambda (a b) (+ a b)) 1 3))
;     ))
;
;(run prog1)
;
(--end-- "5.45")

