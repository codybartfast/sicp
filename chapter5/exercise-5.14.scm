#lang sicp

(#%require "common.scm")

;   Exercise 5.14
;   =============
;
;   Measure the number of pushes and the maximum stack depth required to
;   compute n! for various small values of n using the factorial machine
;   shown in figure [5.11].  From your data determine formulas in terms of n
;   for the total number of push operations and the maximum stack depth used
;   in computing n! for any n > 1. Note that each of these is a linear
;   function of n and is thus determined by two constants.  In order to get
;   the statistics printed, you will have to augment the factorial machine
;   with instructions to initialize the stack and print the statistics. You
;   may want to also modify the machine so that it repeatedly reads a value
;   for n, computes the factorial, and prints the result (as we did for the
;   GCD machine in figure [5.4]), so that you will not have to repeatedly
;   invoke get-register-contents, set-register-contents!, and start.
;
;   ------------------------------------------------------------------------
;   [Exercise 5.14]: http://sicp-book.com/book-Z-H-32.html#%_thm_5.14
;   [Figure 5.11]:   http://sicp-book.com/book-Z-H-31.html#%_fig_5.11
;   [Figure 5.4]:    http://sicp-book.com/book-Z-H-31.html#%_fig_5.4
;   5.2.4 Monitoring Machine Performance - p532
;   ------------------------------------------------------------------------

(-start- "5.14")

(#%require "machine-14.scm")

(define (fact-stack n)
  (let ((machine (make-machine
                  (list
                   (list '= =)
                   (list '- -)
                   (list '* *))
                  '((assign continue (label fact-done))
                    fact-loop
                    (test (op =) (reg n) (const 1))
                    (branch (label base-case))
                    (save continue)
                    (save n)
                    (assign n (op -) (reg n) (const 1))
                    (assign continue (label after-fact))
                    (goto (label fact-loop))
                    after-fact
                    (restore n)
                    (restore continue)
                    (assign val (op *) (reg n) (reg val))
                    (goto (reg continue))
                    base-case
                    (assign val (const 1))
                    (goto (reg continue))
                    fact-done))))
    (set-register-contents! machine 'n n)
    (start machine)
    (stack-stats machine)))

(println
 "
fact  5: " (fact-stack 5) "
fact 10: " (fact-stack 10) "
fact 15: " (fact-stack 15) "
fact 20: " (fact-stack 20) "
fact 25: " (fact-stack 25) "

The stats show total pushes and maximum depth are the same and for n! the
number is (2 * n) - 2.  So if we're looking for a linear equation of the
form an + b then a=2 and b=-2.
")

(--end-- "5.14")

