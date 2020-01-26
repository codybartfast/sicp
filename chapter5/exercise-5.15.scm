#lang sicp

(#%require "common.scm")

;   Exercise 5.15
;   =============
;   
;   Add instruction counting to the register machine simulation. That is,
;   have the machine model keep track of the number of instructions
;   executed.  Extend the machine model's interface to accept a new message
;   that prints the value of the instruction count and resets the count to
;   zero.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.15]: http://sicp-book.com/book-Z-H-32.html#%_thm_5.15
;   5.2.4 Monitoring Machine Performance - p532
;   ------------------------------------------------------------------------

(-start- "5.15")

(#%require "machine-15.scm")

(define (fact-mach n)
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
    (machine-stats machine)))

(println
 "
fact  5: " (fact-mach 5) "
fact 10: " (fact-mach 10) "
fact 15: " (fact-mach 15) "
fact 20: " (fact-mach 20) "
fact 25: " (fact-mach 25) "

The instruction count is (11 * n) - 6

")

(--end-- "5.15")

