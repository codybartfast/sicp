#lang sicp

(#%require "common.scm")

;   Exercise 5.13
;   =============
;
;   Modify the simulator so that it uses the controller sequence to
;   determine what registers the machine has rather than requiring a list of
;   registers as an argument to make-machine.  Instead of pre-allocating the
;   registers in make-machine, you can allocate them one at a time when they
;   are first seen during assembly of the instructions.
;
;   ------------------------------------------------------------------------
;   [Exercise 5.13]: http://sicp-book.com/book-Z-H-32.html#%_thm_5.13
;   5.2.3 Generating Execution Procedures for Instructions - p530
;   ------------------------------------------------------------------------

(-start- "5.13")

(#%require "machine-13.scm")

(println "
Added a method get-make-register that allocates the register if it doesn't
already exit.  During assembly get-make-register is used instead of
get-register:

  (define (get-make-register machine reg-name)
    (if (not ((machine 'contains-register?) reg-name))
        ((machine 'allocate-register) reg-name))
    (get-register machine reg-name))

Evidence things still work (fib 10): "

(let ((n 10)
      (machine (make-machine
                (list
                 (list '+ +)
                 (list '- -)
                 (list '< <))
                '((assign continue (label fib-done))
                  fib-loop
                  (test (op <) (reg n) (const 2))
                  (branch (label immediate-answer))
                  (save continue)
                  (assign continue (label afterfib-n-1))
                  (save n)
                  (assign n (op -) (reg n) (const 1))
                  (goto (label fib-loop))
                  afterfib-n-1
                  (restore n)
                  (restore continue)
                  (assign n (op -) (reg n) (const 2))
                  (save continue)
                  (assign continue (label afterfib-n-2))
                  (save val)
                  (goto (label fib-loop))
                  afterfib-n-2
                  (assign n (reg val))
                  (restore val)
                  (restore continue)
                  (assign val (op +) (reg val) (reg n))
                  (goto (reg continue))
                  immediate-answer
                  (assign val (reg n))
                  (goto (reg continue))
                  fib-done))))
  (set-register-contents! machine 'n n)
  (start machine)
  (get-register-contents machine 'val))
"

An alternative would be to analyse the instructions, as in exercise 12, to
get a list of registers and create them all before assembly.
")

(--end-- "5.13")

