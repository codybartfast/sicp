#lang sicp

(#%require "common.scm")

;   Exercise 5.12
;   =============
;
;   The simulator can be used to help determine the data paths required for
;   implementing a machine with a given controller.  Extend the assembler to
;   store the following information in the machine model:
;
;   * a list of all instructions, with duplicates removed, sorted by
;   instruction type (assign, goto, and so on);
;
;   * a list (without duplicates) of the registers used to hold entry points
;   (these are the registers referenced by goto instructions);
;
;   * a list (without duplicates) of the registers that are saved or
;   restored;
;
;   * for each register, a list (without duplicates) of the sources from
;   which it is assigned (for example, the sources for register val in the
;   factorial machine of figure [5.11] are (const 1) and ((op *) (reg n)
;   (reg val))).
;
;   Extend the message-passing interface to the machine to provide access to
;   this new information.  To test your analyzer, define the Fibonacci
;   machine from figure [5.12] and examine the lists you constructed.
;
;   ------------------------------------------------------------------------
;   [Exercise 5.12]: http://sicp-book.com/book-Z-H-32.html#%_thm_5.12
;   [Figure 5.11]:   http://sicp-book.com/book-Z-H-31.html#%_fig_5.11
;   [Figure 5.12]:   http://sicp-book.com/book-Z-H-31.html#%_fig_5.12
;   5.2.3 Generating Execution Procedures for Instructions - p530
;   ------------------------------------------------------------------------

(-start- "5.12")

(#%require "machine-12.scm")

(let ((n 10)
      (machine (make-machine
                '(n val continue)
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
  (let ((path-info (get-path-info machine)))
    (println "
Instructions:
=============
" (get-insts path-info) "

Entry Registers:
================
" (get-entry-regs path-info) "

Stack Registers:
================
" (get-stack-regs path-info) "

Register Sources:
=================
" (get-reg-sources path-info) "

Evidence things still work (fib 10): "
  (get-register-contents machine 'val))))

(--end-- "5.12")

