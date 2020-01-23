#lang sicp

(#%require "common.scm")

;   Exercise 5.11
;   =============
;
;   When we introduced save and restore in section [5.1.4], we didn't
;   specify what would happen if you tried to restore a register that was
;   not the last one saved, as in the sequence
;
;   (save y)
;   (save x)
;   (restore y)
;
;   There are several reasonable possibilities for the meaning of restore:
;
;   a.  <snip>
;
;   b.  <snip>
;
;   c.  (restore y) puts into y the last value saved from y regardless of
;   what other registers were saved after y and not restored.  Modify the
;   simulator to behave this way.  You will have to associate a separate
;   stack with each register.  You should make the initialize-stack
;   operation initialize all the register stacks.
;
;   ------------------------------------------------------------------------
;   [Exercise 5.11]: http://sicp-book.com/book-Z-H-32.html#%_thm_5.11
;   [Section 5.1.4]: http://sicp-book.com/book-Z-H-31.html#%_sec_5.1.4
;   [Figure 5.12]:   http://sicp-book.com/book-Z-H-31.html#%_fig_5.12
;   5.2.3 Generating Execution Procedures for Instructions - p529
;   ------------------------------------------------------------------------

(-start- "5.11")

(#%require "machine-11c.scm")

(println
 "
Part C
======

Thanks to the good design of the orignal simulator this is a relatively easy
change.  Add a stack to each regester:

  (define (make-register name)
    (let ((contents '*unassigned*)
          (stack (make-stack)))      ;;;
      (define (dispatch message)
        (cond ((eq? message 'get) contents)
              ((eq? message 'set)
               (lambda (value) (set! contents value)))
              ((eq? message 'stack)  ;;;
               stack)
              (else
               (error \"Unknown request -- REGISTER\" message))))
      dispatch))

  (define (get-stack register)
    (register 'stack))

Then update the save and restore procedures to use that stack:

  (define (make-save inst machine stack pc)
    (let ((reg (get-register machine
                             (stack-inst-reg-name inst))))
      (lambda ()
        (push (get-stack reg) (get-contents reg))  ;;;
        (advance-pc pc))))
  (define (make-restore inst machine stack pc)
    (let ((reg (get-register machine
                             (stack-inst-reg-name inst))))
      (lambda ()
        (set-contents! reg (pop (get-stack reg)))  ;;;
        (advance-pc pc))))

Other changes (in machine-11c) are to remove references to the  original
stack.

Evidence things still work (fib 10): "

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
                  ;(restore n)
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

Again the 'optimization' from Part A fails as (restore n) will result in an
attempt to pop from an empty stack.
")

(--end-- "5.11")

