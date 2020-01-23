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
;   b.  (restore y) puts into y the last value saved on the stack, but only
;   if that value was saved from y; otherwise, it signals an error.  Modify
;   the simulator to behave this way.  You will have to change save to put
;   the register name on the stack along with the value.
;   
;   c.  <snip>
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.11]: http://sicp-book.com/book-Z-H-32.html#%_thm_5.11
;   [Section 5.1.4]: http://sicp-book.com/book-Z-H-31.html#%_sec_5.1.4
;   [Figure 5.12]:   http://sicp-book.com/book-Z-H-31.html#%_fig_5.12
;   5.2.3 Generating Execution Procedures for Instructions - p529
;   ------------------------------------------------------------------------

(-start- "5.11")

(#%require "machine-11b.scm")

(println
 "
Part B
======

As implied in the answer we put the register name and its value on the stack
and then verify the register name at restore time:

  (define (make-save inst machine stack pc)
    (let* ((reg-name (stack-inst-reg-name inst))
           (reg (get-register machine reg-name)))
      (lambda ()
        (push stack (make-stack-entry reg-name (get-contents reg)))
        (advance-pc pc))))
  (define (make-restore inst machine stack pc)
    (let* ((reg-name (stack-inst-reg-name inst))
           (reg (get-register machine reg-name)))
      (lambda ()
        (let ((stack-entry (pop stack)))
          (cond
            ((not
              (eq? reg-name (stack-entry-reg-name stack-entry)))
             (error
              \"cannot restore stack to different register -- ASSEMBLE\"
              stack-entry))
            (else
             (set-contents! reg (stack-entry-value stack-entry))
             (advance-pc pc)))))))
  (define (stack-inst-reg-name stack-instruction)
    (cadr stack-instruction))

  (define (make-stack-entry reg-name value)
    (cons reg-name value))
  (define (stack-entry-reg-name stack-entry)
    (car stack-entry))
  (define (stack-entry-value stack-entry)
    (cdr stack-entry))

Original Fibonacci still works (fib 10): "

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

But with the optimization form Part A we now EXPECT an ERROR, (fib 10):
                                             ===============")
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
                  (restore n)
                  ;(assign n (reg val))
                  ;(restore val)
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
         
(--end-- "5.11")

