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
;   a.  (restore y) puts into y the last value saved on the stack,
;   regardless of what register that value came from.  This is the way our
;   simulator behaves.  Show how to take advantage of this behavior to
;   eliminate one instruction from the Fibonacci machine of section [5.1.4]
;   (figure [5.12]).
;   
;   b.  (restore y) puts into y the last value saved on the stack, but only
;   if that value was saved from y; otherwise, it signals an error.  Modify
;   the simulator to behave this way.  You will have to change save to put
;   the register name on the stack along with the value.
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



(--end-- "5.11")

