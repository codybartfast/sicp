#lang sicp

(#%require "common.scm")

;   Exercise 5.49
;   =============
;   
;   As an alternative to using the explicit-control evaluator's
;   read-eval-print loop, design a register machine that performs a
;   read-compile-execute-print loop.  That is, the machine should run a loop
;   that reads an expression, compiles it, assembles and executes the
;   resulting code, and prints the result.  This is easy to run in our
;   simulated setup, since we can arrange to call the procedures compile and
;   assemble as "register-machine operations."
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.49]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.49
;   5.5.7 Interfacing Compiled Code to the Evaluator - p610
;   ------------------------------------------------------------------------

(-start- "5.49")



(--end-- "5.49")

