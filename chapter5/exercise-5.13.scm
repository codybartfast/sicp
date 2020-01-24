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



(--end-- "5.13")

