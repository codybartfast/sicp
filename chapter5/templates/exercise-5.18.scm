#lang sicp

(#%require "common.scm")

;   Exercise 5.18
;   =============
;   
;   Modify the make-register procedure of section [5.2.1] so that registers
;   can be traced. Registers should accept messages that turn tracing on and
;   off.  When a register is traced, assigning a value to the register
;   should print the name of the register, the old contents of the register,
;   and the new contents being assigned.  Extend the interface to the
;   machine model to permit you to turn tracing on and off for designated
;   machine registers.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.18]: http://sicp-book.com/book-Z-H-32.html#%_thm_5.18
;   [Section 5.2.1]: http://sicp-book.com/book-Z-H-32.html#%_sec_5.2.1
;   5.2.4 Monitoring Machine Performance - p532
;   ------------------------------------------------------------------------

(-start- "5.18")



(--end-- "5.18")

