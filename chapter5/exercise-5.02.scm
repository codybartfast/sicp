#lang sicp

(#%require "common.scm")

;   Exercise 5.2
;   ============
;
;   Use the register-machine language to describe the iterative factorial
;   machine of exercise [5.1].
;
;   ------------------------------------------------------------------------
;   [Exercise 5.2]:  http://sicp-book.com/book-Z-H-31.html#%_thm_5.2
;   [Exercise 5.1]:  http://sicp-book.com/book-Z-H-31.html#%_thm_5.1
;   5.1.1 A Language for Describing Register Machines - p498
;   ------------------------------------------------------------------------

(-start- "5.2")

(controller
 test-c
   (test (op >) (reg c) (reg n))
   (branch (label factorial-done))
   (assign p (op mul) (reg p) (reg c))
   (assign c (op add) (reg c) (const 1))
   (goto (label test-c))
 factorial-done)

(--end-- "5.2")

