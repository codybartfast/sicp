#lang sicp

(#%require "common.scm")

;   Exercise 5.6
;   ============
;
;   Ben Bitdiddle observes that the Fibonacci machine's controller sequence
;   has an extra save and an extra restore, which can be removed to make a
;   faster machine.  Where are these instructions?
;
;   ------------------------------------------------------------------------
;   [Exercise 5.6]:  http://sicp-book.com/book-Z-H-31.html#%_thm_5.6
;   5.1.4 Using a Stack to Implement Recursion - p512
;   ------------------------------------------------------------------------

(-start- "5.6")

(println
 "
These instructions are in the Fibonacci controller after the afterfib-n-1
label:

 afterfib-n-1
   (restore n)
-> (restore continue)
   ;; set up to compute Fib(n - 2)
   (assign n (op -) (reg n) (const 2))
-> (save continue)

These can safely be removed.  Generally some of the conditions for it to be
safe are:

  1a) the restored value isn't used (before it is restored again
  1b) the value isn't updated between the two instructions
  2a) nothing is added to the stack between the two instructions
  2b) nothing is removed to the stack between the two instructions
")
(--end-- "5.6")

