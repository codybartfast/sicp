#lang sicp

(#%require "common.scm")

;   Exercise 3.29
;   =============
;   
;   Another way to construct an or-gate is as a compound digital logic
;   device, built from and-gates and inverters.  Define a procedure or-gate
;   that accomplishes this.  What is the delay time of the or-gate in terms
;   of and-gate-delay and inverter-delay?
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.29]: http://sicp-book.com/book-Z-H-22.html#%_thm_3.29
;   3.3.4 A Simulator for Digital Circuits - p278
;   ------------------------------------------------------------------------

(-start- "3.29")

(prn "(A and B) equivalent to (not ((not A) and (not B))):

(define (or-gate a1 a2 output)
  (define not-a1 (make-wire))
  (define not-a2 (make-wire))
  (define and-out (make-wire))

  (define inv1 (inverter a1 not-a1))
  (define inv2 (inverter a2 not-a2))
  (define and (and-gate not-a1 not-a2 and-out))
  (define not-and (inverter and-out outpout))

  'ok)

")

(--end-- "3.29")

