#lang sicp

(#%require "common.scm")

;   Exercise 5.31
;   =============
;   
;   In evaluating a procedure application, the explicit-control evaluator
;   always saves and restores the env register around the evaluation of the
;   operator, saves and restores env around the evaluation of each operand
;   (except the final one), saves and restores argl around the evaluation of
;   each operand, and saves and restores proc around the evaluation of the
;   operand sequence.  For each of the following combinations, say which of
;   these save and restore operations are superfluous and thus could be
;   eliminated by the compiler's preserving mechanism:
;   
;   (f 'x 'y)
;   
;   ((f) 'x 'y)
;   
;   (f (g 'x) y)
;   
;   (f (g 'x) 'y)
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.31]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.31
;   5.5.1 Structure of the Compiler - p574
;   ------------------------------------------------------------------------

(-start- "5.31")

(println
 "

┌──────────────────┬───────────┬───────────┬───────────┬───────────┐
│                  │ operator  │ operands  │ operands  │ op'd seq  │
│                  │ env       │ env       │ argl      │ proc      │
├──────────────────┼───────────┼───────────┼───────────┼───────────┤
│ 1: (f 'x 'y)     │ eliminate │ eliminate │ eliminate │ eliminate │
├──────────────────┼───────────┼───────────┼───────────┼───────────┤
│ 2: ((f) 'x 'y)   │ eliminate │ eliminate │ eliminate │ eliminate │
├──────────────────┼───────────┼───────────┼───────────┼───────────┤
│ 3: (f (g 'x) y)  │ eliminate │           │           │           │
├──────────────────┼───────────┼───────────┼───────────┼───────────┤
│ 4: (f (g 'x) 'y) │ eliminate │ eliminate │           │           │
└──────────────────┴───────────┴───────────┴───────────┴───────────┘


Operator Env
============

Only need to save/restore if env could change in evaluating the operator and
we need env to evaluate the operands.  In the case of 2: the environment
would be changed in evaluating (f) but the environment is not needed to
evaluate symbols 'x and 'y.


Operands Env
============

Only need to save/restore between operands if there's an operand expression
that might change env and a subsequent operand that needs env for variable
lookup.  This is only the case for 3:.


Operands Argl
=============

We need to save/restore argl anytime there's an operand that might change
the value of argl.  That's 3: and 4:.


Operaand Sequence Proc
======================

We need to save/restore proc if there's any operand that might change the
value of proc.  That's 3: and 4:.
")

(--end-- "5.31")

