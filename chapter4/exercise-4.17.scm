#lang sicp

(#%require "common.scm")

;   Exercise 4.17
;   =============
;   
;   Draw diagrams of the environment in effect when evaluating the
;   expression <e3> in the procedure in the text, comparing how this will be
;   structured when definitions are interpreted sequentially with how it
;   will be structured if definitions are scanned out as described. Why is
;   there an extra frame in the transformed program?  Explain why this
;   difference in environment structure can never make a difference in the
;   behavior of a correct program.  Design a way to make the interpreter
;   implement the "simultaneous" scope rule for internal definitions without
;   constructing the extra frame.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.17]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.17
;   4.1.6 Internal Definitions - p390
;   ------------------------------------------------------------------------

(-start- "4.17")
(println"
With the procedure in the text there is only one frame added to the
enclosing environment:
                                       ┌─────────────┐
                                EnvA ─>│var1: val1   │
                                       │var2: val2   │
                                       │   ...       ├─> <enclosing env>
                                       │u: eval-of e1│
                                       │v: eval-of e2│
                                       └─────────────┘

With definitions scanned out there is an additonal frame because the let is
implemented as a lambda which creates an additional apply and hence an
additional frame:

           ┌─────────────┐             ┌─────────────┐
    EnvB ─>│u: eval-of e1│      EnvA ─>│var1: val1   │
           │v: eval-of e2│             │var2: val2   ├─> <enclosing env>
           │             ├────────────>│   ...       │
           └─────────────┘             └─────────────┘

With our evaluator this will make a difference if the same parameter name is
used in vars and in the definitions (e.g. vars contains a variable named u)
and that name is used before the define.

With the first model u will have the value from the vars until it is
overwritten by (define u ...).

With the second model u will have the value *unassigned* until (set! u ...)
is called.

To see this difference though, you have to do two things that are
questionable:
     1) define a variable with the same name as a parameter.
     2) access a 'defined' variable name before it is defined.

Instead of creating an extra frame in which u and v are intially set to
*unassigned*, u and v could be added to the vars.

E.g., this:

	((lambda (a)
	   (define u 2)
	   (define v 3)
	   (+ a u v))
	 1)

could be tranformed to:

	((lambda (a u v)
	   (set! u 2)
	   (set! v 3)
	   (+ a u v))
	 1 '*unassigned* '*unassigned*)
")

(--end-- "4.17")

