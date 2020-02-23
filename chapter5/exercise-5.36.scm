#lang sicp

(#%require "common.scm")

;   Exercise 5.36
;   =============
;   
;   What order of evaluation does our compiler produce for operands of a
;   combination?  Is it left-to-right, right-to-left, or some other order?
;   Where in the compiler is this order determined?  Modify the compiler so
;   that it produces some other order of evaluation.  (See the discussion of
;   order of evaluation for the explicit-control evaluator in section
;   [5.4.1].) How does changing the order of operand evaluation affect the
;   efficiency of the code that constructs the argument list?
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.36]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.36
;   [Section 5.4.1]: http://sicp-book.com/book-Z-H-34.html#%_sec_5.4.1
;   5.5.5 An Example of Compiled Code - p595
;   ------------------------------------------------------------------------

(-start- "5.36")

(println
 "
Our compiler evaluates operands from right to left.  By evaluating the first
arg last we can simply, efficiently cons it with the other args so it is the
first time in argl.

This is implemented in construct-arglist which is called from
compile-application

So with our compiler we evaluate in reverse order so that we can efficiently
construct a list in the correct order.  With the ec-evaluator this
optimisaton wasn't available as we don't know beforehand how many arguments
there are.  As a result primitive application and extend environment needed
to be updated to deal with a 'reversed' argument list.

Evaluating arguments left to right will result in work equivelant to
'reverse' if the arguments are in their declared order in argl.  Even if the
arguments were stored in revese order in argl then the work of reversing
would effectivel have to be done by primitive apply or extend environment.
")
(--end-- "5.36")

