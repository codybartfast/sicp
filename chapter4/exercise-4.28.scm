#lang sicp

(#%require "common.scm")

;   Exercise 4.28
;   =============
;   
;   Eval uses actual-value rather than eval to evaluate the operator before
;   passing it to apply, in order to force the value of the operator. Give
;   an example that demonstrates the need for this forcing.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.28]: http://sicp-book.com/book-Z-H-27.html#%_thm_4.28
;   4.2.2 An Interpreter with Lazy Evaluation - p407
;   ------------------------------------------------------------------------

(-start- "4.28")     

(println "
Forcing will be necessary anytime a function is passed as an argument,
because all arguments are delayed becasue we're lazy.  E.g.:

(define (3something2 operator)
  (operator 3 2))

(3something2 +)

If we didn't use actual-value eval would just return a thunk which cannot be
applied because it's not a procedure.  Actual-value will recognise that it's
a thunk and then use force-it to get the procedure.
")
(--end-- "4.28")

