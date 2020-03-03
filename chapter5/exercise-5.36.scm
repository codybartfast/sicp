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

(define expression
  '(define (f x)
     (+ (g (+ 2 x)) x)))

(println
 "
Our compiler evaluates operands from right to left.  By evaluating the first
arg last we can simply, efficiently cons it with the other args so it is the
first item in argl.

This is implemented in construct-arglist which is called from
compile-application.  Specifically:

  (define (construct-arglist operand-codes)
    (let ((operand-codes (reverse operand-codes)))
      ...

So with our compiler we evaluate in reverse order so that we can efficiently
construct a list in the correct order.  With the ec-evaluator this
optimisaton wasn't available as we didn't know beforehand how many arguments
there were.  As a result primitive application and extend environment needed
to deal with a 'reversed' argument list.

To evaluate the arguments from left to right we need only remove the code
that reverses the operand codes.  And rename code-to-get-last-arg to code-
to-get-first-arg:

  (define (construct-arglist operand-codes)
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-first-arg
               ...

If we make this change then argl will contain aguments in the reverse order
to their declaration.  We can either:

  1) reverse this list which will certainly be less efficient, and require
     new or updated extend-environment and apply-primitive-procedure as they
     these expecte argl in 'reverse' order.

  2) Do nothing.  Argl is only used by extend-environment and apply-
     primitive-procedure.  These already expect arguments in 'reverse' order
     because this is how they are provided by evaluator.  This would have no
     effect on the efficiency of constructing argl.

Reversing the order of arguments in the expression from the previous
question, and then having arguments evaluated left to right, results in the
same code as in the previous question (the two reverses cancelling each
other out).

  " expression "

produces the code from Ex 5.35:
")

(#%require "compiler-36.scm")

(compile
 expression
 'val
 'next)

(--end-- "5.36")

