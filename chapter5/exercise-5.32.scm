#lang sicp

(#%require "common.scm")

;   Exercise 5.32
;   =============
;   
;   Using the preserving mechanism, the compiler will avoid saving and
;   restoring env around the evaluation of the operator of a combination in
;   the case where the operator is a symbol.  We could also build such
;   optimizations into the evaluator.  Indeed, the explicit-control
;   evaluator of section [5.4] already performs a similar optimization, by
;   treating combinations with no operands as a special case.
;   
;   a. Extend the explicit-control evaluator to recognize as a separate
;   class of expressions combinations whose operator is a symbol, and to
;   take advantage of this fact in evaluating such expressions.
;   
;   b. Alyssa P. Hacker suggests that by extending the evaluator to
;   recognize more and more special cases we could incorporate all the
;   compiler's optimizations, and that this would eliminate the advantage of
;   compilation altogether.  What do you think of this idea?
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.32]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.32
;   [Section 5.4]:   http://sicp-book.com/book-Z-H-34.html#%_sec_5.4
;   5.5.1 Structure of the Compiler - p574
;   ------------------------------------------------------------------------

(-start- "5.32")



(--end-- "5.32")

