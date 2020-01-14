#lang sicp

(#%require "common.scm")

;   Exercise 5.42
;   =============
;   
;   Using find-variable from exercise [5.41], rewrite compile-variable and
;   compile-assignment to output lexical-address instructions.  In cases
;   where find-variable returns not-found (that is, where the variable is
;   not in the compile-time environment), you should have the code
;   generators use the evaluator operations, as before, to search for the
;   binding. (The only place a variable that is not found at compile time
;   can be is in the global environment, which is part of the run-time
;   environment but is not part of the compile-time environment.⁽⁴⁷⁾ Thus,
;   if you wish, you may have the evaluator operations look directly in the
;   global environment, which can be obtained with the operation (op
;   get-global-environment), instead of having them search the whole
;   run-time environment found in env.) Test the modified compiler on a few
;   simple cases, such as the nested lambda combination at the beginning of
;   this section.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.42]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.42
;   [Exercise 5.41]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.41
;   [Footnote 47]:   http://sicp-book.com/book-Z-H-35.html#footnote_Temp_830
;   5.5.6 Lexical Addressing - p602
;   ------------------------------------------------------------------------

(-start- "5.42")



(--end-- "5.42")

