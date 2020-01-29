#lang sicp

(#%require "common.scm")

;   Exercise 5.23
;   =============
;   
;   Extend the evaluator to handle derived expressions such as cond, let,
;   and so on (section [4.1.2]). You may "cheat" and assume that the syntax
;   transformers such as cond->if are available as machine operations.⁽²⁸⁾
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.23]: http://sicp-book.com/book-Z-H-34.html#%_thm_5.23
;   [Section 4.1.2]: http://sicp-book.com/book-Z-H-26.html#%_sec_4.1.2
;   [Footnote 28]:   http://sicp-book.com/book-Z-H-34.html#footnote_Temp_781
;   5.4.3 Conditionals, Assignments, and Definitions - p560
;   ------------------------------------------------------------------------

(-start- "5.23")

(println
 "
If we assume we have the conversion operations available, e.g., cond-if,
then I would imagine that each would follow an identical pattern, something
like:

eval-dispatch
  ...
  (test (op cond?) (reg exp))
  (branch (label ev-cond->if))
  ...)

ev-cond-if
  (assign exp (cond->if) (reg exp))
  (goto (label eval-dispatch))
")

(--end-- "5.23")

