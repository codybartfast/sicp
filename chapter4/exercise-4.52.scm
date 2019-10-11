#lang sicp

(#%require "common.scm")

;   Exercise 4.52
;   =============
;   
;   Implement a new construct called if-fail that permits the user to catch
;   the failure of an expression.  If-fail takes two expressions.  It
;   evaluates the first expression as usual and returns as usual if the
;   evaluation succeeds.  If the evaluation fails, however, the value of the
;   second expression is returned, as in the following example:
;   
;   ;;; Amb-Eval input:
;   (if-fail (let ((x (an-element-of '(1 3 5))))
;              (require (even? x))
;              x)
;            'all-odd)
;   ;;; Starting a new problem
;   ;;; Amb-Eval value:
;   all-odd
;   ;;; Amb-Eval input:
;   (if-fail (let ((x (an-element-of '(1 3 5 8))))
;              (require (even? x))
;              x)
;            'all-odd)
;   ;;; Starting a new problem
;   ;;; Amb-Eval value:
;   8
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.52]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.52
;   4.3.3 Implementing the <tt>Amb</tt> Evaluator - p436
;   ------------------------------------------------------------------------

(-start- "4.52")



(--end-- "4.52")

