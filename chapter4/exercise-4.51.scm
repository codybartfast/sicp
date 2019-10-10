#lang sicp

(#%require "common.scm")

;   Exercise 4.51
;   =============
;   
;   Implement a new kind of assignment called permanent-set! that is not
;   undone upon failure.  For example, we can choose two distinct elements
;   from a list and count the number of trials required to make a successful
;   choice as follows:
;   
;   (define count 0)
;   (let ((x (an-element-of '(a b c)))
;         (y (an-element-of '(a b c))))
;     (permanent-set! count (+ count 1))
;     (require (not (eq? x y)))
;     (list x y count))
;   ;;; Starting a new problem
;   ;;; Amb-Eval value:
;   (a b 2)
;   ;;; Amb-Eval input:
;   try-again
;   ;;; Amb-Eval value:
;   (a c 3)
;   
;   What values would have been displayed if we had used set! here rather
;   than permanent-set! ?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.51]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.51
;   4.3.3 Implementing the <tt>Amb</tt> Evaluator - p436
;   ------------------------------------------------------------------------

(-start- "4.51")



(--end-- "4.51")

