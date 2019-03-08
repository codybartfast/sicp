#lang sicp

(#%require "common.scm")

;   Exercise 4.27
;   =============
;   
;   Suppose we type in the following definitions to the lazy evaluator:
;   
;   (define count 0)
;   (define (id x)
;     (set! count (+ count 1))
;     x)
;   
;   Give the missing values in the following sequence of interactions, and
;   explain your answers.⁽³⁸⁾
;   
;   (define w (id (id 10)))
;   ;;; L-Eval input:
;   count
;   ;;; L-Eval value:
;   <response>
;   ;;; L-Eval input:
;   w
;   ;;; L-Eval value:
;   <response>
;   ;;; L-Eval input:
;   count
;   ;;; L-Eval value:
;   <response>
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.27]: http://sicp-book.com/book-Z-H-27.html#%_thm_4.27
;   [Footnote 38]:   http://sicp-book.com/book-Z-H-27.html#footnote_Temp_587
;   4.2.2 An Interpreter with Lazy Evaluation - p406
;   ------------------------------------------------------------------------

(-start- "4.27")



(--end-- "4.27")

