#lang sicp

(#%require "common.scm")

;   Exercise 4.16
;   =============
;   
;   In this exercise we implement the method just described for interpreting
;   internal definitions. We assume that the evaluator supports let (see
;   exercise [4.6]).
;   
;   a.  Change lookup-variable-value (section [4.1.3]) to signal an error if
;   the value it finds is the symbol *unassigned*.
;   
;   b.  Write a procedure scan-out-defines that takes a procedure body and
;   returns an equivalent one that has no internal definitions, by making
;   the transformation described above.
;   
;   c.  Install scan-out-defines in the interpreter, either in
;   make-procedure or in procedure-body (see section [4.1.3]).  Which place
;   is better? Why?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.16]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.16
;   [Section 4.1.3]: http://sicp-book.com/book-Z-H-26.html#%_sec_4.1.3
;   [Exercise 4.6]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.6
;   4.1.6 Internal Definitions - p390
;   ------------------------------------------------------------------------

(-start- "4.16")

(#%require "ea-data-directed-16.scm")
(put-evaluators)
(#%require "ea-pick-fruit-expression.scm")

;; Part B ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(Part A is below because it throws an error)

(println "
=======
Part B:
=======")

(define expression-b
  '((define (add u) (+ u v w))
    (define v (+ w 2))
    (define w 1)
    (display (add 3))))

(println "
Original expression:

    " expression-b "

Scanned out expression:

    " (scan-out-defines expression-b) "
")

;; Part C ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(println "
=======
Part C:
=======")

(println "
I've installed it in make-procedure so it should only be called once when
the procedure is created, whereas if it were installed in procedure-body
then it would be called each time the procedure is applied.

Validating with pick-fruit expression:
")

(check-fruit
 (apply (eval
         pick-fruit
         the-global-environment)
        '()))

;; Part A ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define expression-a
  '(begin
     (define x '*unassigned-token*)
     (println x)))

(println "
=======
Part A:
=======

Evaluating expression:
    " expression-a "

Expect error:
\"       Unassigned variable: x\"
")
(eval expression-a the-global-environment)


(--end-- "4.16")

