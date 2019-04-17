#lang sicp

(#%require "common.scm")

;   Exercise 4.5
;   ============
;   
;   Scheme allows an additional syntax for cond clauses, (<test> =>
;   <recipient>).  If <test> evaluates to a true value, then <recipient> is
;   evaluated.  Its value must be a procedure of one argument; this
;   procedure is then invoked on the value of the <test>, and the result is
;   returned as the value of the cond expression.  For example
;   
;   (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;         (else false))
;   
;   returns 2. Modify the handling of cond so that it supports this extended
;   syntax.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.5]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.5
;   4.1.2 Representing Expressions - p35
;   ------------------------------------------------------------------------

(-start- "4.5")

(#%require "ea-data-directed.scm")
(put-evaluators)

;; Unchanged functions, reproduced here because so they will refer to the
;; new expand-clauses.

(define (eval-cond exp env)
  (eval (cond->if exp) env))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;; New bits used by the updated expand-clauses.
(define make-call cons)
(define (calling-cond? exp)
  (eq? (cadr exp) '=>))
(define calling-cond-actions cddr)

;; Here's the nub:
(define (clause->exp clause predicate-value)
  (if (calling-cond? clause)
      ;; if it's a calling-cond then 'call' the body of clause ...
      (make-call (sequence->exp (calling-cond-actions clause))
            ;; ... with predicate value
            (list predicate-value))
      ;; else just do the usual
      (sequence->exp (cond-actions clause))))

;; Modified to call 'clause->exp'
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND-IF"
                       clauses))
            (let ((predicate-value (cond-predicate first)))
              (make-if predicate-value
                       (clause->exp first predicate-value)
                       (expand-clauses rest)))))))

(put 'eval 'cond eval-cond)

;; Try it ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (uses 'square' added as a language primitive)

(define expression
  '(cond
    (false "FALSE")
    (square => (lambda (f) (f 3)))
    (else "ELSE")))

(println "
Evaluating expression:
    " expression "
expect '9'
")

(eval
 expression
 the-global-environment)

(--end-- "4.5")

