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

;; Unchange functions
(define (eval-cond exp env)
  (eval (cond->if exp) env))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;; New bits
(define (calling-cond? exp)
  (eq? (cadr exp) '=>))
(define calling-cond-actions cddr)

(define (clause->exp clause)
  (if (calling-cond? clause)
      (list (sequence->exp (calling-cond-actions clause))
            (cond-predicate clause))
      (sequence->exp (cond-actions clause))))

;; Modified to call 'clause-exp'
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
            (make-if (cond-predicate first)
                     (clause->exp first)
                     (expand-clauses rest))))))

(put 'eval 'cond eval-cond)

;; Try
;; (uses 'square' added as language primitive)

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

