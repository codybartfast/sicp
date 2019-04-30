#lang sicp

(#%require "common.scm")

;   Exercise 4.7
;   ============
;   
;   Let* is similar to let, except that the bindings of the let variables
;   are performed sequentially from left to right, and each binding is made
;   in an environment in which all of the preceding bindings are visible. 
;   For example
;   
;   (let* ((x 3)
;          (y (+ x 2))
;          (z (+ x y 5)))
;     (* x z))
;   
;   returns 39.  Explain how a let* expression can be rewritten as a set of
;   nested let expressions, and write a procedure let*->nested-lets that
;   performs this transformation.  If we have already implemented let
;   (exercise [4.6]) and we want to extend the evaluator to handle let*, is
;   it sufficient to add a clause to eval whose action is
;   
;   (eval (let*->nested-lets exp) env)
;   
;   or must we explicitly expand let* in terms of non-derived expressions?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.7]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.7
;   [Exercise 4.6]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.6
;   4.1.2 Representing Expressions - p375
;   ------------------------------------------------------------------------

(-start- "4.7")
(println "
Using the given expression:

    (let* ((x 3)
           (y (+ x 2))
           (z (+ x y 5)))
      (* x z))

this can be rewritten as a set of nested let expressions thus:

    (let ((x 3))
      (let ((y (+ x 2)))
        (let ((z (+ x y 5)))
        (* x z))))

Yes, I think this can then be evaluated as a derivied exprssion using
regular let (I believe this demonstrated below). This would only work if a
new environment is created with each level of let, it is, but it might not
be obvious because there is no explicit use of apply.  But each let
expression creates a derived lambda expression that is applied.")

(#%require "ea-data-directed-03.scm")
(put-evaluators)

;; Helpers used by let and let* evaluators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-call cons)
(define let-pairs cadr)
(define let-pair-id car)
(define let-pair-value cadr)
(define (let-params exp)
  (map let-pair-id (let-pairs exp)))
(define (let-values exp)
  (map let-pair-value (let-pairs exp)))
(define (first-let-pair exp) (car (let-pairs exp)))
(define (rest-let-pairs exp) (cdr (let-pairs exp)))
(define let-body cddr)
(define (make-let pairs body)
  (cons 'let (cons pairs body)))
(define (make-let* pairs body)
  (cons 'let* (cons pairs body)))

;; regular let evaluator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (let->combination exp)
  (make-call
   (make-lambda (let-params exp)
                (let-body exp))
   (let-values exp)))

(define (eval-let exp env)
  (eval (let->combination exp) env))

(put 'eval 'let eval-let)

;; let* evaluator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This could be shorter but there's a double checking for empty pairs
;; because given (let* () exp) as the original expression we probably do
;; want (let () exp) to be returned.  But we don't want (let* ((x 1)) exp)
;; to become (let ((x 1)) (let () exp ))

(define (let*->nested-lets exp)
  (define (wrap-lets pairs body)
    (make-let (list (car pairs))
              (if (pair? (cdr pairs))
                  (list (wrap-lets (cdr pairs) body))
                  body)))
  (let ((pairs (let-pairs exp)))
    (if (pair? pairs)
        (wrap-lets pairs (let-body exp))
        (make-let pairs (let-body exp)))))
        
(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))
(put 'eval 'let* eval-let*)

;; Use it ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define expression '(let* ((x 3)
                           (y (+ x 2))
                           (z (+ x y 5)))
                      (* x z)))

(println "
Evaluating expression using let*->nested-lets:
    " expression "
Expect: 39
Got: "
      (eval
       expression
       the-global-environment) "
")

(--end-- "4.7")

