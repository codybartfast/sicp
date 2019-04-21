#lang sicp

(#%require "common.scm")

;   Exercise 4.11
;   =============
;   
;   Instead of representing a frame as a pair of lists, we can represent a
;   frame as a list of bindings, where each binding is a name-value pair.
;   Rewrite the environment operations to use this alternative
;   representation.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.11]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.11
;   4.1.3 Evaluator Data Structures - p380
;   ------------------------------------------------------------------------

(-start- "4.11")

(println "
The first element of the frame is the token '*frame* so that if are no
bindings we still have a pair that can be referenced.  It is then followed
by pairs representing var and val bindings.

The rewritten operations are:

(define (make-frame variables values)
  (define frame (list '*frame*))
  (define (iter vars vals)
    (cond ((pair? vars)
           (add-binding-to-frame! (car vars) (car vals) frame)
           (iter (cdr vars) (cdr vals)))
          (else frame)))
  (iter variables values))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

;; (define (extend-environment vars vals base-env)
;;    <no change>

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame-pairs)
      (cond ((null? frame-pairs)
             (env-loop (enclosing-environment env)))
            ((eq? var (car (car frame-pairs)))
                   (cdr (car frame-pairs)))
            (else (scan (cdr frame-pairs)))))
    (if (eq? env the-empty-environment)
        (error \"Unbound variable:\" var)
        (let ((frame (first-frame env)))
          (scan (cdr frame)))))
  (env-loop env))

 (define (set-variable-value! var val env)
   (define (env-loop env)
     (define (scan frame-pairs)
       (cond ((null? frame-pairs)
              (env-loop (enclosing-environment env)))
             ((eq? var (car (car frame-pairs)))
              (set-cdr! (car frame-pairs) val))
             (else (scan (cdr frame-pairs)))))
     (if (eq? env the-empty-environment)
         (error \"Unbound variable -- SET!:\" var)
         (let ((frame (first-frame env)))
           (scan (cdr frame)))))
   (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame-pairs)
      (cond ((null? frame-pairs)
             (add-binding-to-frame! var val frame))
            ((eq? var (car (car frame-pairs)))
             (set-cdr! (car frame) val))
            (else (scan (cdr frame-pairs)))))
    (scan (cdr frame))))

This is implemented in 'ea-data-directed-11' that is tested below.
")



(#%require "ea-data-directed-11.scm")
(#%require "ea-pick-fruit-expression.scm")

(put-evaluators)

;; Try it ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(println "Checking with data-directed eval '11':")
(check-fruit
 (apply (eval
         pick-fruit
         the-global-environment)
        '()))
(println "")

(--end-- "4.11")

