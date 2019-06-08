#lang sicp

(#%require "common.scm")

;   Exercise 4.33
;   =============
;   
;   Ben Bitdiddle tests the lazy list implementation given above by
;   evaluating the expression
;   
;   (car '(a b c))
;   
;   To his surprise, this produces an error.  After some thought, he
;   realizes that the "lists" obtained by reading in quoted expressions are
;   different from the lists manipulated by the new definitions of cons,
;   car, and cdr.  Modify the evaluator's treatment of quoted expressions so
;   that quoted lists typed at the driver loop will produce true lazy lists.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.33]: http://sicp-book.com/book-Z-H-27.html#%_thm_4.33
;   4.2.3 Streams as Lazy Lists - p411
;   ------------------------------------------------------------------------

(-start- "4.33")

(#%require "ea-data-directed-33.scm")
(put-evaluators)

(define (make-quote exp)
  (list 'quote exp))

(define (eval-quotation exp env)
  (let ((quoted-exp (text-of-quotation exp)))
    (if (not (pair? quoted-exp))
        quoted-exp
        (eval (make-lambda
               (list 'm)
               (list
                (list 'm
                      (make-quote (car quoted-exp))
                      (make-quote (cdr quoted-exp)))))
              env))))

(put 'eval 'quote eval-quotation)

(define program
  '(begin
     
     (define (cons (x lazy-memo) (y lazy-memo))
       (lambda (m) (m x y)))
     (define (car z)
       (z (lambda (p q) p)))
     (define (cdr z)
       (z (lambda (p q) q)))
     (define (cadr z) (car (cdr z)))
     (define (caddr z) (car (cdr (cdr z))))
     (define (cdddr z) (cdr (cdr (cdr z))))

;     (println 'xyx)
;     (println (car '(a b c)))
;     (println (cadr '(a b c)))
;     (println (caddr '(a b c)))
;     (println (cdddr '(a b c)))

     ))

(eval program the-global-environment)
;(driver-loop)

(println "
Sample Output:
==============

> (driver-loop)

;;; M-Eval input:
'xyz

;;; M-Eval value:
xyz

;;; M-Eval input:
'(a b c)

;;; M-Eval value:
(compound-procedure (m) ((m 'a '(b c))) <procedure-env>)

;;; M-Eval input:
(car (cdr '(a b c)))

;;; M-Eval value:
b
")

(--end-- "4.33")
