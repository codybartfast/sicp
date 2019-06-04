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


(define program
  '(begin
     
     (define (cons (x lazy-memo) (y lazy-memo))
       (lambda (m) (m x y)))
     (define (car z)
       (z (lambda (p q) p)))
     (define (cdr z)
       (z (lambda (p q) q)))

     (println (car
      (lambda (m) (m 'a 'b))))

     (println (car (cdr
      (lambda (m) (m 'a (lambda (m) (m 'b 'c)))))))

     (println (car (cdr (cdr 
      (lambda (m) (m 'a (lambda (m) (m 'b (lambda (m) (m 'c '()))))))))))

     (println (cdr (cdr (cdr 
      (lambda (m) (m 'a (lambda (m) (m 'b (lambda (m) (m 'c '()))))))))))


     ))

(eval program the-global-environment)

(--end-- "4.33")


