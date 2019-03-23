#lang sicp

(#%require "common.scm")
(#%require "ea-eval-apply.scm")

;   Exercise 4.3
;   ============
;   
;   Rewrite eval so that the dispatch is done in data-directed style. 
;   Compare this with the data-directed differentiation procedure of
;   exercise [2.73]. (You may use the car of a compound expression as the
;   type of the expression, as is appropriate for the syntax implemented in
;   this section.) .
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.3]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.3
;   [Exercise 2.73]: http://sicp-book.com/book-Z-H-26.html#%_thm_2.73
;   4.1.2 Representing Expressions - p374
;   ------------------------------------------------------------------------

(-start- "4.3")

(define pick-fruit
  (lambda ()
    (define trace '())

    (define (get-apple)
      (set! trace (cons "'getting apple'" trace))
      "apple")

    (define (get-cherry)
      (set! trace (cons "'getting cherry'" trace))
      "cherry")

    (define (first-or-second first second which)
      (cond ((equal? which 'first) (first))
            (else (second))))

    (define (result)
      (list
       (first-or-second get-apple get-cherry 'first)
       (first-or-second get-apple get-cherry 'not-first)
       trace))
    
    (list 'quote (result))))

(define (check-fruit result)
  (println "Got expected fruit: "
           (and 
            (equal? "apple" (car result))
            (equal? "cherry" (cadr result))))
  (println "Got expected trace: "
            (equal?
             '("'getting cherry'" "'getting apple'")
             (caddr result))))

(define fruit
  (eval (pick-fruit)
        the-global-environment))

(check-fruit fruit)


(--end-- "4.3")

