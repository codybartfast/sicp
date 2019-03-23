#lang sicp


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
  (display "Got expected fruit: ")
  (display (and 
            (equal? "apple" (car result))
            (equal? "cherry" (cadr result))))
  (newline)
  (display "Got expected trace: ")
  (display (equal?
            '("'getting cherry'" "'getting apple'")
            (caddr result)))
  (newline))

(#%provide
 pick-fruit
 check-fruit)
