#lang sicp

(#%require "common.scm")

;   Exercise 5.24
;   =============
;
;   Implement cond as a new basic special form without reducing it to if.
;   You will have to construct a loop that tests the predicates of
;   successive cond clauses until you find one that is true, and then use
;   ev-sequence to evaluate the actions of the clause.
;
;   ------------------------------------------------------------------------
;   [Exercise 5.24]: http://sicp-book.com/book-Z-H-34.html#%_thm_5.24
;   5.4.3 Conditionals, Assignments, and Definitions - p560
;   ------------------------------------------------------------------------

(-start- "5.24")

(#%require "machine-19.scm")
(#%require "ec-evaluator-24.scm")

(println "")

(define prog-1
  '(begin
     (define (on-dice? n)
       (cond ((< n 1) false)
             (else (< n 7))))
     (cond
       ((on-dice? 1) "Hello from First clause")
       ((on-dice? 2) "Hello from Second clause")
       (else "Hello from Else clause"))))

(define prog-2
  '(begin
     (define (on-dice? n)
       (cond ((< n 1) false)
             (else (< n 7))))
     (cond
       ((on-dice? 9) "Hello from First clause")
       ((on-dice? 2) "Hello from Second clause")
       (else "Hello from Else clause"))))

(define prog-Else
  '(begin
     (define (on-dice? n)
       (cond ((< n 1) false)
             (else (< n 7))))
     (cond
       ((on-dice? 0) "Hello from First clause")
       ((on-dice? 7) "Hello from Second clause")
       (else "Hello from Else clause"))))

(define prog-None
  '(begin
     (define (on-dice? n)
       (cond ((< n 1) false)
             (else (< n 7))))
     (cond
       ((on-dice? 0) "Hello from First clause")
       ((on-dice? 7) "Hello from Second clause"))))

(define prog-Error
  '(begin
     (define (on-dice? n)
       (cond ((< n 1) false)
             (else (< n 7))))
     (cond
       ((on-dice? 0) "Hello from First clause")
       (else "Hello from Else clause")
       ((on-dice? 2) "Hello from Third clause"))))

(define (run prog)
  (let ((eceval
         (make-machine
          eceval-operations
          explicit-control-evaluator)))

    (set-register-contents! eceval 'exp prog)
    (set-register-contents! eceval 'env the-global-environment)
    ;(trace-on! eceval println)
    (ignore (start eceval))))

(run prog-1)
(run prog-2)
(run prog-Else)
(run prog-None)
(run prog-Error)

(println "")

(--end-- "5.24")

