#lang sicp

(#%require "machine-19.scm")
(#%require "ec-evaluator-00.scm")

(define prog
  '(begin
    (define (on-dice? n)
      (if (< n 1)
          false
          (< n 7)))
    ((lambda (n) (on-dice? n)) 5)))

(define eceval
  (make-machine
   eceval-operations
   explicit-control-evaluator))

(set-register-contents! eceval 'exp prog)
(set-register-contents! eceval 'env the-global-environment)
;(trace-on! eceval (lambda (exp) (println "-trace-> " exp)))
;(get-register-contents eceval 'env)
(start eceval)

