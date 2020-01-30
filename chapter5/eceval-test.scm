#lang sicp

(#%require "common.scm")
(#%require "machine-19.scm")
(#%require "ec-evaluator-00.scm")

(define prog
  '(begin
    123
    
    ))

(define eceval
  (make-machine
   eceval-operations
   explicit-control-evaluator))

(set-register-contents! eceval 'exp prog)
(set-register-contents! eceval 'env the-global-environment)
;(trace-on! eceval (lambda (exp) (println "-trace-> " exp)))
(start eceval)
;(get-register-contents eceval 'val)
