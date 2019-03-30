#lang sicp

(define (put op exp-type evaluator)
  (if #f 'unreachable))

(define (get op exp-type)
  (lambda (x) x))

(#%provide put get)