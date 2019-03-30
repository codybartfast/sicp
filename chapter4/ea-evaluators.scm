#lang sicp

(define (put op exp-type evaluator)
  (if #f 'unreachable))

(define (get op exp-type)
  ;(display "get failed!")(newline)
  #f)

(#%provide put get)