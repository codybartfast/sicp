#lang sicp

(#%require "ea-analyzing-24.scm")
(put-analyzers)

(define eval-exec eval)
(define exec-env the-global-environment)

(#%provide eval-exec
           exec-env)

