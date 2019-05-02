#lang sicp

(#%require "ea-data-directed-24.scm")
(put-evaluators)

(define eval-apply eval)
(define apply-env the-global-environment)

(#%provide eval-apply
           apply-env)
