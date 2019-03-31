#lang sicp

;; eval-apply needs a reference to the underlying 'apply' but this line
;; can't just be put into the eval-apply file (module?) becaue eval and
;; underlying-eval would have the same value.

(define underlying-apply apply)

(#%provide underlying-apply)
