#lang sicp

(#%require "common.scm")

;   Exercise 4.24
;   =============
;   
;   Design and carry out some experiments to compare the speed of the
;   original metacircular evaluator with the version in this section.  Use
;   your results to estimate the fraction of time that is spent in analysis
;   versus execution for various procedures.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.24]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.24
;   4.1.7 Separating Syntactic Analysis from Execution - p398
;   ------------------------------------------------------------------------

(-start- "4.24")

(println "
Using the code from Ex 1.22 with:

    1) racket
    2) orignal eval-apply (ea-data-directed).
    3) new eval-exec (ea-analysing)

The analyzing eval completed in about 55% of the original eval. So,
presumably that means the original eval is spending about 45% of it's time
analyzing (which is only done once with the analysing eval).

Just for the laughs it is also run with racket which is about 50 times
faster below, but is probably about 500 times faster if taking out the
compile time.
")

(#%require (only racket
                 define-namespace-anchor
                 namespace-anchor->namespace))
(define-namespace-anchor env-sicp)

(#%require "exercise-4.24-eval-apply.scm")
(#%require "exercise-4.24-eval-exec.scm")
(#%require "exercise-4.24-search-for-primes.scm")

(define (banner text)
  (println "
======================================
" text "
-------------------
"))
   
(banner "RACKET:")
(time
 (eval search-for-primes-expr (namespace-anchor->namespace env-sicp)))

(banner "EVAL EXECUTE:")
(time
 (eval-exec search-for-primes-expr exec-env))

(banner "EVAL APPLY:")
(time
 (eval-apply search-for-primes-expr apply-env))


;(eval search-for-primes (empty-namespace))
(--end-- "4.24")

