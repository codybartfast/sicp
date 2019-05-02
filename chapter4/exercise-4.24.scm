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

(#%require "exercise-4.24-eval-apply.scm")
(#%require "exercise-4.24-eval-exec.scm")
(#%require "exercise-4.24-search-for-primes.scm")


(eval-apply search-for-primes apply-env)



(--end-- "4.24")

