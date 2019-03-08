#lang sicp

(#%require "common.scm")

;   Exercise 4.78
;   =============
;   
;   Redesign the query language as a nondeterministic program to be
;   implemented using the evaluator of section [4.3], rather than as a
;   stream process.  In this approach, each query will produce a single
;   answer (rather than the stream of all answers) and the user can type
;   try-again to see more answers.  You should find that much of the
;   mechanism we built in this section is subsumed by nondeterministic
;   search and backtracking.  You will probably also find, however, that
;   your new query language has subtle differences in behavior from the one
;   implemented here.  Can you find examples that illustrate this
;   difference?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.78]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.78
;   [Section 4.3]:   http://sicp-book.com/book-Z-H-28.html#%_sec_4.3
;   4.4.4 Implementing the Query System - p489
;   ------------------------------------------------------------------------

(-start- "4.78")



(--end-- "4.78")

