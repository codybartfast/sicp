#lang sicp

(#%require "common.scm")

;   Exercise 4.71
;   =============
;   
;   Louis Reasoner wonders why the simple-query and disjoin procedures
;   (section [4.4.4.2]) are implemented using explicit delay operations,
;   rather than being defined as follows:
;   
;   (define (simple-query query-pattern frame-stream)
;     (stream-flatmap
;      (lambda (frame)
;        (stream-append (find-assertions query-pattern frame)
;                       (apply-rules query-pattern frame)))
;      frame-stream))
;   (define (disjoin disjuncts frame-stream)
;     (if (empty-disjunction? disjuncts)
;         the-empty-stream
;         (interleave
;          (qeval (first-disjunct disjuncts) frame-stream)
;          (disjoin (rest-disjuncts disjuncts) frame-stream))))
;   
;   Can you give examples of queries where these simpler definitions would
;   lead to undesirable behavior?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.71]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.71
;   [Section 4.4.4.2]: http://sicp-book.com/book-Z-H-29.html#%_sec_4.4.4.2
;   4.4.4 Implementing the Query System - p486
;   ------------------------------------------------------------------------

(-start- "4.71")



(--end-- "4.71")

