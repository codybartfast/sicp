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

(println
 "
I think I'm going to take a break from the Chapter 4 exercises.

This is a great section for understanding the ideas of logic programming and
I really enjoyed working on problems in the language. But a detailed
understanding of how the language is implemented is a little too escoteric
for me at the moment.

I'm probably missing somehting, but the 'internet' says, and sometimes
demonstrates, that the explicit delay is needed in this exercsie to prevent
infinite loops.  But so far I don't see a great explanation of how.  Usually
delay is used to make sure variable bindings happen at the right time, but I
don't see how that comes into play in this case.  So I'm tempted to dive
into this to better understand the nuts and bolts.  I might do that if there
weren't a number of other questions in this section that look like they also
require quite an investment of time - and I'm not feeling the usual love.
Unlike so much of the book I don't feel like I'm learning something
fundamental.
")

(--end-- "4.71")

