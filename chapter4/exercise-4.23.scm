#lang sicp

(#%require "common.scm")

;   Exercise 4.23
;   =============
;   
;   Alyssa P. Hacker doesn't understand why analyze-sequence needs to be so
;   complicated.  All the other analysis procedures are straightforward
;   transformations of the corresponding evaluation procedures (or eval
;   clauses) in section [4.1.1]. She expected analyze-sequence to look like
;   this:
;   
;   (define (analyze-sequence exps)
;     (define (execute-sequence procs env)
;       (cond ((null? (cdr procs)) ((car procs) env))
;             (else ((car procs) env)
;                   (execute-sequence (cdr procs) env))))
;     (let ((procs (map analyze exps)))
;       (if (null? procs)
;           (error "Empty sequence -- ANALYZE"))
;       (lambda (env) (execute-sequence procs env))))
;   
;   Eva Lu Ator explains to Alyssa that the version in the text does more of
;   the work of evaluating a sequence at analysis time.  Alyssa's
;   sequence-execution procedure, rather than having the calls to the
;   individual execution procedures built in, loops through the procedures
;   in order to call them: In effect, although the individual expressions in
;   the sequence have been analyzed, the sequence itself has not been.
;   
;   Compare the two versions of analyze-sequence.  For example, consider the
;   common case (typical of procedure bodies) where the sequence has just
;   one expression.  What work will the execution procedure produced by
;   Alyssa's program do?  What about the execution procedure produced by the
;   program in the text above?  How do the two versions compare for a
;   sequence with two expressions?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.23]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.23
;   [Section 4.1.1]: http://sicp-book.com/book-Z-H-26.html#%_sec_4.1.1
;   4.1.7 Separating Syntactic Analysis from Execution - p398
;   ------------------------------------------------------------------------

(-start- "4.23")

(println "
Analysis of the individual expressions still only happens once as procs is
only created oncc.

But Alyssa's always does more work at execution time but it doesn't seem a
significant amount.  With one expression Eva's immediately calls the the
proc for that first expression, but Alyssa's has to first call
execute-sequence then check whether it is the last proc (cdr and a null
comparison) before calling car

With two items we have two calls, two null comaparisons, two calls to car
and two calsl to cdr.  Conceiveably we could have cond being repeatedly
being rewritten to if and begin statements but we can probalby assume the
executing version is also using analysis.  In contrast Eva's just has one
call to 'loop' in addition to the procs.

This iteration over the list of procs doesn't seem as though it would ever
be expensive, but if the procedures were themselves short and the sequence
were heavily recusive then perhaps it could be significant.

When working on Exercise 4.24 I compared timings of Alyssa's version and
there was no noticable difference in speed.")

(--end-- "4.23")

