#lang sicp

(#%require "common.scm")

;   Exercise 4.60
;   =============
;   
;   By giving the query
;   
;   (lives-near ?person (Hacker Alyssa P))
;   
;   Alyssa P. Hacker is able to find people who live near her, with whom she
;   can ride to work.  On the other hand, when she tries to find all pairs
;   of people who live near each other by querying
;   
;   (lives-near ?person-1 ?person-2)
;   
;   she notices that each pair of people who live near each other is listed
;   twice; for example,
;   
;   (lives-near (Hacker Alyssa P) (Fect Cy D))
;   (lives-near (Fect Cy D) (Hacker Alyssa P))
;   
;   Why does this happen? Is there a way to find a list of people who live
;   near each other, in which each pair appears only once?  Explain.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.60]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.60
;   4.4.1 Deductive Information Retrieval - p451
;   ------------------------------------------------------------------------

(-start- "4.60")

(println
 "
In a mathematical sense this is right.  If A is near B then B is near A.  So
both are valid solutions.  It is returning valid permutations when we only
want combinations.

We could choose one permutaion of the elements (people) if the elements can
be ordered. Then we can require that the elements satisfy that ordering.
E.g., we could require that the people in a pair be ordered alphabetically.
Something like:

  (and (lives-near ?person-1 ?person-2)
       (lisp-value less-than ?person-1 ?person-2))
")

(--end-- "4.60")

