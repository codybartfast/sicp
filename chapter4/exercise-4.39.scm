#lang sicp

(#%require "common.scm")

;   Exercise 4.39
;   =============
;   
;   Does the order of the restrictions in the multiple-dwelling procedure
;   affect the answer? Does it affect the time to find an answer?  If you
;   think it matters, demonstrate a faster program obtained from the given
;   one by reordering the restrictions.  If you think it does not matter,
;   argue your case.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.39]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.39
;   4.3.2 Examples of Nondeterministic Programs - p419
;   ------------------------------------------------------------------------

(-start- "4.39")

(println "
It won't affect the correctness of the answer.

I don't think performance can be significantly improved.  Generally I think
requirements should be ordered by their 'efficiency':

     cost of testing
  ----------------------
  probability of failing

I.e. do cheap tests first and those that are likely to fail.

distinct? can be thought of as 10 individual tests.  My crude (and possibly
incorrect) calculation suggests they each have about 28% chance of failing

I think this makes the distinct? test roughly as efficient as the other
tests so order won't make a demonstrable difference, but having it first
does (to me) read more intuitively.

But if we do want to tinker for marginal gains then (> miller cooper)
presumably has a 50% failure rate so should be the first requirement.


Why 28%
=======

There are 10 tests:

item1 = item2, item1 = item3, item1 = item4, ...,  item4 = item5

There are 5^5 different values for items.  5! of these have unique members.

So the likelihood of 10 test succeeding is (5! / 5^5) = 0.0384

The likelihood of an individual test succeeding is (5! / 5^5)^0.1 = 0.7218

I.e., chance of failure is approx. 28%

")



(--end-- "4.39")

