#lang sicp

(#%require "common.scm")

;   Exercise 4.62
;   =============
;   
;   Define rules to implement the last-pair operation of exercise [2.17],
;   which returns a list containing the last element of a nonempty list. 
;   Check your rules on queries such as (last-pair (3) ?x), (last-pair (1 2
;   3) ?x), and (last-pair (2 ?x) (3)). Do your rules work correctly on
;   queries such as (last-pair ?x (3)) ?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.62]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.62
;   [Exercise 2.17]: http://sicp-book.com/book-Z-H-29.html#%_thm_2.17
;   4.4.1 Deductive Information Retrieval - p453
;   ------------------------------------------------------------------------

(-start- "4.62")

(println
 "
Last-pair rules:
================

  (rule (last-pair (?x) (?x)))

  (rule (last-pair (?u . ?v) (?x))
        (last-pair ?v (?x)))

Results:
========

  1a)
    ;;; Query input:
    (last-pair (3) ?x)

    ;;; Query results:
    (last-pair (3) (3))

  2a)
    ;;; Query input:
    (last-pair (1 2 3) ?x)

    ;;; Query results:
    (last-pair (1 2 3) (3))

  3a)
    ;;; Query input:
    (last-pair (2 ?x) (3))

    ;;; Query results:
    (last-pair (2 3) (3))


  4a)
    ;;; Query input:
    (last-pair ?x (3))

    ;;; Query results: <hangs>


Last-pair rules - REVERSED:
===========================

  (rule (last-pair (?u . ?v) (?x))
        (last-pair ?v (?x)))

  (rule (last-pair (?x) (?x)))

Results (Rules Reversed):
=========================

  1b)
    ;;; Query input:
    (last-pair (3) ?x)

    ;;; Query results:
    (last-pair (3) (3))

  2b)
    ;;; Query input:
    (last-pair (1 2 3) ?x)

    ;;; Query results:
    (last-pair (1 2 3) (3))

  3b)
    ;;; Query input:
    (last-pair (2 ?x) (3))

    ;;; Query results:
    (last-pair (2 3) (3))


  4b)
    ;;; Query input:
    (last-pair ?x (3))

    ;;; Query results:
    (last-pair (3) (3))
    (last-pair (?u-20 3) (3))
    (last-pair (?u-20 ?u-22 3) (3))
    (last-pair (?u-20 ?u-22 ?u-24 3) (3))
    (last-pair (?u-20 ?u-22 ?u-24 ?u-26 3) (3))
    (last-pair (?u-20 ?u-22 ?u-24 ?u-26 ?u-28 3) (3))
    (last-pair (?u-20 ?u-22 ?u-24 ?u-26 ?u-28 ?u-30 3) (3))
    (last-pair (?u-20 ?u-22 ?u-24 ?u-26 ?u-28 ?u-30 ?u-32 3) (3))


It seems the parts of the rule definition are evaluated in the opposite
order to which they are defined.  So the 'iterative' step
(last-pair (?u. ?v) ?x) keeps being applied, and because ?x is fixed as (3),
the left hand side keeps being expanded on each iteration indefinately.
So with the first definition it loops indefinately.

When the rule's parts are reversed the guard step, (last-pair (?x) (?x)) is
matched first and the result (last-pair (?3) (?3)) is returned.  It then
returns each of the expansions.  (I'm not sure why the expansions are
returned this time but not with the original order).


Using query system from section 4.4.4:
======================================
Paste the following into the prompt:

  (assert!
    (rule (last-pair (?u . ?v) (?x))
          (last-pair ?v (?x))))

  (assert!
    (rule (last-pair (?x) (?x))))

  (last-pair (3) ?x)

  (last-pair (1 2 3) ?x)

  (last-pair (2 ?x) (3))

  (last-pair ?x (3))
")

(#%require "query-system-71.scm")
(query-driver-loop)


(--end-- "4.62")

