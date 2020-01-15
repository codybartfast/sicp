#lang sicp

(#%require "common.scm")

;   Exercise 5.1
;   ============
;
;   Design a register machine to compute factorials using the iterative
;   algorithm specified by the following procedure.  Draw data-path and
;   controller diagrams for this machine.
;
;   (define (factorial n)
;     (define (iter product counter)
;       (if (> counter n)
;           product
;           (iter (* counter product)
;                 (+ counter 1))))
;     (iter 1 1))
;
;   ------------------------------------------------------------------------
;   [Exercise 5.1]:  http://sicp-book.com/book-Z-H-31.html#%_thm_5.1
;   5.1 Designing Register Machines - p494
;   ------------------------------------------------------------------------

(-start- "5.1")

(println
 "
Data-path:
==========

  ┌───────┐ ┌───────┐    ,─.    ┌───────┐
  │   p   │ │   c   ├──>( > )<──┤   n   │
  └─────┬─┘ └───────┘    `─'    └───────┘
    ^   │     │   ^
    │   │     │   │         ^
    X   │     │   X        /1\\
    │   │     │   │        ─┬─
    │   │     ├───│───┐     │
    │   v     v   │   v     v
    │  ─────────  │  ─────────
    │   \\ mul /   │   \\ add /
    │    ──┬──    │    ──┬──
    └──────┘      └──────┘


Controller:
===========

        start
          │
          │
          v
          ^
         / \\ yes
  ┌────>( > )───> done
  │      \\ /
  │       V
  │       │
  │       │no
  │       v
  │  ┌─────────┐
  │  │  p<-mul │
  │  └────┬────┘
  │       │
  │       v
  │  ┌─────────┐
  │  │  c<-add │
  │  └────┬────┘
  └───────┘

")
(--end-- "5.1")

