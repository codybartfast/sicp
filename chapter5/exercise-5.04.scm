#lang sicp

(#%require "common.scm")

;   Exercise 5.4
;   ============
;
;   Specify register machines that implement each of the following
;   procedures.  For each machine, write a controller instruction sequence
;   and draw a diagram showing the data paths.
;
;   a. Recursive exponentiation:
;
;   (define (expt b n)
;     (if (= n 0)
;         1
;         (* b (expt b (- n 1)))))
;
;   b. Iterative exponentiation:
;
;   (define (expt b n)
;     (define (expt-iter counter product)
;       (if (= counter 0)
;           product
;           (expt-iter (- counter 1) (* b product))))
;     (expt-iter n 1))
;
;   ------------------------------------------------------------------------
;   [Exercise 5.4]:  http://sicp-book.com/book-Z-H-31.html#%_thm_5.4
;   5.1.4 Using a Stack to Implement Recursion - p510
;   ------------------------------------------------------------------------

(-start- "5.4")

(println
 "
Recursive Exponentiation
========================

Controller:
-----------

  (controller
     (assign continue (label expn-done))
   test
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (assign continue (label after-expn))
     (assign n (op sub) (reg n) (const 1))
     (goto (label test))
   after-expn
     (restore continue)
     (assign p (op mul) (reg p) (reg b))
     (goto (reg continue))
   base-case
     (assign p (const 1))
     (goto (reg continue))
   expn-done)

Data-path:
----------

         ┌───────┐          ┌───────┐
    ^    │   n   ├───────X─>│ stack │
   /0\\   │       │<─X───────┤       │
   ─┬─   └┬─────┬┘          └─┬─────┘
    │     │  ^  │       ^     │   ^
    v     │  │  │      /1\\    X   │
   ,─.    │  X  │      ─┬─    │   X
  ( = )<──┘  │  v       v     v   │
   `─'       │ ───────────  ┌─────┴─┐
             │  \\  sub  /   │ conti │
             │   ───┬───    │  nue  │
             └──────┘       └───────┘

      ┌───────┐             ┌───────┐
      │   p   ├──┐       ┌──┤   b   │
      └───────┘  v       v  └───────┘
          ^     ───────────
          │      \\  mul  /
          X       ───┬───
          └──────────┘


Iterative Exponentiation
========================

Controller:
-----------

  (controller
   test
     (test (op =) (reg n) (const 0))
     (branch (label expn-done))
     (assign p (op mul) (reg p) (reg b))
     (assign n (op sub) (reg n) (const 1))
     (goto (label test))
   expn-done)

Data-path:
----------

       ^    ┌───────┐
      /0\\   │   n   │
      ─┬─   └┬─────┬┘
       │     │  ^  │       ^
       v     │  │  │      /1\\
      ,─.    │  X  │      ─┬─
     ( = )<──┘  │  v       v
      `─'       │ ───────────
                │  \\  sub  /
                │   ───┬───
                └──────┘

  ┌───────┐             ┌───────┐
  │   p   ├──┐       ┌──┤   b   │
  └───────┘  v       v  └───────┘
      ^     ───────────
      │      \\  mul  /
      X       ───┬───
      └──────────┘
")

(--end-- "5.4")

