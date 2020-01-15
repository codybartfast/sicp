#lang sicp

(#%require "common.scm")

;   Exercise 5.3
;   ============
;
;   Design a machine to compute square roots using Newton's method, as
;   described in section [1.1.7]:
;
;   (define (sqrt x)
;     (define (good-enough? guess)
;       (< (abs (- (square guess) x)) 0.001))
;     (define (improve guess)
;       (average guess (/ x guess)))
;     (define (sqrt-iter guess)
;       (if (good-enough? guess)
;           guess
;           (sqrt-iter (improve guess))))
;     (sqrt-iter 1.0))
;
;   Begin by assuming that good-enough? and improve operations are available
;   as primitives.  Then show how to expand these in terms of arithmetic
;   operations.  Describe each version of the sqrt machine design by drawing
;   a data-path diagram and writing a controller definition in the
;   register-machine language.
;
;   ------------------------------------------------------------------------
;   [Exercise 5.3]:  http://sicp-book.com/book-Z-H-31.html#%_thm_5.3
;   [Section 1.1.7]: http://sicp-book.com/book-Z-H-10.html#%_sec_1.1.7
;   5.1.2 Abstraction in Machine Design - p502
;   ------------------------------------------------------------------------

(-start- "5.3")
(println
 "
With good-enough? And improve
=============================

Controller:
-----------

  (controller
   test-g-e
     (test (op g-e) (reg g) (reg x) (const 0.001))
     (branch (label sqrt-done))
     (assign g (op imp) (reg g) (reg x))
     (goto (label test-g-e))
   sqrt-done)


Data-path:
----------

  ┌───────┐
  │   x   ├──────────┐
  └───────┘          │
                     │
                     │
      ^              │
     / \\             ├───────┐
    /   \\            │       │
   /0.001\\           v       │
   ───┬───          ,─.      │
      └───────────>(g-e)     │
                    `─'      │
                     ^       │
  ┌───────┐          │       │
  │   g   ├──────────┤       │
  └───────┘          │       │
     ^               V       V
     │              ───────────
     X               \\  imp  /
     │                ───┬───
     └───────────────────┘


Arithmetic Only
===============

Controller:
-----------

  (controller
   test-g-e
     (assign t (op mul) (reg g) (reg g))
     (assign t (op sub) (reg x) (reg t))
     (test (op <) (const 0) (reg t))
     (branch (label test-g-e-final))
     (assign t (op sub) (const 0) (reg t))
   test-g-e-final
     (test (op >) (const 0.001) (reg t))
     (branch (label sqrt-done))
     (assign t (op div) (reg x) (reg g))
     (assign t (op add) (reg g) (reg t))
     (assign g (op div) (reg t) (const 2))
     (goto (label test-g-e))
   sqrt-done)

Data-path:
----------

                     ┌──────────────┐
                     │          ┌───┴───┐        ┌───────┐
                     │          v       v        │   t   │
                     │         ───────────       └─────┬─┘
                     │          \\  mul  /          ^   │
                     │           ───┬───           │   │
                     │              └────────────X─┤   │
                     │                             │   │
  ┌───────┐          │                             │   │
  │   x   ├───┬───── │ ─────────┐       ┌───────── │ ──┤
  └───────┘   │      │          v       v          │   │
              │      │         ───────────         │   │
              │      │          \\  sub  /          │   │
              │      │           ───┬───           │   │
              │      │              └────────────X─┤   │
              │      │                             │   │
              │      │       ^                     │   │
              │      │      /0\\                    │   │
              │      │      ─┬─                    │   │
              │      │       │     ,─.             │   │
              │      │       ├───>( < )<────────── │ ──┤
              │      │       │     `─'             │   │
              │      │       │                     │   │
              │      │       │                     │   │
              │      │       └──┐       ┌───────── │ ──┤
              │      │          v       v          │   │
              │      │         ───────────         │   │
              │      │          \\  sub  /          │   │
      ^       │      │           ───┬───           │   │
     / \\      │      │              └────────────X─┤   │
    /   \\     │      │                             │   │
   /0.001\\    │      │                             │   │
   ───┬───    │      │             ,─.             │   │
      └────── │ ──── │ ──────────>( > )<────────── │ ──┤
              │      │             `─'             │   │
              │      │                             │   │
              │      │                             │   │
              │      ├──────────────────┐          │   │
              │      │                  │          │   │
              └───── │ ─────────┐       │          │   │
                     │          V       V          │   │
                     │         ───────────         │   │
                     │          \\  div  /          │   │
                     │           ───┬───           │   │
                     │              └────────────X─┤   │
                     │                             │   │
  ┌───────┐          │                             │   │
  │   g   ├──────────┴──────────┐       ┌───────── │ ──┤
  └───────┘                     v       v          │   │
    ^                          ───────────         │   │
    │                           \\  add  /          │   │
    X                            ───┬───           │   │
    │                               └────────────X─┘   │
    │                                                  │
    │                           ┌──────────────────────┘
    │                           │
    │                           │       ^
    │                           │      /2\\
    │                           │      ─┬─
    │                           V       V
    │                          ───────────
    │                           \\  div  /
    │                            ───┬───
    └───────────────────────────────┘

")
(--end-- "5.3")

