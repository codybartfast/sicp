#lang sicp

(#%require "common.scm")

;   Exercise 3.36
;   =============
;   
;   Suppose we evaluate the following sequence of expressions in the global
;   environment:
;   
;   (define a (make-connector))
;   (define b (make-connector))
;   (set-value! a 10 'user)
;   
;   At some time during evaluation of the set-value!, the following
;   expression from the connector's local procedure is evaluated:
;   
;   (for-each-except setter inform-about-value constraints)
;   
;   Draw an environment diagram showing the environment in which the above
;   expression is evaluated.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.36]: http://sicp-book.com/book-Z-H-22.html#%_thm_3.36
;   3.3.5 Propagation of Constraints - p295
;   ------------------------------------------------------------------------

(-start- "3.36")

(prn "
┌──────────────────────────────────────────────────────────────────────────┐
│make-connector: ───────> @ @                                              │
│set-value!: ───────────> @ @                                              │
│inform-about-value: ───> @ @                                              │
│for-each-except:  ─────> @ @                                              │
│                                                                          │
│(after calls to make-connector)                                           │
│a:┐                                   b:┐                                 │
└──────────────────────────────────────────────────────────────────────────┘
   │                       ^             │                       ^       
   v                       │             v                       │       
  @ @ ─────┐               │            @ @ ─────┐               │       
           │               │                     │               │       
           V               │                     V               │       
      ┌────────────────────┴──────┐         ┌────────────────────┴──────┐
      │value: false               │         │value: false               │
      │informant: false           │         │informant: false           │
 E1 ─>│constraints: '()           │    E2 ─>│constraints: '()           │
      │set-my-value: ─────> @ @   │         │set-my-value: ─────> @ @   │
      │forget-my-value: ──> @ @   │         │forget-my-value: ──> @ @   │
      │connect: ──────────> @ @   │         │connect: ──────────> @ @   │
      └───────────────────────────┘         └───────────────────────────┘
                               ^
                               │
       call to (set-value! ... │
      ┌───────────────────────────┐
      │connector: ─────> <a>      │
 E3 ─>│new-value: 10              │
      │informant: 'user           │
      └───────────────────────────┘


       call to (me 'set-value! ... 
      ┌───────────────────────────┐
      │request: 'set-value!       │
 E4 ─>│                           ├─> E1
      │                           │
      └───────────────────────────┘


       call to (set-my-value ...
      ┌───────────────────────────┐
      │newval: 10                 │
 E5 ─>│setter: 'user              ├─> E1
      │                           │
      └───────────────────────────┘


       call to (for-each-except ...
      ┌───────────────────────────┐
      │exception: 'user           ├─> E1
 E6 ─>│procedure: ────────────────│─────> <inform-about-value>
      │list: constraints          │
      └───────────────────────────┘
")
(--end-- "3.36")

