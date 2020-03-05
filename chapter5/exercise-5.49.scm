#lang sicp

(#%require "common.scm")

;   Exercise 5.49
;   =============
;
;   As an alternative to using the explicit-control evaluator's
;   read-eval-print loop, design a register machine that performs a
;   read-compile-execute-print loop.  That is, the machine should run a loop
;   that reads an expression, compiles it, assembles and executes the
;   resulting code, and prints the result.  This is easy to run in our
;   simulated setup, since we can arrange to call the procedures compile and
;   assemble as "register-machine operations."
;
;   ------------------------------------------------------------------------
;   [Exercise 5.49]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.49
;   5.5.7 Interfacing Compiled Code to the Evaluator - p610
;   ------------------------------------------------------------------------

(-start- "5.49")

(println
 "
Controller
==========

  (define rcep-controller
    '((assign env (op get-global-environment))
      read
      (perform (op prompt-for-input) (const \";;; RCEP-RM input:\"))
      (assign val (op read))
      (assign val (op compile) (reg val))
      (assemble-val)
      (assign continue (label after-execute))
      (goto (reg val))
      after-execute
      (perform (op announce-output) (const \";;; RCEP-RM value:\"))
      (perform (op user-print) (reg val))
      (goto (label read))))

Assemble-val is the same as in the previous exercise.


Operations
==========

  (define operations
    (append eceval-operations
            (list (list 'read read)
                  (list 'compile statements-with-return)
                  (list 'prompt-for-input prompt-for-input)
                  (list 'announce-output announce-output)
                  (list 'user-print user-print))

Statements-with-return is the same as in the previous exercise.


Running the Read-Compile-Execute-Print Loop
===========================================

  (start (make-machine operations rcep-controller))


Data-Path
=========

                                                                    ^
                                                                   / \\
                                          ───────────────────   after-exc
                                           \\ get-globl-env /     /     \\
                                            ───────┬───────     ────┬────
                                                   │                │
                                                 1 x              6 x
                                                   │                │
                                                   v                v
                     ───────────────────     ┌───────────┐    ┌───────────┐
                      \\     read      /      │    env    │    │ continue  │
                       ───────┬───────       └─────┬─────┘    └─────┬─────┘
                              │                    │                │
                              │                    │                │
                            3 x                    │                │
               ┌─────────┐    │    ┌─────────┐     │     ┌──────────┘
               v         │    v    │         v     v     v
───────────────────     ┌┴─────────┴┐     ───────────────────
 \\    compile    /      │    val    │      \\    execute    /
  ─────────────┬─       └─────┬──┬──┘       ─┬─────────────
               │         ^ ^  │  │ ^         │
               └─────x───┘ │  │  │ └───x─────┘
                     4     │  │  │     7
                         5 x  │  │
                           │  │  └─────────────────────┐
                           │  │                        │
                 ┌─────────┘  │                        │
                 │            v                        v
                 │   ───────────────────      ───────────────────
                 │    \\   assemble    /        \\  user-print   /
                 │     ───────┬───────          ───────┬───────
                 └────────────┘                      9 x


                              ^                        ^
                             / \\                      / \\
                            /;;;\\                    /;;;\\
                           /input\\                  /outpt\\
                          ────┬────                ────┬────
                              │                        │
                              v                        v
                     ───────────────────      ───────────────────
                      \\prompt-for-inpt/        \\announce-output/
                       ───────┬───────          ───────┬───────
                            2 x                      8 x


Controller Diagram
==================

    ┌─────────────────────────┐
  1 │ get-global-environment  │
    └────────────┬────────────┘
                 │
                 v
    ┌─────────────────────────┐
  2 │    prompt-for-input     │<───┐
    └────────────┬────────────┘    │
                 │                 │
                 v                 │
    ┌─────────────────────────┐    │
  3 │          read           │    │
    └────────────┬────────────┘    │
                 │                 │
                 v                 │
    ┌─────────────────────────┐    │
  4 │         compile         │    │
    └────────────┬────────────┘    │
                 │                 │
                 v                 │
    ┌─────────────────────────┐    │
  5 │        assemble         │    │
    └────────────┬────────────┘    │
                 │                 │
                 v                 │
    ┌─────────────────────────┐    │
  6 │     assign-continue     │    │
    └────────────┬────────────┘    │
                 │                 │
                 v                 │
    ┌─────────────────────────┐    │
  7 │         execute         │    │
    └────────────┬────────────┘    │
                 │                 │
                 v                 │
    ┌─────────────────────────┐    │
  8 │     announce-output     │    │
    └────────────┬────────────┘    │
                 │                 │
                 v                 │
    ┌─────────────────────────┐    │
  9 │       user-print        │    │
    └────────────┬────────────┘    │
                 │                 │
                 └─────────────────┘


Demo
====

;;; RCEP-RM input:
(define (factorial n)
        (if (= n 1)
            1
            (* (factorial (- n 1)) n)))

;;; RCEP-RM value:
ok

;;; RCEP-RM input:
(factorial 5)

;;; RCEP-RM value:
120

;;; RCEP-RM input:


Run
===")

(#%require "machine-48.scm")
(#%require "compiler-48.scm")
(#%require "ec-evaluator-48.scm")

(define rcep-controller
  '((assign env (op get-global-environment))
    read
    (perform (op prompt-for-input) (const ";;; RCEP-RM input:"))
    (assign val (op read))
    (assign val (op compile) (reg val))
    (assemble-val)
    (assign continue (label after-execute))
    (goto (reg val))
    after-execute
    (perform (op announce-output) (const ";;; RCEP-RM value:"))
    (perform (op user-print) (reg val))
    (goto (label read))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (define (compound-procedure? p)
    (define (tagged-list? exp tag)
      (if (pair? exp)
          (eq? (car exp) tag)
          false))
    (tagged-list? p 'procedure))
  (define (procedure-parameters p) (cadr  p))
  (define (procedure-body p) (caddr p))
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define operations
  (append eceval-operations
          (list (list 'read read)
                (list 'compile statements-with-return)
                (list 'prompt-for-input prompt-for-input)
                (list 'announce-output announce-output)
                (list 'user-print user-print))))

(start (make-machine operations rcep-controller))

(--end-- "5.49")

