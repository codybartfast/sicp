#lang sicp

(#%require "common.scm")

;   Exercise 5.32
;   =============
;   
;   Using the preserving mechanism, the compiler will avoid saving and
;   restoring env around the evaluation of the operator of a combination in
;   the case where the operator is a symbol.  We could also build such
;   optimizations into the evaluator.  Indeed, the explicit-control
;   evaluator of section [5.4] already performs a similar optimization, by
;   treating combinations with no operands as a special case.
;   
;   a. Extend the explicit-control evaluator to recognize as a separate
;   class of expressions combinations whose operator is a symbol, and to
;   take advantage of this fact in evaluating such expressions.
;   
;   b. Alyssa P. Hacker suggests that by extending the evaluator to
;   recognize more and more special cases we could incorporate all the
;   compiler's optimizations, and that this would eliminate the advantage of
;   compilation altogether.  What do you think of this idea?
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.32]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.32
;   [Section 5.4]:   http://sicp-book.com/book-Z-H-34.html#%_sec_5.4
;   5.5.1 Structure of the Compiler - p574
;   ------------------------------------------------------------------------

(-start- "5.32")

(println
 "
Part A
======

  ev-application
    (save continue)
    (assign unev (op operands) (reg exp))
    (assign exp (op operator) (reg exp))
    (test (op variable?) (reg exp))           ; check if a variable
    (branch (label ev-appl-operator-lookup))  ;   --> to lookup
    (save env)                                ; do need eval
    (save unev)                               ; so do need to save
    (assign continue (label ev-appl-did-operator-eval))
    (goto (label eval-dispatch))

  ev-appl-operator-lookup                     ; peform lookup
    (assign continue (label ev-appl-did-operator-lookup))
    (goto (label ev-variable))

  ev-appl-did-operator-eval                   ; return here if we eval'ed
    (restore unev)
    (restore env)
  ev-appl-did-operator-lookup                 ; return here if we looked up
    (assign argl (op empty-arglist))
    (assign proc (reg val))
    ...

Demo
====
")

(#%require "machine-19.scm")
(#%require "ec-evaluator-32.scm")

(define (run prog)
  (define (printReg reg before after)
    (println "--reg--: " reg ": " before " --> " after))
  (let ((eceval
         (make-machine
          eceval-operations
          explicit-control-evaluator)))

    (set-register-contents! eceval 'exp prog)
    (set-register-contents! eceval 'env (the-global-environment))
    ;(trace-on! eceval println)
    (ignore (start eceval))))

(define prog1
  '(begin
     (* 3 ((lambda (a b) (+ a b)) 1 3))
     ))

(run prog1)

(println
 "
Part B
======

Clearly there are some optimizations, like the Part A, which are are
relatively cheap to recognize and simple to implement.  However, there are a
few reasons why this will  be limited:

  1. Cost of detection.  Consider the optimizations in the previous exercise
     where we can avoid saving a register because none of the arguments need
     the environment, or we know none of the arguments will change it.  To
     implement this we would have to analyse all the arguments before
     deciding whether what registers need saving before revisitng the
     arguments to evaluate them.  The cost of detecting these optimizations
     would likely outweigh their benefit.

     The cost of identifying optimizations would be even greater for general
     sequences of instructions as described in this section.

     However, with compilation that cost is potentially only paid once for
     the lifetime of a package used multiple times by multiple users.

  2. Complexity of implementation.  Even the relatively basic optimizations
     described in this section could become extremely complicated to
     implement in our machine language, a compiled optimization could be
     developed more quickly and more reliably in a higher level language.

  3. Other resources.  Evaluator optimizations that improve speed may still
     have other costs for every user, such as requiring a more complicated,
     and larger interpreter and perhaps increased memory usage storing data
     used for optimization.

By contrast interpreter optimizers have extra information that a compiler
may not have, such as the performance characteristics of the computer
running the application, which resources are under pressure (cpu, memory,
...) and which parts of the code are used most frequently and would benefit
most from optimization.
")

(--end-- "5.32")

