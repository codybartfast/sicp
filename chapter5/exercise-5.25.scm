#lang sicp

(#%require "common.scm")

;   Exercise 5.25
;   =============
;   
;   Modify the evaluator so that it uses normal-order evaluation, based on
;   the lazy evaluator of section [4.2].
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.25]: http://sicp-book.com/book-Z-H-34.html#%_thm_5.25
;   [Section 4.2]:   http://sicp-book.com/book-Z-H-27.html#%_sec_4.2
;   5.4.3 Conditionals, Assignments, and Definitions - p560
;   ------------------------------------------------------------------------

(-start- "5.25")

(println
 "
The modifications essentially match those made to the metaciruclar evaluator
in Section 4.2:

New Primitive Operations:
=========================

   (list 'delay-it (lambda (exp env) (list 'thunk exp env)))
   (list 'thunk? (lambda (exp) (tagged-list? exp 'thunk)))
   (list 'thunk-exp cadr)
   (list 'thunk-env caddr)
   (list 'set-evaluated-thunk!
         (lambda (exp result)
           (set-car! exp 'evaluated-thunk)
           (set-car! (cdr exp) result)
           (set-cdr! (cdr exp) '())))
   (list 'evaluated-thunk?
         (lambda (exp) (tagged-list? exp 'evaluated-thunk)))
   (list 'thunk-value cadr)


New EC-Evaluator Routines:
==========================

  actual-value
    (save continue)
    (assign continue (label force-it))
    (goto (label eval-dispatch))

  force-it
    (test (op thunk?) (reg val))
    (branch (label force-it-thunk))
    (test (op evaluated-thunk?) (reg val))
    (branch (label force-it-evaluated-thunk))
    (restore continue)
    (goto (reg continue))

  force-it-thunk
    (save val)
    (assign exp (op thunk-exp) (reg val))
    (assign env (op thunk-env) (reg val))
    (assign continue (label force-it-result))
    (goto (label actual-value))

  force-it-result
    (restore exp)                                 ; originally val
    (restore continue)
    (perform (op set-evaluated-thunk!) (reg exp) (reg val))
    (goto (reg continue))

  force-it-evaluated-thunk
    (assign val (op thunk-value) (reg val))
    (restore continue)
    (goto (reg continue))


Modified Predicate:
===================

  ev-if
    (save exp)
    (save env)
    (save continue)
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label actual-value))  ; <-- actual-value instead of eval-dispatch


Overview of Changes to ev-application:
======================================

In section 4.2 we have two separate procedures for evaluating arguments,
list-of-arg-values and list-of-delayed-args depending on whether the
precedure is primitive or not.  This is recreated by duplicating the middle
section of the ev-application routines. Specifically,

    ev-appl-operand-loop
    ev-appl-accumulate-arg
    ev-appl-last-arg
    ev-appl-accum-last-arg

are replaced with:

    ev-appl-operand-loop-value
    ev-appl-accumulate-arg-value
    ev-appl-last-arg-value
    ev-appl-accum-last-arg-value

and

    ev-appl-operand-loop-delayed
    ev-appl-accumulate-arg-delayed
    ev-appl-last-arg-delayed
    ev-appl-accum-last-arg-delayed

we then goto ev-appl-operand-loop-value or ev-appl-operand-loop-delayed
depending on whether the operantor is a primitive procedure.

Within these procedures the references to eval-dispatch are replaced
respectively with actual-value and delay-it, e.g.:

  ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))                       ; <-- eval-dispatch

is replaced with:

  ev-appl-last-arg-value
    (assign continue (label ev-appl-accum-last-arg-value))
    (goto (label actual-value))                        ; <-- actual-value

and

  ev-appl-last-arg-delayed
    (assign val (op delay-it) (reg exp) (reg env))     ; <-- delay-it

This can then be tested with the example from Section 4.2:

    (define (try a b)
      (if (= a 0) 'Yes-Lazy! b))

    (try 0 (/ 1 0))

Output:

    eceval DONE - val: Yes-Lazy!

")

(#%require "machine-19.scm")
(#%require "ec-evaluator-25.scm")

(define prog
  '(begin
     (define (try a b)
       (if (= a 0) 'Yes-Lazy! b))
     (try 0 (/ 1 0))))

(define (run prog)
  (define (printReg reg before after)
    (println "--reg--: " reg ": " before " --> " after))
  (let ((eceval
         (make-machine
          eceval-operations
          explicit-control-evaluator)))

    (set-register-contents! eceval 'exp prog)
    (set-register-contents! eceval 'env the-global-environment)
;    (trace-on! eceval println)
;    (reg-trace-on! eceval 'exp printReg)
;    (reg-trace-on! eceval 'proc printReg)
;    (reg-trace-on! eceval 'argl printReg)
;    (reg-trace-on! eceval 'env printReg)
;    (reg-trace-on! eceval 'val printReg)
;    (reg-trace-on! eceval 'unev printReg)
    (ignore (start eceval))))

(run prog)
(--end-- "5.25")

