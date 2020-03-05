#lang sicp

(#%require "common.scm")

;   Exercise 5.48
;   =============
;   
;   The compile-and-go interface implemented in this section is awkward,
;   since the compiler can be called only once (when the evaluator machine
;   is started).  Augment the compiler-interpreter interface by providing a
;   compile-and-run primitive that can be called from within the
;   explicit-control evaluator as follows:
;   
;   ;;; EC-Eval input:
;   (compile-and-run
;    '(define (factorial n)
;       (if (= n 1)
;           1
;           (* (factorial (- n 1)) n))))
;   ;;; EC-Eval value:
;   ok
;   ;;; EC-Eval input:
;   (factorial 5)
;   ;;; EC-Eval value:
;   120
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.48]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.48
;   5.5.7 Interfacing Compiled Code to the Evaluator - p609
;   ------------------------------------------------------------------------

(-start- "5.48")

(println
 "
I believe compile-and-run needs to be a special form becuase if it's 'just'
a procedure there's no way to direct execution to the val register.

The tricky bit is the need to assemble the compiled statements because this
requires access to the machine/eceaval object on which they will be run. The
machine is not available in either the compiler or the ec-evaluator.  It
could be 'injected' into one of these, but instead I added support for an a
ssemble instruction to the register machine's assembler.


Compiler
========

  (define (statements-with-return exp)
    (statements
      (compile exp empty-ctenv 'val 'return)))


Register Machine - Assembler
============================

To keep it simple the assemble instruction doesn't take any parameters, it
reads the statetments from the val register, and then writes the assembled
instructions back to val:

  (define (make-assemble-val inst machine labels operations pc)
    (lambda ()
      (let* ((statements (get-register-contents machine 'val))
             (instructions
              (assemble statements machine)))
        (set-register-contents! machine 'val instructions)
        (advance-pc pc))))

This is called from make-execution-procedure:

  (define (make-execution-procedure ...)
    (cond ((eq? (car inst) 'assign)
           (make-assign inst machine labels ops pc))
          ...
          ((eq? (car inst) 'assemble-val)                     ;*
           (make-assemble-val inst machine labels ops pc))    ;*
          (else (error \"Unknown instruction type -- ASSEMBLE\" inst))))


EC-Evaluator
============

The ec-evaluator compiles the expression to val, uses assemble-val to
replace the statements with the assembled instructions and then goes to
those instructions:

  compile-and-run
    (assign exp (op compile-and-run-exp) (reg exp))
    (assign val (op statements-with-return) (reg exp))
    (assemble-val)
    (goto (reg val))

This is called from eval-dispatch:

  eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))
    ...
    (test (op compile-and-run?) (reg exp))                    ;*
    (branch (label compile-and-run))                          ;*
    (test (op application?) (reg exp))
    (branch (label ev-application))
    (goto (label unknown-expression-type))

These use the following additional primitive operations:

  (list 'compile-and-run?
       (lambda (exp) (tagged-list? exp 'compile-and-run)))
  (list 'statements-with-return statements-with-return)
  (list 'compile-and-run-exp cadr)


Demo:
=====
")

(#%require "machine-48.scm")
(#%require "compiler-48.scm")
(#%require "ec-evaluator-48.scm")

(define commands
  '(begin
     (compile-and-run                        ;; compile multiple definitions
      (begin
        (define (factorial n)
          (if (= n 1)
              1
              (* (factorial (- n 1)) n)))
        (define five (- (factorial 3) 1))
        (define (times5 n) (* five n))
        ))
     (compile-and-run                        ;; another compile definition
      (define (times4 n) (* 4 n)))
     (define (times3 n) (* 3 n))             ;; evaluatad definition
     (times5 (times4 (times3 (factorial 2))));; all together
     ))

(let ((eceval
        (make-machine
         eceval-operations
         explicit-control-evaluator)))
  (set-register-contents! eceval 'exp commands)
  (set-register-contents! eceval 'flag false)
  (ignore (start eceval)))

(println "")

(--end-- "5.48")

