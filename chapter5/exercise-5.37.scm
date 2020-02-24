#lang sicp

(#%require "common.scm")

;   Exercise 5.37
;   =============
;   
;   One way to understand the compiler's preserving mechanism for optimizing
;   stack usage is to see what extra operations would be generated if we did
;   not use this idea.  Modify preserving so that it always generates the
;   save and restore operations. Compile some simple expressions and
;   identify the unnecessary stack operations that are generated. Compare
;   the code to that generated with the preserving mechanism intact.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.37]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.37
;   5.5.5 An Example of Compiled Code - p595
;   ------------------------------------------------------------------------

(-start- "5.36")

(define expression
  '(+ 3 2))

(println
 "
To make preserve put in unnecessary save and restores we can just replace
its if statement with the consequent branch:

  (define (preserving regs seq1 seq2)
    (let ((regs regs))
      (if (null? regs)
          (append-instruction-sequences seq1 seq2)
          (let ((first-reg (car regs)))
            (preserving (cdr regs)
                        (make-instruction-sequence
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (append `((save ,first-reg))
                                 (statements seq1)
                                 `((restore ,first-reg))))
                        seq2)))))

A simple expression like " expression "doesn't need any save and restores so
usually it would only generate 14 instructions:

  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 3))
  (assign argl (op list) (reg val))
  (assign val (const 2))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1))
  compiled-branch2
  (assign continue (label after-call3))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call

However with the updated preserve we get additional 34 instructions, i.e,
there are 20 unnecesary saver or restore statements:

  (save continue)
  (save env)
  (save continue)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (restore continue)
  (restore env)
  (restore continue)
  (save continue)
  (save proc)
  (save env)
  (save continue)
  (assign val (const 2))
  (restore continue)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (save continue)
  (assign val (const 3))
  (restore continue)
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1))
  compiled-branch2
  (assign continue (label after-call3))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch1
  (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  after-call3

Demo:
")

(#%require "compiler-37.scm")

(compile
 expression
 'val
 'next)

(--end-- "5.37")

