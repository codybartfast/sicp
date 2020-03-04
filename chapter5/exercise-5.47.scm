#lang sicp

(#%require "common.scm")

;   Exercise 5.47
;   =============
;   
;   This section described how to modify the explicit-control evaluator so
;   that interpreted code can call compiled procedures.  Show how to modify
;   the compiler so that compiled procedures can call not only primitive
;   procedures and compiled procedures, but interpreted procedures as well. 
;   This requires modifying compile-procedure-call to handle the case of
;   compound (interpreted) procedures. Be sure to handle all the same target
;   and linkage combinations as in compile-proc-appl.  To do the actual
;   procedure application, the code needs to jump to the evaluator's
;   compound-apply entry point. This label cannot be directly referenced in
;   object code (since the assembler requires that all labels referenced by
;   the code it is assembling be defined there), so we will add a register
;   called compapp to the evaluator machine to hold this entry point, and
;   add an instruction to initialize it:
;   
;     (assign compapp (label compound-apply))
;     (branch (label external-entry))      ; branches if flag is set
;   read-eval-print-loop
;     ...
;   
;   To test your code, start by defining a procedure f that calls a
;   procedure g.  Use compile-and-go to compile the definition of f and
;   start the evaluator.  Now, typing at the evaluator, define g and try to
;   call f.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.47]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.47
;   5.5.7 Interfacing Compiled Code to the Evaluator - p609
;   ------------------------------------------------------------------------

(-start- "5.47")

(define source
  '(define (f val)
       (g val val)))

(define commands
  `(begin
     (define (g a b)
       (* a b))
     (f 3)))

(println
 "
This can be done by creating a duplicate of compile-proc-appl (called
compile-compound-appl) and replacing the three occuruances of:

  (assign val (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))

with

  (save continue)
  (goto (reg compapp))

We need to save continue because of apply-dispatch's convetion that the
return address is placed on the stack.

Then we add this an extra test to goto the branch that calls this:

  (define (compile-procedure-call target linkage)
    (let ((primitive-branch (make-label 'primitive-branch))
          (compiled-branch (make-label 'compiled-branch))
          (compound-branch (make-label 'compound-branch))            ;*
          (after-call (make-label 'after-call)))
      (let ((compiled-linkage
             (if (eq? linkage 'next) after-call linkage)))
        (append-instruction-sequences
         (make-instruction-sequence
          '(proc) '() `((test (op primitive-procedure?) (reg proc))
                        (branch (label ,primitive-branch))
                        (test (op compound-procedure?) (reg proc))   ;*
                        (branch (label ,compound-branch))))          ;*
         (parallel-instruction-sequences
          (append-instruction-sequences
           compiled-branch
           (compile-proc-appl target compiled-linkage))
          (append-instruction-sequences                              ;*
           compound-branch                                           ;*
           (compile-compound-appl target compiled-linkage))          ;*
          (append-instruction-sequences
           primitive-branch
           (end-with-linkage
            linkage
            (make-instruction-sequence
             '(proc argl) (list target)
             `((assign ,target
                       (op apply-primitive-procedure)
                       (reg proc)
                       (reg argl)))))))
         after-call))))

This uses an updated parallel-instruction-sequences that takea an arbitrary
number of sequences:

  (define (parallel-instruction-sequences . seqs)
    (define (parallelize-2-seqs seq1 seq2)
      (make-instruction-sequence
       (list-union (registers-needed seq1)
                   (registers-needed seq2))
       (list-union (registers-modified seq1)
                   (registers-modified seq2))
       (append (statements seq1) (statements seq2))))
    (define (parallelize seqs)
      (if (null? seqs)
          (empty-instruction-sequence)
          (parallelize-2-seqs (car seqs)
                              (parallelize (cdr seqs)))))
    (parallelize seqs))


Demo:
=====

Compiled code:

  " source "

and commands:

  " commands "


Output:
=======
")

(#%require "machine-45.scm")
(#%require "compiler-47.scm")
(#%require "ec-evaluator-47.scm")

(define (print-reg reg before after)
  (println "REG: " reg "  " before " ---> " after))

(define (compile-and-go)
  (let* ((eceval
          (make-machine
           eceval-operations
           explicit-control-evaluator))
         (instructions
          (assemble-instructions
           (assemble (statements
                      (compile source empty-ctenv 'val 'return))
                     eceval))))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'exp commands)
    (set-register-contents! eceval 'flag true)
    ;(trace-on! eceval println)
    ;(reg-trace-on! eceval 'argl print-reg)
    ;(set-breakpoint eceval 'external-entry 1)
    (start eceval)))

(ignore (compile-and-go))

(println "")

(--end-- "5.47")

