#lang sicp

(#%require "common.scm")

;   Exercise 5.38
;   =============
;   
;   Our compiler is clever about avoiding unnecessary stack operations, but
;   it is not clever at all when it comes to compiling calls to the
;   primitive procedures of the language in terms of the primitive
;   operations supplied by the machine.  For example, consider how much code
;   is compiled to compute (+ a 1): The code sets up an argument list in
;   argl, puts the primitive addition procedure (which it finds by looking
;   up the symbol + in the environment) into proc, and tests whether the
;   procedure is primitive or compound.  The compiler always generates code
;   to perform the test, as well as code for primitive and compound branches
;   (only one of which will be executed). We have not shown the part of the
;   controller that implements primitives, but we presume that these
;   instructions make use of primitive arithmetic operations in the
;   machine's data paths.  Consider how much less code would be generated if
;   the compiler could open-code primitives -- that is, if it could generate
;   code to directly use these primitive machine operations.  The expression
;   (+ a 1) might be compiled into something as simple as ⁽⁴³⁾
;   
;   (assign val (op lookup-variable-value) (const a) (reg env))
;   (assign val (op +) (reg val) (const 1))
;   
;   In this exercise we will extend our compiler to support open coding of
;   selected primitives.  Special-purpose code will be generated for calls
;   to these primitive procedures instead of the general
;   procedure-application code.  In order to support this, we will augment
;   our machine with special argument registers arg1 and arg2. The primitive
;   arithmetic operations of the machine will take their inputs from arg1
;   and arg2. The results may be put into val, arg1, or arg2.
;   
;   The compiler must be able to recognize the application of an open-coded
;   primitive in the source program.  We will augment the dispatch in the
;   compile procedure to recognize the names of these primitives in addition
;   to the reserved words (the special forms) it currently recognizes.⁽⁴⁴⁾
;   For each special form our compiler has a code generator.  In this
;   exercise we will construct a family of code generators for the
;   open-coded primitives.
;   
;   a.  The open-coded primitives, unlike the special forms, all need their
;   operands evaluated.  Write a code generator spread-arguments for use by
;   all the open-coding code generators.  Spread-arguments should take an
;   operand list and compile the given operands targeted to successive
;   argument registers.  Note that an operand may contain a call to an
;   open-coded primitive, so argument registers will have to be preserved
;   during operand evaluation.
;   
;   b.  For each of the primitive procedures =, *, -, and +, write a code
;   generator that takes a combination with that operator, together with a
;   target and a linkage descriptor, and produces code to spread the
;   arguments into the registers and then perform the operation targeted to
;   the given target with the given linkage.  You need only handle
;   expressions with two operands.  Make compile dispatch to these code
;   generators.
;   
;   c.  Try your new compiler on the factorial example.  Compare the
;   resulting code with the result produced without open coding.
;   
;   d.  Extend your code generators for + and * so that they can handle
;   expressions with arbitrary numbers of operands.  An expression with more
;   than two operands will have to be compiled into a sequence of
;   operations, each with only two inputs.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.38]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.38
;   [Footnote 43]:   http://sicp-book.com/book-Z-H-35.html#footnote_Temp_822
;   [Footnote 44]:   http://sicp-book.com/book-Z-H-35.html#footnote_Temp_823
;   5.5.5 An Example of Compiled Code - p595
;   ------------------------------------------------------------------------

(-start- "5.38")

(println
 "
Part A
======

  (define (spread-arguments operands)
    (if (= 2 (length operands))
        (preserving '(env)
                    (compile (car operands) 'arg1 'next)
                    (preserving '(arg1)
                                (compile (cadr operands) 'arg2 'next)
                                (make-instruction-sequence
                                 '(arg1) '() '())))
        (error \"Spread-arguments expects 2 args -- COMPILE\" operands)))


Part B
======

  (define (compile exp target linkage)
    (cond ((self-evaluating? exp)
          ...
          ((primitive-procedure? exp)
           (compile-primitive-procedure exp target linkage))
          ...
          (else
           (error \"Unknown expression type -- COMPILE\" exp))))

  (define (compile-= exp target linkage)
    (compile-2arg-open-code '= (operands exp) target linkage))

  (define (compile-- exp target linkage)
    (compile-2arg-open-code '- (operands exp) target linkage))

  ...

  (define (compile-2arg-open-code operator operands target linkage)
    (end-with-linkage
     linkage
     (append-instruction-sequences
      (spread-arguments operands)
      (make-instruction-sequence
       '(arg1 arg2)
       `(,target)
       `((assign ,target (op ,operator) (reg arg1) (reg arg2)))))))

  (define primitive-procedure-compilers
    (list
     (cons '= compile-=)
     (cons '- compile--)
     ...))

  (define primitive-procedure-names
    (map car primitive-procedure-compilers))
  (define (primitive-procedure? exp)
    (and (pair? exp)
         (memq (car exp) primitive-procedure-names)))

  (define (lookup-primitive-compiler prim-proc)
    (lookup prim-proc primitive-procedure-compilers))

  (define (compile-primitive-procedure exp target linkage)
    ((lookup-primitive-compiler (car exp)) exp target linkage))


Part C
======

With these modifications there are half as many instructions in the lambda
body than before (58 before, 29 after).  We could therefore expect it to run
about twice as fast.


Part D
======

  (define (compile-* exp target linkage)
    (compile-multi-arg-open-code '* (operands exp) target linkage '1))

  (define (compile-+ exp target linkage)
    (compile-multi-arg-open-code '+ (operands exp) target linkage '0))

  (define (compile-multi-arg-open operator operands target linkage op-id)
    (let ((operand-count (length operands)))
      (cond
        ((= 0 operand-count) (compile op-id target linkage))
        ((= 1 operand-count) (compile (car operands) target linkage))
        (else
         (end-with-linkage
          linkage
          (preserving
           '(env)
           (compile (car operands) 'arg1 'next)
           (compile-open-code-reduce operator (cdr operands) target)))))))

  (define (compile-open-code-reduce operator operands target)
    (let* ((is-last-operand (null? (cdr operands)))
           (trgt (if is-last-operand target 'arg1))
           (open-code-apply
            (preserving '(arg1)
                        (compile (car operands) 'arg2 'next)
                        (make-instruction-sequence
                         '(arg1 arg2)
                         `(,trgt)
                         `((assign ,trgt (op ,operator)
                                   (reg arg1) (reg arg2)))))))
      (if is-last-operand
          open-code-apply
          (preserving
           '(env)
           open-code-apply
           (compile-open-code-reduce operator (cdr operands) target)))))


Simple Example
==============

  (compile
   '(+ 1 2 3 4 5)
   'val
   'next)

Output:
-------

  (()
   (arg1 arg2 val)
   ((assign arg1 (const 1))
    (assign arg2 (const 2))
    (assign arg1 (op +) (reg arg1) (reg arg2))
    (assign arg2 (const 3))
    (assign arg1 (op +) (reg arg1) (reg arg2))
    (assign arg2 (const 4))
    (assign arg1 (op +) (reg arg1) (reg arg2))
    (assign arg2 (const 5))
    (assign val (op +) (reg arg1) (reg arg2))))


More Complex Example
====================

  (compile
   '(* (*) (* 2) (values 3) (* 4 five))
   'val
   'return)

Output:
-------

  ((env continue)
   (proc argl arg1 arg2 val)
   ((save continue)
    (assign arg1 (const 1))
    (assign arg2 (const 2))
    (assign arg1 (op *) (reg arg1) (reg arg2))
    (save env)
    (assign proc (op lookup-variable-value) (const values) (reg env))
    (assign val (const 3))
    (assign argl (op list) (reg val))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch1))
    compiled-branch2
    (assign continue (label proc-return4))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    proc-return4
    (assign arg2 (reg val))
    (goto (label after-call3))
    primitive-branch1
    (assign arg2 (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call3
    (assign arg1 (op *) (reg arg1) (reg arg2))
    (restore env)
    (save arg1)
    (assign arg1 (const 4))
    (assign arg2 (op lookup-variable-value) (const five) (reg env))
    (assign arg2 (op *) (reg arg1) (reg arg2))
    (restore arg1)
    (assign val (op *) (reg arg1) (reg arg2))
    (restore continue)
    (goto (reg continue))))

Demo
====
")

(#%require "compiler-38.scm")

;(compile
; '(define (factorial n)
;    (if (= n 1)
;        1
;        (* (factorial (- n 1)) n)))
; 'val
; 'next)

(compile
 '(+ 1 2 3 4 5)
 'val
 'next)

(println "")

(compile
 '(* (*) (* 2) (values 3) (* 4 five))
 'val
 'return)

(println "")

(--end-- "5.38")

