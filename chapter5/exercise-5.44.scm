#lang sicp

(#%require "common.scm")

;   Exercise 5.44
;   =============
;   
;   In this section we have focused on the use of the compile-time
;   environment to produce lexical addresses.  But there are other uses for
;   compile-time environments.  For instance, in exercise [5.38] we
;   increased the efficiency of compiled code by open-coding primitive
;   procedures.  Our implementation treated the names of open-coded
;   procedures as reserved words.  If a program were to rebind such a name,
;   the mechanism described in exercise [5.38] would still open-code it as a
;   primitive, ignoring the new binding.  For example, consider the
;   procedure
;   
;   (lambda (+ * a b x y)
;     (+ (* a x) (* b y)))
;   
;   which computes a linear combination of x and y.  We might call it with
;   arguments +matrix, *matrix, and four matrices, but the open-coding
;   compiler would still open-code the + and the * in (+ (* a x) (* b y)) as
;   primitive + and *.  Modify the open-coding compiler to consult the
;   compile-time environment in order to compile the correct code for
;   expressions involving the names of primitive procedures. (The code will
;   work correctly as long as the program does not define or set! these
;   names.)
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.44]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.44
;   [Exercise 5.38]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.38
;   5.5.6 Lexical Addressing - p603
;   ------------------------------------------------------------------------

(-start- "5.44")

(define expression
  '(((lambda (+ a b)
       (+ a b))
     - 10 3)))

(println
 "
If we compile the expression:

  " expression  "

then the application of + compiles to:

  (assign arg1 (op lookup-lex-addr) (const (0 1)) (reg env))
  (assign arg2 (op lookup-lex-addr) (const (0 2)) (reg env))
  (assign val (op +) (reg arg1) (reg arg2))

Which would produce 13 instead of the expected 7.

So instead of primitive-procedure? we have primitive-name? which checks if a
symbol is a variable or the 'raw' name of a primitive.  (The variable could
still resolve to a primitive procedure so stictly speaking were not checking
of the procedure is primitive):

  (define (primitive-name? exp ctenv)
    (and (pair? exp)
         (memq (car exp) primitive-procedure-names)
         (eq? 'not-found (find-variable (car exp) ctenv))))

This then produces the original, verbose code:

  (assign proc (op lookup-lex-addr) (const (0 0)) (reg env))
  (assign val (op lookup-lex-addr) (const (0 2)) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-lex-addr) (const (0 1)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch3))
  compiled-branch4
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch3
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))

The question says \"The code will work correctly as long as the program does
not define or set! these names.\".  This is because with the above solution
neither:

  (define + -)
  (+ 10 3)

nor

  (set! + -)
  (+ 10 3)

will compile correctly.  Because a top level define + would be in the global
environment which is not available at compile time, and set! will fail
because there is no + in the environment that can be modified.

An internal definition would work if scan-out-defines:

  (define (seven)
    (define + -)
    (+ 10 3))

because the internal define would be transformed into our original lambda
expression.

Compile lambda:
===============
")

(#%require "compiler-44.scm")

(define exp0
  '(define (seven)
     (define + -)
     (+ 10 3)))

(define exp1
  '(begin
     (define + -)
     (+ 10 3)))

(define exp2
  '(define (seven)
     (set! + -)
     (+ 10 3)))

(compile
 expression
 empty-ctenv
 'val
 'next)

(--end-- "5.44")

