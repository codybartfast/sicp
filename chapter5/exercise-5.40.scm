#lang sicp

(#%require "common.scm")

;   Exercise 5.40
;   =============
;   
;   Modify the compiler to maintain the compile-time environment as
;   described above.  That is, add a compile-time-environment argument to
;   compile and the various code generators, and extend it in
;   compile-lambda-body.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.40]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.40
;   5.5.6 Lexical Addressing - p602
;   ------------------------------------------------------------------------

(-start- "5.40")

(println
 "
Added 'ctenv', (compile time environment) as an argument to compile
procedures that take an expression.  E.g:

                         v
  (define (compile exp ctenv target linkage)
    (cond ((self-evaluating? exp)         v
           (compile-self-evaluating exp ctenv target linkage))
          ((quoted? exp) (compile-quoted exp ctenv target linkage))
          ((variable? exp)                     ^
           (compile-variable exp ctenv target linkage))
          ((assignment? exp)       ^
           (compile-assignment exp ctenv target linkage))
                                     ^

An empty compile time environment to pass to intitial compile

  (define empty-ctenv '())

A procedure to extend the ctenv with variable names

  (define (extend-ctenv ctenv vars)
    (cons vars ctenv))

Compile-lambda-body is updated to make a call to extend-ctenv

  (define (compile-lambda-body exp ctenv proc-entry)
    (let* ((formals (lambda-parameters exp))
           (ctenv (extend-ctenv ctenv formals)))      <---
      (append-instruction-sequences
")
(--end-- "5.40")

