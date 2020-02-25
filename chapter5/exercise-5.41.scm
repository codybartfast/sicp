#lang sicp

(#%require "common.scm")

;   Exercise 5.41
;   =============
;   
;   Write a procedure find-variable that takes as arguments a variable and a
;   compile-time environment and returns the lexical address of the variable
;   with respect to that environment.  For example, in the program fragment
;   that is shown above, the compile-time environment during the compilation
;   of expression <e1> is ((y z) (a b c d e) (x y)).  Find-variable should
;   produce
;   
;   (find-variable 'c '((y z) (a b c d e) (x y)))
;   (1 2)
;   
;   (find-variable 'x '((y z) (a b c d e) (x y)))
;   (2 0)
;   
;   (find-variable 'w '((y z) (a b c d e) (x y)))
;   not-found
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.41]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.41
;   5.5.6 Lexical Addressing - p602
;   ------------------------------------------------------------------------

(-start- "5.41")

(println
 "
  (define (make-lex-addr frame-number displacement)
    (list frame-number displacement))

  (define (index-of item list)
    (define (iter l n)
      (if (pair? l)
          (if (eq? item (car l))
              n
              (iter (cdr l) (+ n 1)))
          #f))
    (iter list 0))

  (define (find-variable var ctenv)
    (define (iter env frame-number)
      (if (pair? env)
          (let ((vars (car env)))
            (cond ((index-of var vars)
                   => (lambda (displacement)
                        (make-lex-addr frame-number displacement)))
                  (else (iter (cdr env) (+ frame-number 1)))))
            'not-found))
    (iter ctenv 0))
")

(--end-- "5.41")

