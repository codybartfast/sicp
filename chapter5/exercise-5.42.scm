#lang sicp

(#%require "common.scm")

;   Exercise 5.42
;   =============
;   
;   Using find-variable from exercise [5.41], rewrite compile-variable and
;   compile-assignment to output lexical-address instructions.  In cases
;   where find-variable returns not-found (that is, where the variable is
;   not in the compile-time environment), you should have the code
;   generators use the evaluator operations, as before, to search for the
;   binding. (The only place a variable that is not found at compile time
;   can be is in the global environment, which is part of the run-time
;   environment but is not part of the compile-time environment.⁽⁴⁷⁾ Thus,
;   if you wish, you may have the evaluator operations look directly in the
;   global environment, which can be obtained with the operation (op
;   get-global-environment), instead of having them search the whole
;   run-time environment found in env.) Test the modified compiler on a few
;   simple cases, such as the nested lambda combination at the beginning of
;   this section.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.42]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.42
;   [Exercise 5.41]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.41
;   [Footnote 47]:   http://sicp-book.com/book-Z-H-35.html#footnote_Temp_830
;   5.5.6 Lexical Addressing - p602
;   ------------------------------------------------------------------------

(-start- "5.42")

(#%require "compiler-39.scm")

(define expression
  '(((lambda (x y)
       (lambda (a b c d e)
         ((lambda (y z)
            'expression-1
            (set! e (+ c x w)))
          'expression-2
          )))
     3
     4)))

(println
 "
(define (compile-variable exp ctenv target linkage)
  (let ((lex-addr (find-variable exp ctenv)))
    (let ((lookup-code
           (if (eq? lex-addr 'not-found)
               ;; put global-env into target to preserve env
               `((assign ,target
                         (op get-global-environment))
                 (assign ,target
                         (op lookup-variable-value)
                         (const ,exp)
                         (reg ,target)))
               `((assign ,target
                         (op lookup-lex-addr)
                         (const ,lex-addr)
                         (reg env))))))
      (end-with-linkage
       linkage
       (make-instruction-sequence
        '(env) (list target)
        lookup-code)))))

(define (compile-assignment exp ctenv target linkage)
  (display exp)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) ctenv 'val 'next)))
    (let ((lex-addr (find-variable var ctenv)))
      (let ((assignment-seq
             (if (eq? lex-addr 'not-found)
                 (make-instruction-sequence
                  '(env val)
                  (list 'env target)  ;; we're modifying env
                  ;; target could be val so can't put global-env there
                  `((assign env (op get-global-environment))
                    (perform (op set-variable-value!)
                             (const ,var)
                             (reg val)
                             (reg env))
                    (assign ,target (const ok))))
                 (make-instruction-sequence
                  '(env val)
                  (list target)
                  `((perform (op set-lex-addr!)
                             (const ,lex-addr)
                             (reg val)
                             (reg env))
                    (assign ,target (const ok)))))))
        (end-with-linkage
         linkage
         (preserving
          '(env)
          get-value-code
          assignment-seq))))))

Using the a variation on the suggested lambda:

" expression "

We (at least) get some of the statements we would expect:

  (assign arg1 (op lookup-lex-addr) (const (1 2)) (reg env))
  (assign arg2 (op lookup-lex-addr) (const (2 0)) (reg env))
  (assign arg1 (op +) (reg arg1) (reg arg2))
  (assign arg2 (op get-global-environment))
  (assign arg2 (op lookup-variable-value) (const w) (reg arg2))
  (assign val (op +) (reg arg1) (reg arg2))
  (perform (op set-lex-addr!) (const (1 4)) (reg val) (reg env))

Result of compilation:
======================
")

(compile
 expression
 empty-ctenv
 'val
 'next)

(--end-- "5.42")

