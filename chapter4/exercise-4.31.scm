#lang sicp

(#%require "common.scm")

;   Exercise 4.31
;   =============
;   
;   The approach taken in this section is somewhat unpleasant, because it
;   makes an incompatible change to Scheme.  It might be nicer to implement
;   lazy evaluation as an upward-compatible extension, that is, so that
;   ordinary Scheme programs will work as before.  We can do this by
;   extending the syntax of procedure declarations to let the user control
;   whether or not arguments are to be delayed.  While we're at it, we may
;   as well also give the user the choice between delaying with and without
;   memoization.  For example, the definition
;   
;   (define (f a (b lazy) c (d lazy-memo))
;     ...)
;   
;   would define f to be a procedure of four arguments, where the first and
;   third arguments are evaluated when the procedure is called, the second
;   argument is delayed, and the fourth argument is both delayed and
;   memoized.  Thus, ordinary procedure definitions will produce the same
;   behavior as ordinary Scheme, while adding the lazy-memo declaration to
;   each parameter of every compound procedure will produce the behavior of
;   the lazy evaluator defined in this section. Design and implement the
;   changes required to produce such an extension to Scheme.  You will have
;   to implement new syntax procedures to handle the new syntax for define. 
;   You must also arrange for eval or apply to determine when arguments are
;   to be delayed, and to force or delay arguments accordingly, and you must
;   arrange for forcing to memoize or not, as appropriate.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.31]: http://sicp-book.com/book-Z-H-27.html#%_thm_4.31
;   4.2.2 An Interpreter with Lazy Evaluation - p408
;   ------------------------------------------------------------------------

(-start- "4.31")

(println "
Given an expression like (define (f a (b lazy) c (d lazy-memo)) I decided to
call (b lazy) a 'parameter definition'  where b is the parameter and lazy is
the 'parameter style'.  If no style (i.e., lazy / lazy-memo) is defined then
the styly is considered to be 'default. So the new syntax handlers are:

    (define (param-def-parameter param-def)
      (if (symbol? param-def)
          param-def
          (car param-def)))

    (define (param-def-style param-def)
      (if (symbol? param-def)
          'default
          (cadr param-def)))

    (define (param-defs->params parameter-defs)
      (map param-def-parameter parameter-defs))

    (define first-parameter-def car)
    (define rest-parameter-defs cdr)

And procedure-parameters is renamed procedure-parameter-defs

    (define (procedure-parameter-defs p) (cadr  p))

I renamed the existing delay-it to delay-memo-it, and created a new delay-it
that delays without memoizing.  Delay only expressions are tagged with
'thunk, while delay-memo expressions are tagged with 'thunk-memo

    (define (delay-memo-it exp env)
      (list 'thunk-memo exp env))

    (define (thunk-memo? obj)
      (tagged-list? obj 'thunk-memo))

    (define (delay-it exp env)
      (list 'thunk exp env))

    (define (thunk? obj)
      (tagged-list? obj 'thunk))

This procedure gets an argument from an expression appropriate to the style:

    (define (exp->arg parameter-def exp env)
      (let ((param-style (param-def-style parameter-def))) 
        (cond ((equal? param-style 'default)
               (actual-value exp env))
              ((equal? param-style 'lazy-memo)
               (delay-memo-it exp env))
              ((equal? param-style 'lazy)
               (delay-it exp env))
              (else error \"Unknown parameter style:\" param-style))))

This is then called by list-of-delayed-args (which now only delays args that
are explicitly marked as lazy). 

    (define (list-of-delayed-args parameter-defs exps env)
      (cond ((no-operands? exps) '())
            (else
             (if (no-parameter-defs? parameter-defs)
                 (error \"Too few parameter-defs\"))
             (cons
              (exp->arg (first-parameter-def parameter-defs)
                        (first-operand exps)
                        env)
              (list-of-delayed-args
               (rest-parameter-defs parameter-defs)
               (rest-operands exps)
               env)))))

As the above requires paramater definitions, apply is updated to pass them:

    (define (apply procedure arguments env)
      (cond ((primitive-procedure? procedure)
             (apply-primitive-procedure
              procedure
              (list-of-arg-values arguments env)))
            ((compound-procedure? procedure)
             (let ((parameter-defs (procedure-parameter-defs procedure)))
               (eval-sequence
                (procedure-body procedure)
                (extend-environment
                 (param-defs->params parameter-defs)
                 (list-of-delayed-args parameter-defs arguments  env)
                 (procedure-environment procedure)))))
            (else
             (error
              \"Unknown procedure type -- APPLY\" procedure))))

Demo Code
=========")

(#%require "ea-data-directed-31.scm")
(put-evaluators)

(define program
  '(begin

     (define (get-greeting)
       (println "    " "Getting greeting...")
       "Hello, World!")

     (define (greet3 greeting)
       (println "    " "< About to Greet >")
       (println "    " greeting)
       (println "    " greeting)
       (println "    " greeting))

     (define (greet3-lazy (greeting lazy))
       (println "    " "< About to Greet >")
       (println "    " greeting)
       (println "    " greeting)
       (println "    " greeting))

     (define (greet3-lazy-memo (greeting lazy-memo))
       (println "    " "< About to Greet >")
       (println "    " greeting)
       (println "    " greeting)
       (println "    " greeting))

     (println "
Using strict argument:")
     (greet3 (get-greeting))

     (println "
Using lazy-memo argument:")
     (greet3-lazy-memo (get-greeting))

     (println "
Using lazy argument:")     
     (greet3-lazy (get-greeting))
      
     ))

(eval program the-global-environment)


(--end-- "4.31")

