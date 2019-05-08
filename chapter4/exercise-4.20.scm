#lang sicp

(#%require "common.scm")

;   Exercise 4.20
;   =============
;   
;   Because internal definitions look sequential but are actually
;   simultaneous, some people prefer to avoid them entirely, and use the
;   special form letrec instead.  Letrec looks like let, so it is not
;   surprising that the variables it binds are bound simultaneously and have
;   the same scope as each other.  The sample procedure f above can be
;   written without internal definitions, but with exactly the same meaning,
;   as
;   
;   (define (f x)
;     (letrec ((even?
;               (lambda (n)
;                 (if (= n 0)
;                     true
;                     (odd? (- n 1)))))
;              (odd?
;               (lambda (n)
;                 (if (= n 0)
;                     false
;                     (even? (- n 1))))))
;       <rest of body of f>))
;   
;   Letrec expressions, which have the form
;   
;   (letrec ((<var₁> <exp₁>) ... (<var_(n)> <exp_(n)>))
;     <body>)
;   
;   are a variation on let in which the expressions <exp_(k)> that provide
;   the initial values for the variables <var_(k)> are evaluated in an
;   environment that includes all the letrec bindings.  This permits
;   recursion in the bindings, such as the mutual recursion of even? and
;   odd? in the example above, or the evaluation of 10 factorial with
;   
;   (letrec ((fact
;             (lambda (n)
;               (if (= n 1)
;                   1
;                   (* n (fact (- n 1)))))))
;     (fact 10))
;   
;   a. Implement letrec as a derived expression, by transforming a letrec
;   expression into a let expression as shown in the text above or in
;   exercise [4.18]. That is, the letrec variables should be created with a
;   let and then be assigned their values with set!.
;   
;   b. Louis Reasoner is confused by all this fuss about internal
;   definitions.  The way he sees it, if you don't like to use define inside
;   a procedure, you can just use let.  Illustrate what is loose about his
;   reasoning by drawing an environment diagram that shows the environment
;   in which the <rest of body of f> is evaluated during evaluation of the
;   expression (f 5), with f defined as in this exercise.  Draw an
;   environment diagram for the same evaluation, but with let in place of
;   letrec in the definition of f.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.20]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.20
;   [Exercise 4.18]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.18
;   4.1.6 Internal Definitions - p391
;   ------------------------------------------------------------------------

(-start- "4.20")

(#%require "ea-data-directed-19.scm")
(put-evaluators)


;; Part A
;; ======

;; letrec implementation
(define (letrec->let exp)
  (make-let
   (map (lambda (pair) (list (let-pair-id pair) '*unassigned*))
        (let-pairs exp))
   (append (map (lambda (pair)
                  (list 'set! (let-pair-id pair) (let-pair-value pair)))
                (let-pairs exp))
           (let-body exp))))

(define (eval-letrec exp env)
  (eval (letrec->let exp) env))
(put 'eval 'letrec eval-letrec)


;; program using letrec
(define letrec-prog
  '(begin

     (define (f x)
       (letrec ((even?
                 (lambda (n)
                   (if (equal? n 0)
                       true
                       (odd? (- n 1)))))
                (odd?
                 (lambda (n)
                   (if (equal? n 0)
                       false
                       (even? (- n 1))))))
         (even? x)))

     (f 5)))

;; run the programe
(println "
Evaling (f 5), expect #f, got: "
         (eval letrec-prog the-global-environment) )

;; the let expression the program
(define let???-exp
  '(let??? ((even?
             (lambda (n)
               (if (equal? n 0)
                   true
                   (odd? (- n 1)))))
            (odd?
             (lambda (n)
               (if (equal? n 0)
                   false
                   (even? (- n 1))))))
           (even? x)))


;(println (let->combination (letrec->let let???-exp)))
;(println (let->combination let???-exp))

(println "
letrec expression gets replaced with:
-------------------------------------

    ((lambda (even? odd?)
       (set! even? (lambda (n) (if (equal? n 0) true (odd? (- n 1)))))
       (set! odd? (lambda (n) (if (equal? n 0) false (even? (- n 1)))))
       (even? x))
     *unassigned* *unassigned*)

compared with a regular let:
----------------------------

    ((lambda (even? odd?)
       (even? x))
     (lambda (n) (if (equal? n 0) true (odd? (- n 1))))
     (lambda (n) (if (equal? n 0) false (even? (- n 1)))))

Letrec Environment Diagram
==========================
Even? and odd? procs reference E2 because the are created when evaluating
set! within the body of the lambda.  This means they can lookup the even?
and odd? variables defined in this frame.

global env ──┐
             v
┌───────────────────────────┐
│                           │
│(after call to define)     │
│f:┐                        │<─────────────────────────────┐
└───────────────────────────┘                              │
   │  ^                                                    │
   │  │                                  call to f         │
   v  │                          ┌─────────────────────────┴─┐
  @ @ │                          │x: 5                       │
  │ └─┘                     E1 ->│                           │
  v                              │                           │<───┐
parameter: x                     └───────────────────────────┘    │
((lambda (even? odd?)                                             │
   (set! even? (lambda (n) ...)                                   │
   (set! odd? (lambda (n) ...)             call to letrec/lambda  │
   (even? x))                           ┌─────────────────────────┴─┐
 *unassigned* *unassigned*)             │even?:─────────────────┐   │
                                   E2 ->│odd?:┐                 │   │
                                        │     │                 │   │
                                        └───────────────────────────┘
                                              │  ^              │  ^
                                              │  │              │  │
                                              v  │              v  │
                                             @ @ │             @ @ │
                                             │ └─┘             │ └─┘
                                             v                 v
                                        parameter: n      parameter: n
                                      (if (equal? n 0)  (if (equal? n 0)
                                          false             true
                                          ...               ...

Let Environment Diagram
=======================
Even? and odd? procs reference E1 because they are evaluated in the body of
f but outside the 'let lambda' because they are passed as arguments to that
lambda.  This means they can't lookup the even? and odd? variables defined
in E2.

global env ──┐
             v
┌───────────────────────────┐
│                           │
│(after call to define)     │
│f:┐                        │<─────────────────────────────┐
└───────────────────────────┘                              │
   │  ^                                                    │
   │  │                                  call to f         │
   v  │                          ┌─────────────────────────┴─┐
  @ @ │                          │x: 5                       │<───────────┐
  │ └─┘                     E1 ->│                           │<─────────┐ │
  v                              │                           │<───┐     │ │
parameter: x                     └───────────────────────────┘    │     │ │
((lambda (even? odd?)                                             │     │ │
   (even? x))                                                     │     │ │
 (lambda (n) (if (equal? n ...))           call to let/lambda     │     │ │
 (lambda (n) (if (equal? n ...)))       ┌─────────────────────────┴─┐   │ │
                                        │even?:─────────────────┐   │   │ │
                                   E2 ->│odd?:┐                 │   │   ^ │
                                        │     │                 │   │   │ │
                                        └───────────────────────────┘   │ │
                                              │                 │       │ │
                                              │  ┌──────────────────────┘ ^
                                              │  │              │         │
                                              v  │              v         │
                                             @ @ │             @ @        │
                                             │ └─┘             │ └────────┘
                                             v                 v
                                        parameter: n      parameter: n
                                      (if (equal? n 0)  (if (equal? n 0)
                                          false             true
                                          ...               ...
")
(--end-- "4.20")

