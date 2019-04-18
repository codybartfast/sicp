#lang sicp

(#%require "common.scm")

;   Exercise 4.8
;   ============
;   
;   "Named let" is a variant of let that has the form
;   
;   (let <var> <bindings> <body>)
;   
;   The <bindings> and <body> are just as in ordinary let, except that <var>
;   is bound within <body> to a procedure whose body is <body> and whose
;   parameters are the variables in the <bindings>.  Thus, one can
;   repeatedly execute the <body> by invoking the procedure named <var>. 
;   For example, the iterative Fibonacci procedure (section [1.2.2]) can be
;   rewritten using named let as follows:
;   
;   (define (fib n)
;     (let fib-iter ((a 1)
;                    (b 0)
;                    (count n))
;       (if (= count 0)
;           b
;           (fib-iter (+ a b) a (- count 1)))))
;   
;   Modify let->combination of exercise [4.6] to also support named let.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.8]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.8
;   [Section 1.2.2]: http://sicp-book.com/book-Z-H-11.html#%_sec_1.2.2
;   [Exercise 4.6]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.6
;   4.1.2 Representing Expressions - p376
;   ------------------------------------------------------------------------

(-start- "4.8")

(#%require "ea-data-directed.scm")
(put-evaluators)

;; Helpers used by named-let evaluators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; constructors
(define make-call cons)
(define (make-definition name params body)
  (cons 'define
        (cons (cons name params)
              body)))

;; 'generic' properties shared between let and named let
(define let-name cadr)
(define (named-let? exp)
  (symbol? (let-name exp)))
(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))
(define (let-pairs exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))

;; unchanged pairs info
(define let-pair-id car)
(define let-pair-value cadr)
(define (let-params exp)
  (map let-pair-id
       (let-pairs exp)))
(define (let-values exp)
  (map let-pair-value
       (let-pairs exp)))

;; Named let evaluator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (let->combination exp )
  (if (named-let? exp)
       ;; If a named let then...
      (make-begin
       (list
        ;; ...create a proc with the given name
        (make-definition (let-name exp)
                         (let-params exp)
                         (let-body exp))
        ;; ...and then call it
        (make-call (let-name exp)
                   (let-values exp))))
      ;; Else do regular let lambda thing
      (make-call
       (make-lambda (let-params exp)
                    (let-body exp))
       (let-values exp))))

(define (eval-let exp env)
  (eval (let->combination exp) env))
(put 'eval 'let eval-let)

;; Use it ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define expression
  '(let this ((x 1))
     (if (equal? x 16)
         x
         (this (* 2 x)))))

(println "
Evaluating expression:
    " expression "
Expect: 16
Got: "
      (eval
       expression
       the-global-environment))

(--end-- "4.8")

