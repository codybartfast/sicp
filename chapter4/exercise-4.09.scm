#lang sicp

(#%require "common.scm")

;   Exercise 4.9
;   ============
;   
;   Many languages support a variety of iteration constructs, such as do,
;   for, while, and until.  In Scheme, iterative processes can be expressed
;   in terms of ordinary procedure calls, so special iteration constructs
;   provide no essential gain in computational power.  On the other hand,
;   such constructs are often convenient.  Design some iteration constructs,
;   give examples of their use, and show how to implement them as derived
;   expressions.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.9]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.9
;   4.1.2 Representing Expressions - p376
;   ------------------------------------------------------------------------

(-start- "4.9")

(#%require "ea-data-directed.scm")
(put-evaluators)

(println "
I'm assuming that while, do and for are primarily imperative constructs and
that normally (i.e., in other langauges) they do not have return values and
are only used for their side effects.  Therefore the return value of these
procs is undefined.
")


(define make-call cons)

;; Named let (from ex 4.08) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-named-let name params body)
  (list 'let name params body))

(define (make-definition name params body)
  (cons 'define
        (cons (cons name params)
              body)))

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
(define let-pair-id car)
(define let-pair-value cadr)
(define (let-params exp)
  (map let-pair-id
       (let-pairs exp)))
(define (let-values exp)
  (map let-pair-value
       (let-pairs exp)))

(define (let->combination exp )
  (if (named-let? exp)
      (make-begin
       (list
        (make-definition (let-name exp)
                         (let-params exp)
                         (let-body exp))
        (make-call (let-name exp)
                   (let-values exp))))
      (make-call
       (make-lambda (let-params exp)
                    (let-body exp))
       (let-values exp))))

(define (eval-let exp env)
  (eval (let->combination exp) env))
(put 'eval 'let eval-let)

;; while ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-while predicate body)
  (cons 'while (cons predicate body)))

(define while-predicate cadr)
(define while-body cddr)

(define (while->combination exp)
  (make-named-let 'loop
                  '()
                  (make-if (while-predicate exp)
                           (make-begin
                            (append
                             (while-body exp)
                             '((loop))))
                           'undefined)))

(define (eval-while exp env)
  (eval (while->combination exp) env))
(put 'eval 'while eval-while)

;; do ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define do-body cddr)
(define do-predicate cadr)

(define (do->combination exp)
  (make-begin
   (list
    (make-call (make-lambda '() (do-body exp)) '())
    (make-while (do-predicate exp) (do-body exp)))))

(define (eval-do exp env)
  (eval (do->combination exp) env))
(put 'eval 'do eval-do)

;; for ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-let pairs body)
  (cons 'let (cons pairs body)))

(define for-pairs cadr)
(define for-predicate caddr)
(define for-iterate cadddr)
(define for-body cddddr)

(define (for->combination exp)
  (make-let (for-pairs exp) ;; value pairs as in a 'let'
            (list
             (make-while (for-predicate exp)     ;; if predicate true
                         (append
                          (for-body exp)         ;; eval body
                          (for-iterate exp)))))) ;; and call iterate

(define (eval-for exp env)
  (eval (for->combination exp) env))
(put 'eval 'for eval-for)
               
;; Use them ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; As mentioned above these are 'imperative' procs so we can only test by
;; looking at their side effect - displayed text, and not by their return
;; values.

;; helper function
(define (print-eval exp expected)
  (define (sink x) "")
  (println "
Evaluating expression:
    " exp "
Expect: " expected "
" (sink (eval exp the-global-environment)) "
-------------------------------------------"))

;; Use while ;;;;;;
(println "
While loop:
============
Basic print and decrement.
")

(print-eval
 '(begin
    (list
     (define bell-count 5)

     (while (> bell-count 0)
            (println "Ding! Dong!" )
            (set! bell-count (- bell-count 1)))))
 "Five bells printed above.")


;; Use 'do' ;;;;;;
(println "
Simple 'do':
============
Check body is called at least once even if predicate always false.
")

(print-eval
 '(do false
    (println "Hello! from do-exp1"))
 "One greeting printed above")

(println "
Using 'do':
===========
Regular 'do' where predicate is true then false.
")

(print-eval
 '(begin
    (list
     (define greeting-count 3)

     (do (> greeting-count 0)
       (println "Hello! from do-exp2  " )
       (set! greeting-count (- greeting-count 1)))))
 "Three greetings printed above.")

;; Use 'for' ;;;;;;
(println "
Using 'for':
===========
For loop with seperate 'init-pairs', 'predicate', 'iterate' and body.
")

(print-eval
 '(for
      ((x 0) (y 9))
    (> y x)
    ((set! x (+ x 2))
     (set! y (+ y 1)))
    (println (+ x y)))    
 "Multiples of 3 from 9 -> 33 printed above.")

(println "

If music be the food of love, play on; Give me excess of it ...
")
(--end-- "4.9")

