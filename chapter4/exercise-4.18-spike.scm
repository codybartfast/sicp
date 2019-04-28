#lang sicp

(#%require "common.scm")

;   Exercise 4.18
;   =============
;   
;   Consider an alternative strategy for scanning out definitions that
;   translates the example in the text to
;   
;   (lambda <vars>
;     (let ((u '*unassigned*)
;           (v '*unassigned*))
;       (let ((a <e1>)
;             (b <e2>))
;         (set! u a)
;         (set! v b))
;       <e3>))
;   
;   Here a and b are meant to represent new variable names, created by the
;   interpreter, that do not appear in the user's program. Consider the
;   solve procedure from section [3.5.4]:
;   
;   (define (solve f y0 dt)
;     (define y (integral (delay dy) y0 dt))
;     (define dy (stream-map f y))
;     y)
;   
;   Will this procedure work if internal definitions are scanned out as
;   shown in this exercise?  What if they are scanned out as shown in the
;   text?  Explain.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.18]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.18
;   [Section 3.5.4]: http://sicp-book.com/book-Z-H-24.html#%_sec_3.5.4
;   4.1.6 Internal Definitions - p390
;   ------------------------------------------------------------------------

(-start- "4.18")
(#%require "ea-data-directed-18.scm")
(put-evaluators)
(#%require "ea-pick-fruit-expression.scm")

;; delay
(define (delay->lambda exp)
  (make-lambda '() (cdr exp)))
(define (eval-delay exp env)
  (eval (delay->lambda exp) env))
(put 'eval 'delay eval-delay)

;; force
(define (eval-force exp env)
     (eval (cdr exp) env))
(put 'eval 'force eval-force)

;; cons-stream
(define (cons-stream->cons exp)
  (list 'cons (cadr exp) (list 'delay (caddr exp))))
(define (eval-cons-stream exp env)
  (eval (cons-stream->cons exp) env))
(put 'eval 'cons-stream eval-cons-stream)

;; stream-null?
(define (stream-null?->null? exp)
  (cons 'null? (cdr exp)))
(define (eval-stream-null? exp env)
  (eval (stream-null?->null? exp) env))
(put 'eval 'stream-null? eval-stream-null?)

;; stream-car
(define (eval-stream-car exp env)
  (eval (cons 'car (cdr exp)) env))
(put 'eval 'stream-car eval-stream-car)
  
;; stream-cdr
(define (eval-stream-cdr exp env)
  (println (list 'force (list 'cdr (cadr exp))))
  (eval (list 'force (list 'cdr (cadr exp))) env))
(put 'eval 'stream-cdr eval-stream-cdr)


(define expression
  '(lambda ()
     
     ;(define (stream-car stream) (car stream))

     ;(define (stream-cdr stream) (force (cdr stream)))

     (define (stream-map proc s)
       (if (stream-null? s)
           the-empty-stream
           (cons-stream (proc (stream-car s))
                        (stream-map proc (stream-cdr s)))))

     (define (scale-stream stream factor)
       (stream-map (lambda (x) (* x factor)) stream))

     (define (add-streams s1 s2)
       (cons-stream (+ (stream-car s1) (stream-car s2))
                    (add-streams (stream-cdr s1) (stream-cdr s2))))

     (define (integral integrand initial-value dt)
       (define int
         (cons-stream initial-value
                      (add-streams (scale-stream (force integrand) dt)
                                   int)))
       int)

     (define (solve f y0 dt)
       (define y (integral (delay dy) y0 dt))
       (define dy (stream-map f y))
       y)

     ;     (define solve
     ;       (lambda (f y0 dt)
     ;         (let ((y '*unassigned*)
     ;               (dy '*unassigned*))
     ;           (let ((a (integral (delay dy) y0 dt))
     ;                 (b (stream-map f y)))
     ;             (set! y a)
     ;             (set! dy b)
     ;             y))))
     
     ;(define solution (solve (lambda (x) x) 0 1))
     (define solution (solve (lambda (x) x) 1 1))

     ;     (define (integers-starting-from n)
     ;       (cons-stream n (integers-starting-from (+ n 1))))
     ;
     ;     (define integers (integers-starting-from 1))
     ;
     ;     (define evens (scale-stream integers 2))
     ;
     ;     (define fives (add-streams integers
     ;                                (scale-stream evens 2)))

     ;(stream-car (stream-cdr (stream-cdr fives)))
     ;(stream-car (stream-cdr (stream-cdr solution)))

     (define (reverse list)
       (define (iter list rev-list)
         (if (null? list)
             rev-list
             (iter (cdr list) (cons (car list) rev-list))))
       (iter list '()))
     
     (define (stream->list count stream)
       (define (iter count stream list)
         (if (equal? count 0)
             list
             (iter (- count 1)
                   (stream-cdr stream)
                   (cons (stream-car stream) list))))
       (reverse (iter count stream '())))

     (println (stream->list 5 solution))
     ))


(apply (eval expression the-global-environment) '())

(println "")
(--end-- "4.18")

