#lang sicp

(#%require "common.scm")

;   Exercise 3.52
;   =============
;   
;   Consider the sequence of expressions
;   
;   (define sum 0)
;   (define (accum x)
;     (set! sum (+ x sum))
;     sum)
;   (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;   (define y (stream-filter even? seq))
;   (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;                            seq))
;   (stream-ref y 7)
;   (display-stream z)
;   
;   What is the value of sum after each of the above expressions is
;   evaluated?  What is the printed response to evaluating the stream-ref
;   and display-stream expressions?  Would these responses differ if we had
;   implemented (delay <exp>) simply as (lambda () <exp>) without using the
;   optimization provided by memo-proc ?  Explain.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.52]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.52
;   3.5.1 Streams Are Delayed Lists - p325
;   ------------------------------------------------------------------------

(-start- "3.52")

(define program
  '(begin
     
     (define (memo-proc proc)
       (let ((already-run? false) (result false))
         (lambda ()
           (if (not already-run?)
               (begin (set! result (proc))
                      (set! already-run? true)
                      result)
               result))))
     
     (define (stream-map proc s)
       (if (stream-null? s)
           the-empty-stream
           (cons-stream (proc (stream-car s))
                        (stream-map proc (stream-cdr s)))))

     (define (stream-enumerate-interval low high)
       (if (> low high)
           the-empty-stream
           (cons-stream
            low
            (stream-enumerate-interval (+ low 1) high))))

     (define (stream-filter pred stream)
       (cond ((stream-null? stream) the-empty-stream)
             ((pred (stream-car stream))
              (cons-stream (stream-car stream)
                           (stream-filter pred
                                          (stream-cdr stream))))
             (else (stream-filter pred (stream-cdr stream)))))

     (define (stream-ref s n)
       (if (= n 0)
           (stream-car s)
           (stream-ref (stream-cdr s) (- n 1))))

     (define (display-stream s)
       (stream-for-each println s))

     (define (stream-for-each proc s)
       (if (stream-null? s)
           'done
           (begin (proc (stream-car s))
                  (stream-for-each proc (stream-cdr s)))))

     (define sum 0)
     (define (accum x)
       (set! sum (+ x sum))
       sum)    
     (define seq (stream-map accum (stream-enumerate-interval 1 20)))
     (define y (stream-filter even? seq))
     (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                              seq))
     (stream-ref y 7)
     (println "sum after stream-ref: " sum)
     (display-stream z)
     (println "sum after display-stream: " sum)
     ))

(#%require "exercise-3.52-eval.scm")
(put-analyzers)

(prn "With Naive Stream:"
     "==================")
(eval program (setup-environment))

(put 'analyze 'delay
     (lambda (exp)
       (analyze
        (list 'memo-proc (make-lambda '() (cdr exp)))
        )))
       
(prn ""
     ""
     "With Naive Stream plus memo-proc:"
     "=================================")
(eval program (setup-environment))

(--end-- "3.52")

