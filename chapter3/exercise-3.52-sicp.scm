#lang sicp

(#%require "common.scm")

(define (println . parts)
  (for-each display parts)
  (newline))
     
;(define (memo-proc proc)
;  (let ((already-run? false) (result false))
;    (lambda ()
;      (if (not already-run?)
;          (begin (set! result (proc))
;                 (set! already-run? true)
;                 result)
;          result))))
     
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
(println "sum after define accum: " sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(println "sum after define seq: " sum)
     
(define y (stream-filter even? seq))
(println "sum after define y: " sum)
     
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
(println "sum after define z: " sum)

(stream-ref y 7)
(println "sum after stream-ref: " sum)

(display-stream z)
(println "sum after display-stream: " sum)