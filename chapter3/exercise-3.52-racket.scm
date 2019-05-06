#lang racket
(require racket/stream)

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
     
;(define (stream-map proc s)
;  (if (stream-empty? s)
;      empty-stream
;      (stream-cons (proc (stream-first s))
;                   (stream-map proc (stream-rest s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1) high))))

;(define (stream-filter pred stream)
;  (cond ((stream-empty? stream) empty-stream)
;        ((pred (stream-first stream))
;         (stream-cons (stream-first stream)
;                      (stream-filter pred
;                                     (stream-rest stream))))
;        (else (stream-filter pred (stream-rest stream)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-first s)
      (stream-ref (stream-rest s) (- n 1))))

(define (display-stream s)
  (stream-for-each println s))

(define (stream-for-each proc s)
  (if (stream-empty? s)
      'done
      (begin (proc (stream-first s))
             (stream-for-each proc (stream-rest s)))))

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