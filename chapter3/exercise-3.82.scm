#lang sicp

(#%require "common.scm")

;   Exercise 3.82
;   =============
;   
;   Redo exercise [3.5] on Monte Carlo integration in terms of streams.  The
;   stream version of estimate-integral will not have an argument telling
;   how many trials to perform.  Instead, it will produce a stream of
;   estimates based on successively more trials.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.82]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.82
;   [Exercise 3.5]:  http://sicp-book.com/book-Z-H-24.html#%_thm_3.5
;   3.5.5 Modularity of Functional Programs and Modularity of Objects - p354
;   ------------------------------------------------------------------------

(-start- "3.82")

;; To help demonstrate
;; -------------------

(define (display-line x)
  (display "    ")
  (display x)
  (newline))

(define (display-stream s)
  (cond ((not (stream-null? s))
         (display-line (stream-car s))
         (display-stream (stream-cdr s)))))

(define (take n stream)
  (if (= n 0)
      '()
      (cons-stream
       (stream-car stream)
       (take (- n 1) (stream-cdr stream)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))


;; Stream dependencies
;; -------------------

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))


;; From Ex 3.5
;; -----------

(define rand-size (expt 2 32))
(define rand-size-real (* 1.0 rand-size))

(define rand-update
  (lambda (x)
    (let ((a 1664525)
          (b 1013904223)
          (m rand-size))
      (modulo
       (+ (* x a) b)
       m))))

;; below we have a map that takes two streams of random values. As as their
;; initiated at the same time they both got the same value for runtime and
;; so both returned the same values.  This resulted in all the 'random'
;; points being on the line x=y and instead of an estimate of pi we get an 
;; estimate of the square root of 8.
;;
;; This 'sleep' function is called ensure the two random number streams are
;; initiated with different values of 'runtime'

(define (sleep n)
  (define (iter str n)
    (if (> n 0)
        (iter (string-append str " ") (- n 1))))
  (iter "" n))

(define (random-init)
  (sleep 10000)
  (rand-update (runtime)))


;; Random streams
;; --------------

(define (random-numbers)
  (define rand-stream
    (cons-stream (random-init)
                 (stream-map rand-update rand-stream)))
  rand-stream)

(define (rand-range low high)
  (define (rand-to-ranged rand)
    (+ low
       (/ (* rand (- high low))
          rand-size-real)))
  (stream-map rand-to-ranged (random-numbers)))


;; Monte Carlo
;; -----------

(define (monte-carlos experiments)
  (define (iter prev-total prev-passed experiments)
    (let ((total (+ prev-total 1))
          (passed (if ((stream-car experiments))
                      (+ prev-passed 1)
                      prev-passed)))
      (cons-stream
       (/ passed total)
       (iter total passed (stream-cdr experiments)))))
  (iter 0.0 0.0 experiments))


;; Experiments
;; -----------

(define (experiments x1 x2 y1 y2)
  (define (is-in-circle x y) 
    (> 1.0 (+ (* x x) (* y y))))
  (define (get-experiment x y)
    (lambda ()
      (is-in-circle x y)))
  (stream-map get-experiment (rand-range x1 x2) (rand-range y1 y2)))


;; Pi Estimates
;; ------------

(define (pi-estimates x1 x2 y1 y2)
  (let ((area-of-rectangle (* (- x2 x1) (- y2 y1))))
    (scale-stream 
     (monte-carlos (experiments x1 x2 y1 y2))
     area-of-rectangle)))


;; Demonstrate
;; -----------

(define estimates (pi-estimates -1 1 -1 1))

(prn "" "First 20 estimates...")
(display-stream (take 20 estimates))

(prn "" "1,000,000'th estimate ...")
(stream-ref estimates 1000000)

(--end-- "3.82")

