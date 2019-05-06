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
(prn "
+--------------------------+--------------+-------------+--------------+
|                          | SICP Scheme  | SICP Scheme | Racket With  |
|                          |    With      |   Without   |   Built in   |
|                          | Memoization  | Memoization | Map & Filter |
+--------------------------+--------------+-------------+--------------+
| sum after define accum   |        0     |       0     |        0     |
+--------------------------+--------------+-------------+--------------+
| sum after define seq     |        1     |       1     |        0     |
+--------------------------+--------------+-------------+--------------+
| sum after define y       |        6     |       6     |        0     |
+--------------------------+--------------+-------------+--------------+
| sum after define z       |       10     |      15     |        0     |
+--------------------------+--------------+-------------+--------------+
| sum after stream-ref     |      136     |     162     |      136     |
+--------------------------+--------------+-------------+--------------+
| sum after display-stream |      210     |     362     |      210     |
+--------------------------+--------------+-------------+--------------+

  Printed response with memoization: 10, 15, 45, 55, 105, 120, 190, 210
  Printed response without memoization: 15, 180, 230, 305
  Printed response with Racket: 10, 15, 45, 55, 105, 120, 190, 210

Unlike generators and streams from most languages, including modern Scheme,
the first element of the stream is not delayed and is evaluated at creation
time.  With memoization this doesn't make a difference to the elements of
seq (and hence values of sum) as they are evaluated just once and always in
the same order.  But without memoization several elements are evaluated more
than once and the order in which they are evaluated will affect the values
of seq.

The lack of a delay for the first item of a stream is discussed in the
Rationale of SRFI 41 (Scheme Request For Implementation) where Abelson and
Sussman's implementation is described as 'odd' streams and this chapter of
SICP is referenced for understanding 'odd' streams.  However, 'even' streams
(Wadler et al), which do delay the first item, predominate today.

The non-memoizing results were obtained by implementing a non-memoizing
stream using the language implementation from Chapter 4.


============================================================================
==  Run the Code  ==========================================================
============================================================================
")

;; The program from the exercise and text
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
     ))


;; An implemenation of the language from Chapter 4
(#%require "exercise-3.52-eval.scm")
(put-analyzers)

;; Existing non-memoizing delay analyzer
(define original-delay-analyzer (get 'analyze 'delay))

;; Memoizing delay analyzer
(define memoizing-delay-analyzer
  (lambda (exp)
       (analyze
        (list 'memo-proc (make-lambda '() (cdr exp))))))

;; First run with the memoizing version of delay
(put 'analyze 'delay memoizing-delay-analyzer)
(prn "With Memoization"
     "================")
(eval program (setup-environment))

;; And now the original non-memoizing version
(put 'analyze 'delay original-delay-analyzer)
(prn ""
     "Without Memoization"
     "===================")
(eval program (setup-environment))



(prn "

============================================================================
==  Output from running above code  ========================================
============================================================================

>   With Memoization
>   ================
>   sum after define accum: 0
>   sum after define seq: 1
>   sum after define y: 6
>   sum after define z: 10
>   sum after stream-ref: 136
>   10
>   15
>   45
>   55
>   105
>   120
>   190
>   210
>   sum after display-stream: 210
>
>   Without Memoization
>   ===================
>   sum after define accum: 0
>   sum after define seq: 1
>   sum after define y: 6
>   sum after define z: 15
>   sum after stream-ref: 162
>   15
>   180
>   230
>   305
>   sum after display-stream: 362


============================================================================
==  With Memoization  ======================================================
============================================================================

Call to define seq:
===================
      sum:   0 |
 interval:     | 1
---------------+--
      seq:     | 1

Call to define y:
=================
      sum:   1 |
  car seq:   1 | 1
 interval:   1 |   2 3
      seq:     |   3 6
---------------+------
        y:     | - - 6


Call to define z:
=================
      sum:   6 |
  car seq:   1 | 1
    car y:   6 |
 interval:   3 |        4
      seq:     |       10
 memoized:     |   3 6 
---------------+---------
        z:     | - - - 10


Call to stream-ref y 7
======================
      sum:  10 |
  car seq:   1 |
    car y:   6 | 6  
    car z:  10 |
 interval:   4 |       5  6  7  8  9 10 11 12 13  14  15  16
      seq:     |      15 21 28 36 45 55 66 78 91 105 120 136
 memoized:     |   10
---------------+--------------------------------------------
 stream-ref:   | 6 10  -  - 28 36  -  - 66 78  -   - 120 136


Call to display-stream
======================
      sum: 136 |
  car seq:   1 |
    car y:   6 |
    car z:  10 | 10
 interval:  16 |                                            17  18  19  20
      seq:     |                                           153 171 190 210
 memoized:     |    15 21 28 36 45 55 66 78 91 105 120 136
---------------+----------------------------------------------------------
display-stream:| 10 15  -  -  - 45 55  -  -  - 105 120   -   -   - 190 210


============================================================================
==  Without Memoization  ===================================================
============================================================================

Call to define seq:
===================
      sum:   0 |
 interval:     | 1
---------------+--
      seq:     | 1

Call to define y:
=================
      sum:   1 |
  car seq:   1 | 1
 interval:   1 |   2 3
      seq:     |   3 6
---------------+------
        y:     | - - 6


Call to define z:
=================
      sum:   6 |
  car seq:   1 | 1
    car y:   6 |
 interval:   1 |   2  3  4
      seq:     |   8 11 15
---------------+----------
        z:     | - -  - 15


Call to stream-ref y 7
======================
      sum:  15 |
  car seq:   1 |
    car y:   6 | 6
    car z:  15 |
 interval:   3 |    4  5  6  7  8  9 10 11 12  13  14  15  16  17
      seq:     |   19 24 30 37 45 54 64 75 87 100 114 129 145 162
---------------+-------------------------------------------------
 streamm-ref:  | 6  - 24 30  -  - 54 64  -  - 100 114   -   - 162

       
Call to display-stream
======================
      sum: 162 |
  car seq:   1 |
    car y:   6 |
    car z:  15 | 15
 interval:   4 |      5   6   7   8   9  10  11  12 ...  16  17  18  19  20
      seq:     |    167 173 180 188 197 207 218 230 ... 288 305 323 342 362
---------------+-----------------------------------     -------------------
display-stream:| 15   -   - 180   -   -   -   - 230 ...   - 305   -   -   -
")

(--end-- "3.52")

