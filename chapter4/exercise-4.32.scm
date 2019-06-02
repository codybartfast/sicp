#lang sicp

(#%require "common.scm")

;   Exercise 4.32
;   =============
;   
;   Give some examples that illustrate the difference between the streams of
;   chapter 3 and the "lazier" lazy lists described in this section. How can
;   you take advantage of this extra laziness?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.32]: http://sicp-book.com/book-Z-H-27.html#%_thm_4.32
;   4.2.3 Streams as Lazy Lists - p411
;   ------------------------------------------------------------------------

(-start- "4.32")

(println "
The code below demonstrates the expected behaviour with the first member of
the list being evaluated at creation time with cons-stream, but not being
evaluated until accessed using the redefined, lazy cons.")

(define (tprintln x)
  (println "tprinting: " x)
  x)


(println "
Lazy List
=========")

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define lazy-list
  (cons-stream
   (tprintln 1)
   (cons-stream
    (tprintln 2)
    (cons-stream
     (tprintln 3)
     '()))))

(println "Created lazy-list")

(println (stream-car lazy-list))
(println (stream-car (stream-cdr lazy-list)))
(println (stream-car (stream-cdr (stream-cdr lazy-list))))

(#%require "ea-data-directed-32.scm")
(put-evaluators)

(define program1
  '(begin

     (define (cons (x lazy-memo) (y lazy-memo))
       (lambda (m) (m x y)))
     (define (car z)
       (z (lambda (p q) p)))
     (define (cdr z)
       (z (lambda (p q) q)))

     (define (tprintln x)
       (println "tprinting: " x)
       x)

     (println "
Lazier List
===========")

     (define lazier-list
       (cons
        (tprintln 1)
        (cons
         (tprintln 2)
         (cons
          (tprintln 3)
          '()))))

     (println "Created lazier-list")

     (println (car lazier-list))
     (println (car (cdr lazier-list)))
     (println (car (cdr (cdr lazier-list))))

     ))

(eval program1 the-global-environment)

(println "
The extra laziness will be of use when there are side effects or 'impure'
functions.  E.g., suppose the list contained procedures that read the
current temperature from three different thermomenters, we would want the
first temperature to be the temperature when that first item was accessed,
not when the list was created.

The consistent laziness also makes code easier to reason about.  The code
below shows two lists of adjustments to a variable x.  A list of additions
and a list of multiplications.  If we enumerate the two lists then we get
the intuitive result with the extra lazines:
    - apply all additions
    - apply all multiplications
But with cons-stream we get the result from:
    - apply first addition
    - apply first multiplication
    - apply rest of addition
    - apply reset of multiplication
")

(define x 1)

(define (add n)
  (set! x (+ x n))
  x)

(define (mult n)
  (set! x (* x n))
  x)

(define list1
  (cons-stream
   (add 2)
   (cons-stream
    (add 3)
    (cons-stream
     (add 5) '()))))

(define list2
  (cons-stream
   (mult 2)
   (cons-stream
    (mult 3)
    (cons-stream
     (mult 5) '()))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons-stream (stream-car list1) 
            (append (stream-cdr list1) 
                    list2))))

(define (list-ref items n)
  (if (= n 0)
      (stream-car items)
      (list-ref (stream-cdr items) (- n 1))))

(println "final x with lazy list "
 (list-ref (append list1 list2) 5))
(println "(((1 + 2) * 2) + 3 + 5) * 3 * 5)
")

(define program2
  '(begin

     (define (cons (x lazy-memo) (y lazy-memo))
       (lambda (m) (m x y)))
     (define (car z)
       (z (lambda (p q) p)))
     (define (cdr z)
       (z (lambda (p q) q)))
     
     (define x 1)

     (define (add n)
       (set! x (+ x n))
       x)

     (define (mult n)
       (set! x (* x n))
       x)

     (define list1
       (cons
        (add 2)
        (cons
         (add 3)
         (cons
          (add 5) '()))))

     (define list2
       (cons
        (mult 2)
        (cons
         (mult 3)
         (cons
          (mult 5) '()))))

     (define (append list1 list2)
       (if (null? list1)
           list2
           (cons (car list1) 
                 (append (cdr list1) 
                         list2))))

     (define (list-ref items n)
       (if (= n 0)
           (car items)
           (list-ref (cdr items) (- n 1))))
     
     (println "final x with lazier cons: "
              (list-ref (append list1 list2) 5))
     (println "(1 + 2 + 3 + 5) * 2 * 3 * 5")
     
     ))

(eval program2 the-global-environment)

(--end-- "4.32")

