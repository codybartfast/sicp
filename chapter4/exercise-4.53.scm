#lang sicp

(#%require "common.scm")

;   Exercise 4.53
;   =============
;   
;   With permanent-set! as described in exercise [4.51] and if-fail as in
;   exercise [4.52], what will be the result of evaluating
;   
;   (let ((pairs '()))
;     (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
;                (permanent-set! pairs (cons p pairs))
;                (amb))
;              pairs))
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.53]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.53
;   [Exercise 4.51]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.51
;   [Exercise 4.52]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.52
;   4.3.3 Implementing the <tt>Amb</tt> Evaluator - p437
;   ------------------------------------------------------------------------

(-start- "4.53")

(println "The result is:

  ((8 35) (3 110) (3 20))

Sample output:

  ;;; Starting a new problem 
  ;;; Amb-Eval value:
  ((8 35) (3 110) (3 20))
")

(#%require "ea-analyzing-50.scm")
(put-evaluators)

#| paste the following into driver loop to demonstrate

(define (prime? n)
  (define (divides? a b)
    (= 0 (remainder b a)))
  (define (next d)
    (if (= d 2)
        3
        (+ d 2)))
  (define (find-factor f)
    (cond 
      ((> (* f f) n) n)
      ((divides? f n) f)
      (else (find-factor (next f)))))
  (= n (find-factor 2)))
(define (require p) (if (not p) (amb)))
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))
(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))
(let ((pairs '()))
  (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
             (permanent-set! pairs (cons p pairs))
             (amb))
           pairs))

|#


(driver-loop)

(--end-- "4.53")

