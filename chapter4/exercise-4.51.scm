#lang sicp

(#%require "common.scm")

;   Exercise 4.51
;   =============
;   
;   Implement a new kind of assignment called permanent-set! that is not
;   undone upon failure.  For example, we can choose two distinct elements
;   from a list and count the number of trials required to make a successful
;   choice as follows:
;   
;   (define count 0)
;   (let ((x (an-element-of '(a b c)))
;         (y (an-element-of '(a b c))))
;     (permanent-set! count (+ count 1))
;     (require (not (eq? x y)))
;     (list x y count))
;   ;;; Starting a new problem
;   ;;; Amb-Eval value:
;   (a b 2)
;   ;;; Amb-Eval input:
;   try-again
;   ;;; Amb-Eval value:
;   (a c 3)
;   
;   What values would have been displayed if we had used set! here rather
;   than permanent-set! ?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.51]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.51
;   4.3.3 Implementing the <tt>Amb</tt> Evaluator - p436
;   ------------------------------------------------------------------------

(-start- "4.51")

(println "
permanent-set!:

  (define (analyze-permanent-assignment exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp))))
      (lambda (env succeed fail)
        (vproc env
               (lambda (val fail2)        ; *1*
                 (set-variable-value! var val env)
                 (succeed 'ok fail2))
               fail))))

This is the existing set! implementation with the code removed that stores
and re-assigns the existing value.  Sample output:

  ;;; Amb-Eval input:

  ;;; Starting a new problem 
  ;;; Amb-Eval value:
  (a b 2)

  ;;; Amb-Eval input:

  ;;; Amb-Eval value:
  (a c 3)

With regular set!, count is always 1.")

(#%require "ea-analyzing-50.scm")
(put-evaluators)

#| paste the following into driver loop to demonstrate
 
 (define (require p) (if (not p) (amb)))
 (define (an-element-of items)
   (require (not (null? items)))
   (amb (car items) (an-element-of (cdr items))))

 (define count 0)
 (let ((x (an-element-of '(a b c)))
       (y (an-element-of '(a b c))))
   (permanent-set! count (+ count 1))
   (require (not (equal? x y)))
   (list x y count))
 try-again

 |#
     
(driver-loop)


(--end-- "4.51")

