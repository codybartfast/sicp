#lang sicp

(#%require "common.scm")

;   Exercise 4.52
;   =============
;   
;   Implement a new construct called if-fail that permits the user to catch
;   the failure of an expression.  If-fail takes two expressions.  It
;   evaluates the first expression as usual and returns as usual if the
;   evaluation succeeds.  If the evaluation fails, however, the value of the
;   second expression is returned, as in the following example:
;   
;   ;;; Amb-Eval input:
;   (if-fail (let ((x (an-element-of '(1 3 5))))
;              (require (even? x))
;              x)
;            'all-odd)
;   ;;; Starting a new problem
;   ;;; Amb-Eval value:
;   all-odd
;   ;;; Amb-Eval input:
;   (if-fail (let ((x (an-element-of '(1 3 5 8))))
;              (require (even? x))
;              x)
;            'all-odd)
;   ;;; Starting a new problem
;   ;;; Amb-Eval value:
;   8
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.52]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.52
;   4.3.3 Implementing the <tt>Amb</tt> Evaluator - p436
;   ------------------------------------------------------------------------

(-start- "4.52")

(println "
if-fail:

  (define try-expr cadr)
  (define fail-expr caddr)

  (define (analyze-if-fail exp)
    (let ((tproc (analyze (try-expr exp)))
          (fproc (analyze (fail-expr exp))))
      (lambda (env succeed fail)
        (tproc env succeed (lambda () (fproc env succeed fail))))))

Sample output:

  ;;; Amb-Eval input:

  ;;; Starting a new problem 
  ;;; Amb-Eval value:
  all-odd

  ;;; Amb-Eval input:

  ;;; Starting a new problem 
  ;;; Amb-Eval value:
  8

")


(#%require "ea-analyzing-50.scm")
(put-evaluators)

#| paste the following into driver loop to demonstrate

  (define (even? n) (= 0 (remainder n 2)))
  (define (require p) (if (not p) (amb)))
  (define (an-element-of items)
    (require (not (null? items)))
    (amb (car items) (an-element-of (cdr items))))
  (if-fail (let ((x (an-element-of '(1 3 5))))
             (require (even? x))
             x)
           'all-odd)

  (if-fail (let ((x (an-element-of '(1 3 5 8))))
             (require (even? x))
             x)
           'all-odd)


|#
     
(driver-loop)

(--end-- "4.52")

