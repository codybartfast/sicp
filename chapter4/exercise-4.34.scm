#lang sicp

(#%require "common.scm")

;   Exercise 4.34
;   =============
;   
;   Modify the driver loop for the evaluator so that lazy pairs and lists
;   will print in some reasonable way.  (What are you going to do about
;   infinite lists?) You may also need to modify the representation of lazy
;   pairs so that the evaluator can identify them in order to print them.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.34]: http:/make/sicp-book.com/book-Z-H-27.html#%_thm_4.34
;   4.2.3 Streams as Lazy Lists - p411
;   ------------------------------------------------------------------------

(-start- "4.34")

(#%require "ea-data-directed-34.scm")
(put-evaluators)

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

;(define (user-print object)
;  (if (compound-procedure? object)
;      (display (list 'compound-procedure
;                     (procedure-parameter-defs object)
;                     (procedure-body object)
;                     '<procedure-env>))
;      (display object)))

(define (user-print object)
  (if (compound-procedure? object)
      (user-print-list object 4)
      (display 'other)))

(define (user-print-list object count)
  (display (apply object '((lambda (p q) p)) the-global-environment))
  (newline)
  (display 'done)
  )

(define (lcar z)
  (z (lambda (p q) p)))
(define (lcdr z)
  (z (lambda (p q) q)))

(define program
  '(begin

     (define (cons (x lazy-memo) (y lazy-memo))
       (lambda (m) (m x y)))
     (define (car z)
       (z (lambda (p q) p)))
     (define (cdr z)
       (z (lambda (p q) q)))

     (define list1
       (cons 'apple '()))
     
     (define list2
       (cons 'apple
             (cons 'banana '())))

     (define list4
       (cons 'apple
             (cons 'banana
                   (cons 'cherry
                         (cons 'durian '())))))

     (define list8
       (cons 'apple
             (cons 'banana
                   (cons 'cherry
                         (cons 'duck
                               (cons 'elk
                                     (cons 'fox
                                           (cons 'grape
                                                 (cons 'horse '())))))))))

     (define (add2) (+ 2 0))

     ))

(eval program the-global-environment)


(driver-loop)

(--end-- "4.34")

