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

(define (cons-proc? obj)
  (tagged-list? obj 'cons-proc))

(define (user-print object)
  (cond ((compound-procedure? object)
         (user-print-proc object))
        ((cons-proc? object)
         (user-print-list object))
        (else (display object))))

(define (user-print-proc object)
  (display (list 'compound-procedure
                 (procedure-parameter-defs object)
                 (procedure-body object)
                 '<procedure-env>)))

(define (user-print-list object)
  (display "(")
  (user-print-list-head object 4))

(define (user-print-list-head object count)
  (cond ((< 0 count)
         (let ((list-car
                (apply (cdr object)
                       '((lambda (p q) p))
                       the-global-environment))
               (list-cdr
                (apply (cdr object)
                       '((lambda (p q) q))
                       the-global-environment)))
           (user-print list-car)
           (cond ((pair? list-cdr)
                  (display " ")
                  (user-print-list-head list-cdr (- count 1)))
                 (else (display ")")))))           
        (else (display "...)"))))


(define program
  '(begin

     (define (cons (x lazy-memo) (y lazy-memo))
       (ucons 'cons-proc (lambda (m) (m x y))))
     (define (car z)
       ((ucdr z) (lambda (p q) p)))
     (define (cdr z)
       ((ucdr z) (lambda (p q) q)))

     (define fruit
       (cons
        'apple
        (cons
         'banana '())))

     (define animals
       (cons
        'aardvark
        (cons
         'bee
         (cons
          'cat
          (cons
           'duck '())))))

     (define list-of-5
       (cons
        fruit
        (cons
         animals
         (cons
          'minerals
          (cons
           'colours
           (cons
            'emotions
            '()))))))

     (define (add2 n) (+ 2 n))

     ))

(eval program the-global-environment)

;;  Sample Output:
;;  ==============
;;
;;  ;;; M-Eval input:
;;  'abc
;;  
;;  ;;; M-Eval value:
;;  abc
;;  
;;  ;;; M-Eval input:
;;  add2
;;  
;;  ;;; M-Eval value:
;;  (compound-procedure (n) ((+ 2 n)) <procedure-env>)
;;  
;;  ;;; M-Eval input:
;;  list-of-5
;;  
;;  ;;; M-Eval value:
;;  ((apple banana) (aardvark bee cat duck) minerals colours ...)

(driver-loop)

(--end-- "4.34")

