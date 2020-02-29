#lang sicp

(#%require "common.scm")

;   Exercise 5.43
;   =============
;   
;   We argued in section [4.1.6] that internal definitions for block
;   structure should not be considered "real" defines.  Rather, a procedure
;   body should be interpreted as if the internal variables being defined
;   were installed as ordinary lambda variables initialized to their correct
;   values using set!.  Section [4.1.6] and exercise [4.16] showed how to
;   modify the metacircular interpreter to accomplish this by scanning out
;   internal definitions.  Modify the compiler to perform the same
;   transformation before it compiles a procedure body.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.43]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.43
;   [Section 4.1.6]: http://sicp-book.com/book-Z-H-26.html#%_sec_4.1.6
;   [Exercise 4.16]: http://sicp-book.com/book-Z-H-35.html#%_thm_4.16
;   5.5.6 Lexical Addressing - p603
;   ------------------------------------------------------------------------

(-start- "5.43")

(#%require "compiler-39.scm")

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define make-call cons)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (scan-out-defines exp)
  (define (scan exp new-members vars)
    (if (null? exp)
        (cons new-members vars)
        (let ((member (car exp)))
          (if (definition? member)
              (scan (cdr exp)
                    (cons
                     (list 'set!
                           (definition-variable member)
                           (definition-value member))
                     new-members)
                    (cons (definition-variable member) vars))
              (scan (cdr exp)
                    (cons member new-members)
                    vars)))))
  (let ((scan-rslt (scan exp '() '())))
    (let ((new-body (reverse (car scan-rslt)))
          (vars (reverse (cdr scan-rslt))))
      (let ((vals (map (lambda (var) '*unassigned*) vars)))
        (if (null? vars)
            exp
            (make-call (make-lambda vars new-body) vals))))))

(scan-out-defines
 '(
    (define a n)
    (define (double n) (* 2 n))
    (double a)))

(--end-- "5.43")

