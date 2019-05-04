#lang sicp

;; Provides 'put' and 'get' for analyzers.  It uses a single key
;; table (for simplicity) but presents a double key interface (in case it is
;; needed).  Initial key has to be 'analyzer.

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define (check-op op)
  (if (not (eq? op 'analyze))
      (error "'analyze is the only supported operation -- ANALYZERS")))

(define analyzers (make-table))

(define (put op exp-type analyzer)
  (check-op op)
  (insert! exp-type analyzer analyzers)
  (if #f 'unreachable))

(define (get op exp-type)
  (check-op op)
  (lookup exp-type analyzers))

(#%provide put get)