#lang sicp

;; Provides 'put' and 'get' for a data-directed eval.  It uses a single key
;; table (for simplicity) but presents a double key interface (in case it is
;; needed).  Initial key has to be 'eval.

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
  (if (not (eq? op 'eval))
      (error "'eval is the only supported operation -- EVALUATORS")))

(define evaluators (make-table))

(define (put op exp-type evaluator)
  (check-op op)
  (insert! exp-type evaluator evaluators)
  (if #f 'unreachable))

(define (get op exp-type)
  (check-op op)
  (lookup exp-type evaluators))

(#%provide put get)