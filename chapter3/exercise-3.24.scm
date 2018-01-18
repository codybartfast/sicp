#lang sicp

(#%require "common.scm")

;   Exercise 3.24
;   =============
;   
;   In the table implementations above, the keys are tested for equality
;   using equal? (called by assoc).  This is not always the appropriate
;   test.  For instance, we might have a table with numeric keys in which we
;   don't need an exact match to the number we're looking up, but only a
;   number within some tolerance of it. Design a table constructor
;   make-table that takes as an argument a same-key? procedure that will be
;   used to test "equality" of keys.  Make-table should return a dispatch
;   procedure that can be used to access appropriate lookup and insert!
;   procedures for a local table.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.24]: http://sicp-book.com/book-Z-H-22.html#%_thm_3.24
;   3.3.3 Representing Tables - p272
;   ------------------------------------------------------------------------

(-start- "3.24")

;; Table from book, but with name of dispatch method shortened.


(define (make-booktable)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;;=========================================================
;; New make-table that takes 'same?' parameter.

(define (make-table same?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;;=========================================================
;; Data and Helpers

(define exact-locations
  (list
   (list 'baintree 51.878447 0.551177)
   (list 'cambridge 52.204968 0.182342)
   (list 'huntingdon 52.33081 -0.223216)))
  
(define rough-locations
  (list
   (list 'baintree 51.880447 0.551777)
   (list 'cambridge 52.2049685 0.18234)
   (list 'huntingdon 52.330981 -0.22316)))
  
(define (populate table)
  (define (add location)
    ((table 'insert!) (cadr location) (caddr location) (car location)))
  (map add exact-locations))

(define (lookup-names table locations)
  (define (lookup location)
    ((table 'lookup) (cadr location) (caddr location)))
  (define (desc-lookup location)
    (let ((result (lookup location)))
        (if result
            (prn (str "  Found location: " result))
            (prn (str "  Nothing found for: "
                      "(" (cadr location) ", " (caddr location) ")")))))
  (iter desc-lookup locations))


;;=========================================================
;; Book implementation of table

(define booktable (make-booktable))
(ignore (populate booktable))


(prn "" "Book implementaion - looking up EXACT coordinates:")
(lookup-names booktable exact-locations)

(prn "" "Book implementaion - looking up APPROXIMATE coordinates:")
(lookup-names booktable rough-locations)

(prn "" "===========================================================")

;;=========================================================
;; New implementation of table - using eqaual?

(define table-eq (make-table equal?))
(ignore (populate table-eq))


(prn "" "Using 'equal?' - looking up EXACT coordinates:")
(lookup-names table-eq exact-locations)

(prn "" "Using 'equals?' - looking up APPROXIMATE coordinates:")
(lookup-names table-eq rough-locations)

(prn "" "===========================================================")

;;=========================================================
;; New implementaio of table - using close?

(define (close? a b)
  (< (abs (- a b)) 0.01))

(define table (make-table close?))
(ignore (populate table))


(prn "" "Using 'close?' - looking up EXACT coordinates:")
(lookup-names table exact-locations)

(prn "" "Using 'close?' - looking up APPROXIMATE coordinates:")
(lookup-names table rough-locations)

(prn "" "===========================================================")

(--end-- "3.24")

