#lang sicp

(#%require "common.scm")

;   Exercise 3.25
;   =============
;   
;   Generalizing one- and two-dimensional tables, show how to implement a
;   table in which values are stored under an arbitrary number of keys and
;   different values may be stored under different numbers of keys.  The
;   lookup and insert! procedures should take as input a list of keys used
;   to access the table.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.25]: http://sicp-book.com/book-Z-H-22.html#%_thm_3.25
;   3.3.3 Representing Tables - p272
;   ------------------------------------------------------------------------

(-start- "3.25")

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup table keys)
      (if (null? keys)
          (error "Was given NULL keys"))
      (let ((entry (assoc (car keys) (cdr table))) 
            (next-keys (cdr keys)))
        (if  entry
             ; if next-keys is empty then at a 'leaf'
             ;   otherwise we have a subtable
             (if (null? next-keys)
                 (cdr entry)
                 (lookup entry next-keys))
             false)))
    (define (insert! table keys value)
      (if (null? keys)
          (error "Was given NULL keys"))          
      (let ((entry (assoc (car keys) (cdr table)))
            (key (car keys))
            (next-keys (cdr keys)))
        (if (null? next-keys)
            ; at 'leaf'
            (if entry
                (set-cdr! entry value)
                (set-cdr! table
                          (cons (cons key value)
                                 (cdr table))))
            ;handle a subltable
            (if entry 
                (insert! entry next-keys value)
                (begin
                  (set-cdr! table
                            (cons (list key)
                                  (cdr table)))
                  (insert! (cadr table) next-keys value)))))
      'ok)
         
    (define (dispatch m)
      (cond ((eq? m 'lookup)
             (lambda (keys) (lookup local-table keys)))
            ((eq? m 'insert!)
             (lambda (keys value) (insert! local-table keys value)))
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))



(define table (make-table))

(prn "" "Adding 'apple and 'ant under single key.")  
(define key1a '('a))
(define key1b '(1))
(ignore ((table 'insert!) key1a 'apple))
(ignore ((table 'insert!) key1b 'ant))


(prn "" "Adding 'banana and 'bull under double keys.")  
(define key2a '('b 2))
(define key2b '('bb 22))
(ignore ((table 'insert!) key2a 'banana))
(ignore ((table 'insert!) key2b 'bull))


(prn "" "Adding 'cherry and 'cat under tripple  keys.")  
(define key3a '('c 2 'same))
(define key3b '('c 22 'same))
(ignore ((table 'insert!) key3a 'cherry))
(ignore ((table 'insert!) key3b 'cat))


(prn "" "Retrieving values:"
     ((table 'lookup) key1a)
     ((table 'lookup) key1b)
     ((table 'lookup) key2a)
     ((table 'lookup) key2b)
     ((table 'lookup) key3a)
     ((table 'lookup) key3b))

(--end-- "3.25")

