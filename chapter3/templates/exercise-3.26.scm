#lang sicp

(#%require "common.scm")

;   Exercise 3.26
;   =============
;   
;   To search a table as implemented above, one needs to scan through the
;   list of records.  This is basically the unordered list representation of
;   section [2.3.3].  For large tables, it may be more efficient to
;   structure the table in a different manner.  Describe a table
;   implementation where the (key, value) records are organized using a
;   binary tree, assuming that keys can be ordered in some way (e.g.,
;   numerically or alphabetically).  (Compare exercise [2.66] of chapter 2.)
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.26]: http://sicp-book.com/book-Z-H-22.html#%_thm_3.26
;   [Section 2.3.3]: http://sicp-book.com/book-Z-H-16.html#%_sec_2.3.3
;   [Exercise 2.66]: http://sicp-book.com/book-Z-H-22.html#%_thm_2.66
;   3.3.3 Representing Tables - p272
;   ------------------------------------------------------------------------

(-start- "3.26")

(prn "If going to implement this rather than just describe it, then suggest:
   1. redo tree 'test' below so that db is built up programatically using
      adjoin-set.
   2. But first fix adjoin-set - it was never run in this context and
      I think it constructs sets ordered sets rather than ordered records.
      I.e., it should probably be 'adjoin-record'.
   3. Create a few more tests to check above implementation.
   4. Create methods to support make-table-generic.
   5. If never want to get to Ex 3.27 then do something about
      rebalancing the tree.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 'assoc' table

(define (try-get-record-assoc table key)
  (assoc key (cdr table)))

(define (get-record-assoc table key)
  (let ((entry (try-get-record-assoc table key)))
    (cond (entry entry)
           (else
            (let ((record (list key)))
              (set-cdr! table
                        (cons record
                              (cdr table)))
              record)))))

(define (update-record-assoc record value)
  (set-cdr! record value))

    
(define (make-table-assoc)
  (make-table-generic try-get-record-assoc
                      get-record-assoc
                      update-record-assoc))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; binary table



(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
  
(define (lookup key get-key tree)
  (if (null? tree)
      #false
      (let* ((record (entry tree))
             (record-key (get-key record)))
        (cond ((= key record-key) record)
              ((> key record-key) (lookup key get-key (left-branch tree)))
              ((< key record-key) (lookup key get-key (right-branch tree)))))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))
((lambda ()
   (let ((pete '(1 "Peter Puds" 23 "Manchester"))
         (jane '(2 "Jane Jigs" 45 "Brimingham"))
         (anne '(3 "Anne Ack" 19 "Coventry"))
         (mick '(4 "Mick Muck" 16 "Hastings"))
         (fu   '(5 "Fu Manchu" 189 "Windsor")))
     (define db (list pete
                      (list mick
                            '() (list anne '() '()))
                      (list fu
                            (list jane '()) '())))
  
     (define (get-key record) (car record))

     (display (lookup 3 get-key db))))) ;(3 "Anne Ack" 19 Coventry))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; genric multi-dimensional table

(define (make-table-generic try-get-record get-record update-record)
  (let
      ((local-table (list '*table*)))
    (define (lookup table keys)
      (if (null? keys)
          (error "Was given NULL keys"))
      (let ((entry (try-get-record table (car keys))) 
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
      (let ((entry (get-record table (car keys)))
            (key (car keys))
            (next-keys (cdr keys)))
        (if (null? next-keys)
            ; at 'leaf' - entry is a record
            (update-record entry value)               
            ;handle a subltable
            (insert! entry next-keys value)))
      'ok)
         
    (define (dispatch m)
      (cond ((eq? m 'lookup)
             (lambda (keys) (lookup local-table keys)))
            ((eq? m 'insert!)
             (lambda (keys value) (insert! local-table keys value)))
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


(lambda ()
  (define table (make-table-assoc))

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
       ((table 'lookup) key3b)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                   




(--end-- "3.26")

