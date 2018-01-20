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

(prn
"Felt like implementing it rather than describing it. Here made the
multi-dimensional table generic and passed in the functions that touch the
underlying data store.

One difference in the binary tree used here, vs the binary tree in 2.66 is
that the in 2.66 it was the values in the tree that were ordered.  Here
though the values, (i.e., records) are ordred by their key is a property
of the value.  In this case 'car' is function to get the key from the
recored.
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; one-dimensional binary tree

(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define make-tree  list)
(define (new-tree) '())
  
(define (lookup-tree key get-key tree)
  (if (null? tree)
      #false
      (let* ((record (entry tree))
             (record-key (get-key record)))
        (cond ((= key record-key) record)
              ((< key record-key)
               (lookup-tree key get-key (left-branch tree)))
              ((> key record-key)
               (lookup-tree key get-key (right-branch tree)))))))

(define (adjoin-tree get-key record tree)
  (define new-key (get-key record))
  (cond ((null? tree) (make-tree record '() '()))
        ((= new-key (get-key (entry tree))) tree)
        ((< new-key (get-key (entry tree)))
         (make-tree
          (entry tree) 
          (adjoin-tree get-key record (left-branch tree))
          (right-branch tree)))
        ((> new-key (get-key (entry tree)))
         (make-tree
          (entry tree)
          (left-branch tree)
          (adjoin-tree get-key record (right-branch tree))))))

(define (test-tree)
   (let* (
          (jane (list 2 "Jane Jigs" 45 "Brimingham"))
          (mick (list 4 "Mick Muck" 16 "Hastings"))
          (pete (list 1 "Peter Puds" 23 "Manchester"))
          (anne (list 3 "Anne Ack" 19 "Coventry"))
          (fu   (list 5 "Fu Manchu" 189 "Windsor"))

         (get-key (lambda (record) (car record)))

         (make-test-tree (lambda (items)
                  (define (iter items set)
                    (if (null? items) set
                        (iter
                         (cdr items)
                         (adjoin-tree get-key (car items) set)))) 
                  (iter items (new-tree))))
         
         (db (make-test-tree (list pete jane anne mick fu))))
     
     (display (lookup-tree 3 get-key db))))
;(test-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; binary table

(define (try-get-record-binary table key)
  (lookup-tree key car (cdr table)))

(define (get-record-binary table key)
  (let ((entry (try-get-record-binary table key)))
    (cond (entry entry)
           (else
            (let ((record (list key)))
              (set-cdr! table
                        (adjoin-tree car record (cdr table)))
              record)))))

(define update-record-binary set-cdr!)
    
(define (make-table-binary)
  (make-table-generic try-get-record-binary
                      get-record-binary
                      update-record-binary))

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

(define update-record-assoc set-cdr!)

    
(define (make-table-assoc)
  (make-table-generic try-get-record-assoc
                      get-record-assoc
                      update-record-assoc))

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


(define (test-table make-table)
  (define table (make-table))

  (prn "Adding 'apple and 'ant under single key.")  
  (define key1a '(1))
  (define key1b '(100))
  (ignore ((table 'insert!) key1a 'apple))
  (ignore ((table 'insert!) key1b 'ant))

  (prn "Adding 'banana and 'bull under double keys.")  
  (define key2a '(200 2))
  (define key2b '(202 22))
  (ignore ((table 'insert!) key2a 'banana))
  (ignore ((table 'insert!) key2b 'bull))

  (prn "Adding 'cherry and 'cat under tripple  keys.")  
  (define key3a '(3000 2 1))
  (define key3b '(3000 22 1))
  (ignore ((table 'insert!) key3a 'cherry))
  (ignore ((table 'insert!) key3b 'cat))

  (prn "" "Retrieving values:"
       (str "   " ((table 'lookup) key1a))
       (str "   " ((table 'lookup) key1b))
       (str "   " ((table 'lookup) key2a))
       (str "   " ((table 'lookup) key2b))
       (str "   " ((table 'lookup) key3a))
       (str "   " ((table 'lookup) key3b))))

(prn "" "ASSOC table")
(prn "-----------")
(test-table make-table-assoc)
(prn "" "BINARY table")
(prn    "------------")
(test-table make-table-binary)

(--end-- "3.26")

