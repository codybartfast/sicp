#lang sicp

(#%require "common.scm")

;   Exercise 4.43
;   =============
;   
;   Use the amb evaluator to solve the following puzzle:⁽⁴⁹⁾ 
;   
;       Mary Ann Moore's father has a yacht and so has each of his four
;       friends: Colonel Downing, Mr. Hall, Sir Barnacle Hood, and Dr.
;       Parker.  Each of the five also has one daughter and each has
;       named his yacht after a daughter of one of the others.  Sir
;       Barnacle's yacht is the Gabrielle, Mr. Moore owns the Lorna; Mr.
;       Hall the Rosalind.  The Melissa, owned by Colonel Downing, is
;       named after Sir Barnacle's daughter.  Gabrielle's father owns
;       the yacht that is named after Dr. Parker's daughter.  Who is
;       Lorna's father? 
;   
;   Try to write the program so that it runs efficiently (see exercise
;   [4.40]).  Also determine how many solutions there are if we are not told
;   that Mary Ann's last name is Moore.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.43]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.43
;   [Exercise 4.40]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.40
;   [Footnote 49]:   http://sicp-book.com/book-Z-H-28.html#footnote_Temp_616
;   4.3.2 Examples of Nondeterministic Programs - p420
;   ------------------------------------------------------------------------

(-start- "4.43")

(define (require p)
  (if (not p) (amb)))            

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (daughters-and-yachts)
  (define mary 'mary)
  (define gabrielle 'gabrielle)
  (define lorna 'lorna)
  (define rosalind 'rosalind)
  (define melissa 'melissa)
  
  (let ((moore (amb mary gabrielle lorna rosalind melissa))
        (downing (amb mary gabrielle lorna rosalind melissa))
        (hall (amb mary gabrielle lorna rosalind melissa))
        (barnacle (amb mary gabrielle lorna rosalind melissa))
        (parker (amb mary gabrielle lorna rosalind melissa))
        (moore-yacht (amb mary gabrielle lorna rosalind melissa))
        (downing-yacht (amb mary gabrielle lorna rosalind melissa))
        (hall-yacht (amb mary gabrielle lorna rosalind melissa))
        (barnacle-yacht (amb mary gabrielle lorna rosalind melissa))
        (parker-yacht (amb mary gabrielle lorna rosalind melissa)))

    (require (distinct? (list moore downing hall barnacle parker)))
    (require (distinct? (list moore-yacht downing-yacht
                              hall-yacht barnacle-yacht parker-yacht)))
    
    (require (not (equal? moore moore-yacht)))
    (require (not (equal? downing downing-yacht)))
    (require (not (equal? hall hall-yacht)))
    (require (not (equal? barnacle barnacle-yacht)))
    (require (not (equal? parker parker-yacht)))
    
    (require (equal? moore mary))
    (require (equal? barnacle-yacht gabrielle))
    (require (equal? moore-yacht lorna))
    (require (equal? hall-yacht rosalind))
    (require (equal? downing-yacht melissa))
    (require (equal? barnacle melissa))
    (require (or (and (equal? moore gabrielle)
                      (equal? moore-yacht parker))
                 (and (equal? downing gabrielle)
                      (equal? downing-yacht parker))
                 (and (equal? hall gabrielle)
                      (equal? hall-yacht parker))
                 (and (equal? barnacle gabrielle)
                      (equal? barnacle-yacht parker))
                 (and (equal? parker gabrielle)
                      (equal? parker-yacht parker))))
    (println (list (list 'moore moore moore-yacht)
          (list 'downing downing downing-yacht)
          (list 'hall hall hall-yacht)
          (list 'barnacle barnacle barnacle-yacht)
          (list 'parker parker parker-yacht)))
    (amb)))

 (daughters-and-yachts)
    

(--end-- "4.43")

