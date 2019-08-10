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
  (let ((mary 'mary)
        (gabrielle 'gabrielle)
        (lorna 'lorna)
        (rosalind 'rosalind)
        (melissa 'melissa))  
    (define (amb-of-names) (amb mary gabrielle lorna rosalind melissa))

    (let ((moore-daughter (amb-of-names)))
      (require (equal? moore-daughter mary))
      ;; remove if mary's last name is unknown

      (let ((moore-yacht (amb-of-names)))
        (require (equal? moore-yacht lorna))
        (require (not (equal? moore-daughter moore-yacht)))

        (let ((barnacle-daughter (amb-of-names)))
          (require (equal? barnacle-daughter melissa))
          (let ((barnacle-yacht (amb-of-names)))
            (require (equal? barnacle-yacht gabrielle))
            (require (not (equal? barnacle-daughter barnacle-yacht)))

            (let ((downing-daughter (amb-of-names))
                  (downing-yacht (amb-of-names)))
              (require (equal? downing-yacht melissa))
              (require (not (equal? downing-daughter downing-yacht)))

              (let ((hall-daughter (amb-of-names))
                    (hall-yacht (amb-of-names)))
                (require (equal? hall-yacht rosalind))
                (require (not (equal? hall-daughter hall-yacht)))

                (let ((parker-daughter (amb-of-names))
                      (parker-yacht (amb-of-names)))
                  (require (not (equal? parker-daughter parker-yacht)))
                  (require (distinct?
                            (list moore-daughter downing-daughter
                                  hall-daughter barnacle-daughter
                                  parker-daughter)))
                  (require (distinct?
                            (list moore-yacht downing-yacht
                                  hall-yacht barnacle-yacht
                                  parker-yacht)))
                  (require (or (and (equal? moore-daughter gabrielle)
                                    (equal? moore-yacht parker-daughter))
                               (and (equal? downing-daughter gabrielle)
                                    (equal? downing-yacht parker-daughter))
                               (and (equal? hall-daughter gabrielle)
                                    (equal? hall-yacht parker-daughter))
                               (and (equal? barnacle-daughter gabrielle)
                                    (equal? barnacle-yacht parker-daughter))
                               (and (equal? parker-daughter gabrielle)
                                    (equal? parker-yacht parker-daughter))))
                  (println
                   (list (list 'moore moore-daughter moore-yacht)
                         (list 'downing downing-daughter downing-yacht)
                         (list 'hall hall-daughter hall-yacht)
                         (list 'barnacle barnacle-daughter barnacle-yacht)
                         (list 'parker parker-daughter parker-yacht)))
                  ;(amb) ;; uncomment to print all solutions
                  )))))))))

(daughters-and-yachts)    

(println "
Lorna's father is Colonel Downing.  If Mary's family name is unknown then
there are two solutions with Dr. Parker  being the alternative father.
")
(--end-- "4.43")

