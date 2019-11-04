#lang sicp

(#%require "common.scm")

;   Exercise 4.58
;   =============
;   
;   Define a rule that says that a person is a "big shot" in a division if
;   the person works in the division but does not have a supervisor who
;   works in the division.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.58]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.58
;   4.4.1 Deductive Information Retrieval - p450
;   ------------------------------------------------------------------------

(-start- "4.58")

(println
 "
Rule:

 (assert! (rule (bigshot ?bshot ?division)
                (and (job ?bshot (?division . ?bshot-rest))    
                     (not (and (supervisor ?bshot ?boss)          
                               (job ?boss (?division . ?boss-rest)))))))

roduces:

 ;;; Query results:
 (bigshot (Scrooge Eben) accounting)
 (bigshot (Warbucks Oliver) administration)
 (bigshot (Bitdiddle Ben) computer)

Using query system from section 4.4.4
=====================================
Paste the following in the prompt:
")

(define extra-data
  '(

    (assert!
     (rule (bigshot ?bshot ?division)
           (and (job ?bshot (?division . ?bshot-rest))    
                (not (and (supervisor ?bshot ?boss)          
                          (job ?boss (?division . ?boss-rest)))))))
    ))

(#%require "query-system-71.scm")
(#%require "query-microshaft.scm")

(ignore (map println microshaft))
(println)
(ignore (map println extra-data))
(println)
(println "(bigshot ?a ?b)")

(query-driver-loop)


(--end-- "4.58")

