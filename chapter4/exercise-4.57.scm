#lang sicp

(#%require "common.scm")

;   Exercise 4.57
;   =============
;   
;   Define a rule that says that person 1 can replace person 2 if either
;   person 1 does the same job as person 2 or someone who does person 1's
;   job can also do person 2's job, and if person 1 and person 2 are not the
;   same person. Using your rule, give queries that find the following:
;   
;   a.  all people who can replace Cy D. Fect;
;   
;   b.  all people who can replace someone who is being paid more than they
;   are, together with the two salaries.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.57]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.57
;   4.4.1 Deductive Information Retrieval - p450
;   ------------------------------------------------------------------------

(-start- "4.57")

(println "
(rule (can-replace ?person-a ?person-b)
      (and (job ?person-a ?title-a)
           (job ?person-b ?title-b)
           (not (same ?person-a ?person-b))
           (or (same ?title-a ?title-b)
               (can-do-job ?title-a ?title-b))))

    a. (can-replace ?replacing (Fect Cy D))

    b. (and (can-replace ?person-a ?person-b)
            (salary ?person-a ?salary-a)
            (salary ?person-b ?salary-b)
            (lisp-value < ?salary-a ?salary-b))
               
")             

(--end-- "4.57")

