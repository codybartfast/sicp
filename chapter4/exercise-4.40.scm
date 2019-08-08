#lang sicp

(#%require "common.scm")

;   Exercise 4.40
;   =============
;   
;   In the multiple dwelling problem, how many sets of assignments are there
;   of people to floors, both before and after the requirement that floor
;   assignments be distinct?  It is very inefficient to generate all
;   possible assignments of people to floors and then leave it to
;   backtracking to eliminate them.  For example, most of the restrictions
;   depend on only one or two of the person-floor variables, and can thus be
;   imposed before floors have been selected for all the people. Write and
;   demonstrate a much more efficient nondeterministic procedure that solves
;   this problem based upon generating only those possibilities that are not
;   already ruled out by previous restrictions.  (Hint: This will require a
;   nest of let expressions.)
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.40]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.40
;   4.3.2 Examples of Nondeterministic Programs - p419
;   ------------------------------------------------------------------------

(-start- "4.40")

(println "
There are 5^5 or 3,125 sets of assignments before the distinct requirement
and 5! or 120 sets of assignments afterwards.
")

(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((fletcher (amb 1 2 3 4 5)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((miller (amb 1 2 3 4 5)))
        (require (> miller cooper))
        (let ((smith (amb 1 2 3 4 5)))
          (require (not (= (abs (- smith fletcher)) 1)))
          (let ((baker (amb 1 2 3 4 5)))
            (require (not (= baker 5)))
            (require
              (distinct? (list baker cooper fletcher miller smith)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))
    

(time (multiple-dwelling))

(--end-- "4.40")

