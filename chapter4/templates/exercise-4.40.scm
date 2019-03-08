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



(--end-- "4.40")

