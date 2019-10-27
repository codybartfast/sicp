#lang sicp

(#%require "common.scm")

;   Exercise 4.69
;   =============
;   
;   Beginning with the data base and the rules you formulated in exercise
;   [4.63], devise a rule for adding "greats" to a grandson relationship.
;   This should enable the system to deduce that Irad is the great-grandson
;   of Adam, or that Jabal and Jubal are the
;   great-great-great-great-great-grandsons of Adam.  (Hint: Represent the
;   fact about Irad, for example, as ((great grandson) Adam Irad).  Write
;   rules that determine if a list ends in the word grandson.  Use this to
;   express a rule that allows one to derive the relationship ((great . 
;   ?rel) ?x ?y), where ?rel is a list ending in grandson.) Check your rules
;   on queries such as ((great grandson) ?g ?ggs) and (?relationship Adam
;   Irad).
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.69]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.69
;   [Exercise 4.63]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.63
;   4.4.3 Is Logic Programming Mathematical Logic? - p468
;   ------------------------------------------------------------------------

(-start- "4.69")



(--end-- "4.69")

