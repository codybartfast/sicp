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

(println
 "
The Great Rule
==============

 (rule ((great . ?rel) ?g ?ggs)
       (or
        (and (same ?rel (grandson))
             (grandson ?i ?ggs)
             (son-of ?g ?i))
        (and (same ?rel (great . ?rest))
             ((great . ?rest) ?i ?ggs)
             (son-of ?g ?i)))))

Using query system from section 4.4.4
=====================================
Paste the following in the prompt:

(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))
(assert!
  (rule (grandson ?grand-parent ?grand-son)
          (and (son-of ?parent ?grand-son)
               (son-of ?grand-parent ?parent))))
(assert!
  (rule (son-of ?parent ?son)
          (or (son ?parent ?son)
              (and (son ?m ?son)
                   (wife ?parent ?m)))))

(assert!
  (rule (same ?x ?x)))

(assert!                 
  (rule ((great . ?rel) ?g ?ggs)
    (or
      (and (same ?rel (grandson))
           (grandson ?i ?ggs)
           (son-of ?g ?i))
      (and (same ?rel (great . ?rest))
           ((great . ?rest) ?i ?ggs)
           (son-of ?g ?i)))))


((great great grandson) Adam ?who)

((great grandson) ?g ?ggs)

(?rel Adam Jabal)


Output:
=======

  ;;; Query input:
  ((great great grandson) Adam ?who)

  ;;; Query results:
  ((great great grandson) Adam Mehujael)


  ;;; Query input:
  ((great grandson) ?g ?ggs)

  ;;; Query results:
  ((great grandson) Mehujael Jubal)
  ((great grandson) Mehujael Jabal)
  ((great grandson) Irad Lamech)
  ((great grandson) Enoch Methushael)
  ((great grandson) Cain Mehujael)
  ((great grandson) Adam Irad)


  ;;; Query input:
  (?rel Adam Jabal)

  ;;; Query results:
  ((great great great great great grandson) Adam Jabal)
")

(#%require "query-system-71.scm")
(query-driver-loop)

(--end-- "4.69")

