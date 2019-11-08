#lang sicp

(#%require "common.scm")

;   Exercise 4.63
;   =============
;   
;   The following data base (see Genesis 4) traces the genealogy of the
;   descendants of Ada back to Adam, by way of Cain:
;   
;   (son Adam Cain)
;   (son Cain Enoch)
;   (son Enoch Irad)
;   (son Irad Mehujael)
;   (son Mehujael Methushael)
;   (son Methushael Lamech)
;   (wife Lamech Ada)
;   (son Ada Jabal)
;   (son Ada Jubal)
;   
;   Formulate rules such as "If S is the son of F, and F is the son of G,
;   then S is the grandson of G" and "If W is the wife of M, and S is the
;   son of W, then S is the son of M" (which was supposedly more true in
;   biblical times than today) that will enable the query system to find the
;   grandson of Cain; the sons of Lamech; the grandsons of Methushael. (See
;   exercise [4.69] for some rules to deduce more complicated
;   relationships.)
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.63]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.63
;   [Exercise 4.69]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.69
;   4.4.1 Deductive Information Retrieval - p453
;   ------------------------------------------------------------------------

(-start- "4.63")

(println
 "
Rules:
======

  (rule (grandson ?grand-parent ?grand-son)
        (and (son-of ?parent ?grand-son)
             (son-of ?grand-parent ?parent)))
  
  (rule (son-of ?parent ?son)
        (or (son ?parent ?son)
            (and (son ?mother ?son)
                 (wife ?parent ?mother))))


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

  (grandson Cain ?gs)

  (son-of Lamech ?s)

  (grandson Methushael ?gs)


Output:
=======

  ;;; Query input:
  (grandson Cain ?gs)

  ;;; Query results:
  (grandson Cain Irad)
  
  ;;; Query input:
  (son-of Lamech ?s)

  ;;; Query results:
  (son-of Lamech Jubal)
  (son-of Lamech Jabal)
  
  ;;; Query input:
  (grandson Methushael ?gs)
  
  ;;; Query results:
  (grandson Methushael Jubal)
  (grandson Methushael Jabal)
")

(#%require "query-system-71.scm")
(query-driver-loop)

(--end-- "4.63")

