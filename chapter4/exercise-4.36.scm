#lang sicp

(#%require "common.scm")

;   Exercise 4.36
;   =============
;   
;   Exercise [3.69] discussed how to generate the stream of all Pythagorean
;   triples, with no upper bound on the size of the integers to be searched.
;   Explain why simply replacing an-integer-between by
;   an-integer-starting-from in the procedure in exercise [4.35] is not an
;   adequate way to generate arbitrary Pythagorean triples.  Write a
;   procedure that actually will accomplish this.  (That is, write a
;   procedure for which repeatedly typing try-again would in principle
;   eventually generate all Pythagorean triples.)
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.36]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.36
;   [Exercise 3.69]: http://sicp-book.com/book-Z-H-28.html#%_thm_3.69
;   [Exercise 4.35]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.35
;   4.3.1 Amb and Search - p417
;   ------------------------------------------------------------------------

(-start- "4.36")

(println "
The 'simple' solution would probably be:

  (define (a-pythagorean-triple)
    (let ((i (an-integer-starting-from 1)))
      (let ((j (an-integer-starting-from i)))
        (let ((k (an-integer-starting-from j)))
          (require (= (+ (* i i) (* j j)) (* k k)))
          (list i j k)))))

This is not an adequate solution because it will run indefinitely without
returning a value.  Each time 'require' fails k be be incremented but i and
j will always have the value 1.

To avoid this we can reverse the let statements and use k as an upper bound
for i and j.

  (define (a-pythagorean-triple)
    (let ((k (an-integer-starting-from 1)))
      (let ((j (an-integer-between 1 k)))
        (let ((i (an-integer-between 1 j)))))
          (require (= (+ (* i i) (* j j)) (* k k)))
          (list i j k)))))

")



(--end-- "4.36")

