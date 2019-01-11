#lang sicp

(#%require "common.scm")

;   Exercise 3.68
;   =============
;   
;   Louis Reasoner thinks that building a stream of pairs from three parts
;   is unnecessarily complicated.  Instead of separating the pair (S₀,T₀)
;   from the rest of the pairs in the first row, he proposes to work with
;   the whole first row, as follows:
;   
;   (define (pairs s t)
;     (interleave
;      (stream-map (lambda (x) (list (stream-car s) x))
;                  t)
;      (pairs (stream-cdr s) (stream-cdr t))))
;   
;   Does this work?  Consider what happens if we evaluate (pairs integers
;   integers) using Louis's definition of pairs.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.68]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.68
;   3.5.3 Exploiting the Stream Paradigm - p341
;   ------------------------------------------------------------------------

(-start- "3.68")

(prn "My Hypothesis:
==============
I feel I'm walking into a trap by saying I think Louis's code would work
'after a fashion'.  I think every pair of i, j would be produced eventually
but the order would quite 'asymetric.  E.g. (4,4) would not be generated
until after (1,32).  So (ignoring trivial cases) you can't stop the sequence
at some point where have all pairs i<=j and only pairs i<=j.  Because by the
the time you get i=j you will also have (1, j+1) etc...

Looking at other folks' answers (e.g., Kana) this is (of course) only the
lesser evil.  The bigger problem is the absence of 'cons-stream' so that
Louis's code has no delayed evaluation and so, with an infinite streams it
would just recurse indefinitely.")

(--end-- "3.68")

