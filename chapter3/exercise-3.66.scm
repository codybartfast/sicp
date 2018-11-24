#lang sicp

(#%require "common.scm")

;   Exercise 3.66
;   =============
;   
;   Examine the stream (pairs integers integers). Can you make any general
;   comments about the order in which the pairs are placed into the stream?
;   For example, about how many pairs precede the pair (1,100)? the pair
;   (99,100)? the pair (100,100)? (If you can make precise mathematical
;   statements here, all the better. But feel free to give more qualitative
;   answers if you find yourself getting bogged down.)
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.66]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.66
;   3.5.3 Exploiting the Stream Paradigm - p341
;   ------------------------------------------------------------------------

(-start- "3.66")

(prn
 "My Theory:
==========
Generally, if to get to item (r c) we will need to enumerate 1 + (c - r)
items in row r.  In enumerating items in that row we will enumerate the same
number of items in lower rows (2 * (1 + c - r) items so far).  But to get
these items we will have had to enumerate as many items in the row above and
twice as many in the one above that, also for each new row we need to
enumerate the 'corner'.  So total is:
    (2 ^ r)(2 * (1 + c + r)) + r = (2 ^ (r + 1))(1 + c + r) + r
In which case
    (1, 100) => r = 1, c = 100
    (2^1)(1+100-1)+1
    201 (200 preceeding)

    (99, 100) => r = 99, c = 100
    (2^99)(2)+99
    (2^100)+98 preceeding

    (100, 100) => r = 100, c = 100
    (2^100)(1)+100
    (2^100)+99 preceeding
")

(--end-- "3.66")

