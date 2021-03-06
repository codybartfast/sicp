#lang sicp

(#%require "common.scm")

;   Exercise 3.51
;   =============
;   
;   In order to take a closer look at delayed evaluation, we will use the
;   following procedure, which simply returns its argument after printing
;   it:
;   
;   (define (show x)
;     (display-line x)
;     x)
;   
;   What does the interpreter print in response to evaluating each
;   expression in the following sequence?⁽⁵⁹⁾
;   
;   (define x (stream-map show (stream-enumerate-interval 0 10)))
;   (stream-ref x 5)
;   (stream-ref x 7)
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.51]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.51
;   [Footnote 59]:   http://sicp-book.com/book-Z-H-24.html#footnote_Temp_453
;   3.5.1 Streams Are Delayed Lists - p325
;   ------------------------------------------------------------------------

(-start- "3.51")
(prn "What is displayed will depend on whether there is memoization.  If
there is no memoization then I'd expect:

(stream-ref x 5)
0
1
2
3
4
5

(stream-ref x 7)
0
1
2
3
4
5
6
7

With memoization:

(stream-ref x 5)
0
1
2
3
4
5

(stream-ref x 7)
6
7
")

(--end-- "3.51")

