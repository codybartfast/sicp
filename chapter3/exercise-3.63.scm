#lang sicp

(#%require "common.scm")

;   Exercise 3.63
;   =============
;   
;   Louis Reasoner asks why the sqrt-stream procedure was not written in the
;   following more straightforward way, without the local variable guesses:
;   
;   (define (sqrt-stream x)
;     (cons-stream 1.0
;                  (stream-map (lambda (guess)
;                                (sqrt-improve guess x))
;                              (sqrt-stream x))))
;   
;   Alyssa P. Hacker replies that this version of the procedure is
;   considerably less efficient because it performs redundant computation.
;   Explain Alyssa's answer.  Would the two versions still differ in
;   efficiency if our implementation of delay used only (lambda () <exp>)
;   without using the optimization provided by memo-proc (section [3.5.1])?
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.63]: http://sicp-book.com/book-Z-H-24.html#%_thm_3.63
;   [Section 3.5.1]: http://sicp-book.com/book-Z-H-24.html#%_sec_3.5.1
;   3.5.3 Exploiting the Stream Paradigm - p338
;   ------------------------------------------------------------------------

(-start- "3.63")

(prn
 "In the original implemenation the stream-map is always being applied to
the same structure, guesses, there is just one stream.

In Louis's implementation a new stream is created with each new guess.  This
is evidenced by the fact is that (cons-stream 1.0 ...) is called multiple
times.

At a naive level the n-th term of of the n-th stream is calculated from the
(n-1)-th item from the (n-1)-th stream so it looks like only n computations
are required. However before calculating that term we first have to
enumerate the (n-1)-th term of the n-th stream.  That requires n-1
computational steps but that result never used.  I.e. (n^2)/2 computational
steps are made in total.

Without memoization I think they would be equivalent. Each call to
stream-map would cause guesses to be re-evaluated.")

(--end-- "3.63")

