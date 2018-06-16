#lang sicp

(#%require "common.scm")

;   Exercise 3.40
;   =============
;   
;   Give all possible values of x that can result from executing
;   
;   (define x 10)
;   
;   (parallel-execute (lambda () (set! x (* x x)))
;                     (lambda () (set! x (* x x x))))
;   
;   Which of these possibilities remain if we instead use serialized
;   procedures:
;   
;   (define x 10)
;   
;   (define s (make-serializer))
;   
;   (parallel-execute (s (lambda () (set! x (* x x))))
;                     (s (lambda () (set! x (* x x x)))))
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.40]: http://sicp-book.com/book-Z-H-23.html#%_thm_3.40
;   3.4.2 Mechanisms for Controlling Concurrency - p306
;   ------------------------------------------------------------------------

(-start- "3.40")

(prn "Going to work on the assumption that each arguement to a function is
evaluated independently, even if it's the same expression, i.e. each 'x' is
evaluated seprately.

So I think the final value for x are:
100; 1,000; 10,000; 100,000; 1,000,000

With both 'sets' after all evaluations of x.
(* 10 10)
(* 10 10 10)
-> 100, 1,000

With first 'set' befor full evaulation of (* x x x)
(* 10 10)
(* 10 10 100)
-> 10,000

(* 10 10)
(* 10 100 100)
-> 100,000

(* 10 10)
(* 100 100 100)
-> 1,000,000

With secon 'set' before full evaluation of (* x x)
(* 10 1,000)
(* 10 10 10)
-> 10,000

(* 1,000 1,000)
(* 10 10 10)
-> 1,000,000

With the use of serialize only possible final value is 1,000,000 

(* 10 10)
(* 100 100 100)
-> 1,000,000

(* 1,000 1,000)
(* 10 10 10)
-> 1,000,000
")

(--end-- "3.40")

