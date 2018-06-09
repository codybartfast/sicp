#lang sicp

(#%require "common.scm")

;   Exercise 3.39
;   =============
;   
;   Which of the five possibilities in the parallel execution shown above
;   remain if we instead serialize execution as follows:
;   
;   (define x 10)
;   
;   (define s (make-serializer))
;   
;   (parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
;                     (s (lambda () (set! x (+ x 1)))))
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.39]: http://sicp-book.com/book-Z-H-23.html#%_thm_3.39
;   3.4.2 Mechanisms for Controlling Concurrency - p306
;   ------------------------------------------------------------------------

(-start- "3.39")

(prn "This appears to have a deliberate mistake in the first call to set! is
not protected by the serializer.  Presumably this allows stuff to happen
between the evaluation of the argument and the setting of the x.

So possible values would be 101, 121 and 100.

The first two are two expected values.  100 could occur thus:

(* x x)         (+ x 1)
=======         =======
(s lambda...)
                (s lambda...)
set!


Looking at other answers, (e.g. kharthikk here
http://community.schemewiki.org/?sicp-ex-3.39),I missed the possibility of
11. This is possible becasuse once the top (s ...) is complete there is
nothing to stop the first call interleaving with the second call even
thought the second call is serialized.


(* x x)         (+ x 1)
=======         =======
(s lambda...)
                (s lambda...
                    (+ x 1)
set!
                    set!
                )
")
(--end-- "3.39")

