#lang sicp

(#%require "common.scm")

;   Exercise 4.14
;   =============
;   
;   Eva Lu Ator and Louis Reasoner are each experimenting with the
;   metacircular evaluator.  Eva types in the definition of map, and runs
;   some test programs that use it.  They work fine.  Louis, in contrast,
;   has installed the system version of map as a primitive for the
;   metacircular evaluator.  When he tries it, things go terribly wrong. 
;   Explain why Louis's map fails even though Eva's works.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.14]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.14
;   4.1.4 Running the Evaluator as a Program - p384
;   ------------------------------------------------------------------------

(-start- "4.14")

(println "
I believe the problem is that Louis is 'crossing the streams'.  He is
passing one of 'our' procedures to a primitive procedure.

We have a procedure, cube, that is designed to be applied in the
implementation that we are constructing.  But here we are passing cube (i.e.
the procedure object referenced by the symbol 'cube) to the underlying map.
So it will be the underlying implementation (e.g. Racket, Guile, Chicken,
...) that will apply map and, in turn, attempt to apply our cube procedure.
This cannot work if our implementation of procedures, environments, etcetera
is different from the ones used by the underlying implementation.

(If key parts of the underlying implementation were identical to our
implementation I can imagine that it might work, but clearly we should never
rely on that.)

So if we define our own map we get:

;;; M-Eval input:
(define (cube x) (* x x x))

;;; M-Eval value:
#<void>

;;; M-Eval input:
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

;;; M-Eval value:
#<void>

;;; M-Eval input:
(map cube (list 1 2 3))

;;; M-Eval value:
(1 8 27)

However if we use the underlying map (by using ea-data-drive-14 as the
implementation instead of ea-data-drive-12) then we get an error as 'our'
cube procedure is not recognized as a procedure by the metacircular
evaluator's lisp:

;;; M-Eval input:
(define (cube x) (* x x x))

;;; M-Eval value:
#<void>

;;; M-Eval input:
(map cube (list 1 2 3))
xxx X application: not a procedure;
 expected a procedure that can be applied to arguments
  given: #0=(mcons 'procedure (mcons (mcons 'x '()) (mcons (mcons (mcons '*
      (mcons 'x (mcons 'x (mcons 'x '())))) '()) (mcons (mcons (mcons
      '*frame* (mcons (mcons 'cube #0#) (mcons (mcons 'false #f) (mcons
      (mcons 'true #t) (mcons (mcons 'map #<procedure:mm...
  arguments...:

>>>>> end quote >>>>>

Now starting driver-loop, have fun! ...")

(#%require "ea-data-directed-12.scm")
;(#%require "ea-data-directed-14.scm")
(put-evaluators)

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(driver-loop)

(--end-- "4.14")
