#lang sicp

(#%require "common.scm")

;   Exercise 4.19
;   =============
;   
;   Ben Bitdiddle, Alyssa P. Hacker, and Eva Lu Ator are arguing about the
;   desired result of evaluating the expression
;   
;   (let ((a 1))
;     (define (f x)
;       (define b (+ a x))
;       (define a 5)
;       (+ a b))
;     (f 10))
;   
;   Ben asserts that the result should be obtained using the sequential rule
;   for define: b is defined to be 11, then a is defined to be 5, so the
;   result is 16.  Alyssa objects that mutual recursion requires the
;   simultaneous scope rule for internal procedure definitions, and that it
;   is unreasonable to treat procedure names differently from other names. 
;   Thus, she argues for the mechanism implemented in exercise [4.16].  This
;   would lead to a being unassigned at the time that the value for b is to
;   be computed.  Hence, in Alyssa's view the procedure should produce an
;   error.  Eva has a third opinion.  She says that if the definitions of a
;   and b are truly meant to be simultaneous, then the value 5 for a should
;   be used in evaluating b.  Hence, in Eva's view a should be 5, b should
;   be 15, and the result should be 20.  Which (if any) of these viewpoints
;   do you support?  Can you devise a way to implement internal definitions
;   so that they behave as Eva prefers?⁽²⁶⁾
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.19]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.19
;   [Exercise 4.16]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.16
;   [Footnote 26]:   http://sicp-book.com/book-Z-H-26.html#footnote_Temp_565
;   4.1.6 Internal Definitions - p391
;   ------------------------------------------------------------------------

(-start- "4.19")

(println "
Alyssa's of course :-)

  - Ben's approach is hamstrung by not supporting mutual recursion.
    (Although after seeing exercise 4.20 Ben's approach may be more
    practical than initially apparent because I quite like the idea of
    making mutal recursion explicit.)

  - Eva's approach has a purity to it, but the analysis required to support
    every kind of definition seems complicated.  One could also argue that
    mutual recursion of functions is a necessary complication that allows
    compact and expressive code.  Whereas having simple variables used
    before they are defined is generally a bad thing and the use of delay is
    available for the occassions it's necessary.

Implementing Eva's approach:
============================

Below is an example of how the desired result, 20, is attained by delaying
the evaluation of all the definition values.

Memoization is also added, not for performance, but but to ensure that the
values of the variables are constant between lookups.  E.g. we want b to
always have the same value even if the value of a changes.

Alternatively, the order of definitions could be reorderd. (If that is an
option then my argument above is the original code should be in that order
to make it more readable).

But it does seem analysis of code, whether for dependency analysis or for
rewriting a to (a) is very hard.  If you encounter (funky (a (+ a 1)) ...)
how do you know if funky is a varient of exp, let, set! or 'quote' but with
funky syntax? (funky a (+ a 1) ... could be quivalent to:
    (exp a (+ a 1))
    (let ((a (+ a 1))) ...)
    (set! a (+ a 1))
so funky can't be analysed merely by looking at the syntax.

--------

Manually rewriting the expression so it is evaluated as Eva prefers:
")


(#%require "ea-data-directed-19.scm")
(put-evaluators)

(define with-delay
  '(let ((a 1))
       (define (f x)
         (define b (delay (+ (a) x)))
         (define a (delay 5))
         (+ (a) (b)))
       (f 10)))

(define with-memo-delay
  '(begin

     (define (memo-proc proc)
       (let ((already-run? false) (result false))
         (lambda ()
           (if already-run?
               result
               (begin (set! result (proc))
                      (set! already-run? true)
                      result)))))

     (let ((a 1))
       (define (f x)
           (define b (memo-proc (delay (+ (a) x))))
           (define a (memo-proc (delay 5)))
           (+ (a) (b)))
       (f 10))))


(println
"Evaluating with delayed eval of definitions: "
(eval with-delay (setup-environment)))

(println
"Evaluating with memoized delayed eval of definitions: "
(eval with-memo-delay (setup-environment)))


(--end-- "4.19")

