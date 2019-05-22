#lang sicp

(#%require "common.scm")

;   Exercise 4.26
;   =============
;
;   Ben Bitdiddle and Alyssa P. Hacker disagree over the importance of lazy
;   evaluation for implementing things such as unless.  Ben points out that
;   it's possible to implement unless in applicative order as a special
;   form. Alyssa counters that, if one did that, unless would be merely
;   syntax, not a procedure that could be used in conjunction with
;   higher-order procedures.  Fill in the details on both sides of the
;   argument.  Show how to implement unless as a derived expression (like
;   cond or let), and give an example of a situation where it might be
;   useful to have unless available as a procedure, rather than as a special
;   form.
;
;   ------------------------------------------------------------------------
;   [Exercise 4.26]: http://sicp-book.com/book-Z-H-27.html#%_thm_4.26
;   4.2.1 Normal Order and Applicative Order - p401
;   ------------------------------------------------------------------------

(-start- "4.26")

(println "
Ben is correct, 'unless' can be implemented so that it behaves as expected.
In particular that only one expression is evaluated.

As an example of how it could be used, consider an automated weeding machine
that has the ability to cut and collect a plant or to apply a liquid
fertilizer. Having identified an individual plant, a procedure crop-plant?
determines if that plant is part of the crop.

    (unless (crop-plant? plant)
        (cut-and-collect plant) ; remove weed
        (apply-liquid plant))   ; fertilize crop plant

If both expressions were evaluated then every plant, whether crop or weed
would be cut-and-collected.

Alyssa's point is that although 'unless' works, it would be useful if it
could be passed as a parameter so that we can change the behaviour of the
machine.  If 'unless' were replaced with 'when' (and the tank filled with
herbicide instead of fertilizer) then it could be used as a harvesting
machine:

    (when (crop-plant? plant)
        (cut-and-collect plant) ; harvest crop
        (apply-liquid plant))   ; apply herbicide to weed

If 'unless' and 'when' were procedures then they could be chosen
programatically and passed as arguments:

    (define (get-select)
        (if (harvesting?)
            when
            unless))

    (define (process-plant plant select)
        (select (crop-plant? plant)
                (cut-and-collect plant)
                (apply-liquid plant)))

    (process-plant plant (get-select))

This cannot work with Ben's implementation, there is no 'unless' procedure
object that can be returned from get-select or passed to process-plant.

But nor can we create these procedures with our current language because
apply will always evaluate the arguments to 'unless' before the body of
'unless' is evaluated, hence every plant will already be cut-and-collected
before we know whether if it is a weed or a crop.

So if we want features like 'unless' to be procedures then they need lazy
arguments so that an option is only evaluated if it is selected.

Meanwhile, Ben's approach can be implemented with:

unless:

    (define (analyze-unless exp)
      (analyze-if
       (make-if (unless-condition exp)
                (unless-exceptional-value exp)
                (unless-usual-value exp))))

when:

    (define (analyze-when exp)
      (analyze-if
       (make-if (when-condition exp)
                (when-usual-value exp)
                (when-exceptional-value exp))))

Which is demonstrated in the code below.
")


(#%require "ea-analyzing-26.scm")
(put-analyzers)

(define prog
  '(begin

     (define (cut-and-collect plant)
       (println "cutting plant...")
       'collected)

     (define (apply-liquid plant)
       (println "applying liquid...")
       'liquid-applied)

     (define (crop-plant? plant) true)
     (define plant 'unused-data)

     (println "Expect to apply liquid to crop when not harvesting:")
     (unless (crop-plant? plant)
       (cut-and-collect plant)
       (apply-liquid plant))

     (println)
     (println "Expect to cut and collect crop during harvest:")
     (when (crop-plant? plant)
	(cut-and-collect plant)
	(apply-liquid plant))

     ))


(println "Evaluating expression:
    " prog "
")

(eval
 prog
 the-global-environment)


(--end-- "4.26")

