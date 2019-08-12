#lang sicp

(#%require "common.scm")

;   Exercise 4.46
;   =============
;   
;   The evaluators in sections [4.1] and [4.2] do not determine what order
;   operands are evaluated in. We will see that the amb evaluator evaluates
;   them from left to right. Explain why our parsing program wouldn't work
;   if the operands were evaluated in some other order.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.46]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.46
;   [Section 4.1]:   http://sicp-book.com/book-Z-H-26.html#%_sec_4.1
;   [Section 4.2]:   http://sicp-book.com/book-Z-H-27.html#%_sec_4.2
;   4.3.2 Examples of Nondeterministic Programs - p425
;   ------------------------------------------------------------------------

(-start- "4.46")

(println "
I currently can't see any reason why it wouldn't work.  I've tried changing
the order of the operands to amb, so that they get evaluated in a different
order, and it still produces a valid result.

My best guess it has an undesirable affect on back tracking.  Perhaps having
amb fail when enumerating the values of an outer amb causes stops its peers
being used.  e.g.: suppose evaluating operands right-to-left and the second
maybe-extend fails, then perhaps that causes an amb higher up the tree to
advance preventing noun-phrase being returned.

  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (...

My read the question as referring to the order that the operands to amb are
evaluated.  Clearly if the operands to:

  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase))

were evaluated right-to-left then you've got problems as parse-verb-phrase
would fail if it's called before parse-noun-phrase.
")
(--end-- "4.46")

