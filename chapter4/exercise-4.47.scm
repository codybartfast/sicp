#lang sicp

(#%require "common.scm")

;   Exercise 4.47
;   =============
;   
;   Louis Reasoner suggests that, since a verb phrase is either a verb or a
;   verb phrase followed by a prepositional phrase, it would be much more
;   straightforward to define the procedure parse-verb-phrase as follows
;   (and similarly for noun phrases):
;   
;   (define (parse-verb-phrase)
;     (amb (parse-word verbs)
;          (list 'verb-phrase
;                (parse-verb-phrase)
;                (parse-prepositional-phrase))))
;   
;   Does this work?  Does the program's behavior change if we interchange
;   the order of expressions in the amb?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.47]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.47
;   4.3.2 Examples of Nondeterministic Programs - p425
;   ------------------------------------------------------------------------

(-start- "4.47")

(println "
No it won't work.  It results in infinite recursion.  When parse-verb-phrase
is evaluated in the second branch it is effectively the same as when it is
evaluated in the first branch because *unparsed* is exactly the same.  So if
the second branch is taken on the first call parse-verb-phrase then it will
be taken on every subsequent call to parse-verb-phrase.

If the operands are reordered then, essentially, the same thing happens,
except the recursion happens on the first branch.

This doesn't happen with the original code because the verb is removed from
*unused* before calling amb.
")

(--end-- "4.47")

