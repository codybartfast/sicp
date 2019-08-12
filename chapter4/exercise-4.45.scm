#lang sicp

(#%require "common.scm")

;   Exercise 4.45
;   =============
;   
;   With the grammar given above, the following sentence can be parsed in
;   five different ways: "The professor lectures to the student in the class
;   with the cat." Give the five parses and explain the differences in
;   shades of meaning among them.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.45]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.45
;   4.3.2 Examples of Nondeterministic Programs - p425
;   ------------------------------------------------------------------------

(-start- "4.45")

(println "
Unlike the previous exercises, back tracking doesn't seem to work properly,
probably somehting to do with use of set! as alluded to in the text.  As
it looks like we're adding amb to our (metacircular) evaluator in the next
section, I might revisit this if/when I get amb working in that environment.


1)  She lectures in the class and she lectures with the cat:

        The professor lectures           to the student
                            in the class
                            with the cat

2)  She lectures in the class, it is the class with the cat:

        The professor lectures           to the student
                            in the class
                                with the cat

3)  She lecture with the cat, the student is in the class:

        The professor lectures           to the student
                            with the cat            in the class


4)  The student is in the class, and he is with the cat:

        The professor lectures           to the student
                                                    in the class
                                                    with the cat

5)  The student is in the class, it is the class with the cat:

        The professor lectures           to the student
                                                    in the class
                                                        with the cat
")


(define (require p)
  (if (not p) (amb)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    ;(println sent)
    ;(amb)
    sent))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(parse '(the professor lectures to the student in the class with the cat))



(--end-- "4.45")

