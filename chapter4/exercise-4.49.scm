#lang sicp

(#%require "common.scm")

;   Exercise 4.49
;   =============
;   
;   Alyssa P. Hacker is more interested in generating interesting sentences
;   than in parsing them.  She reasons that by simply changing the procedure
;   parse-word so that it ignores the "input sentence" and instead always
;   succeeds and generates an appropriate word, we can use the programs we
;   had built for parsing to do generation instead.  Implement Alyssa's
;   idea, and show the first half-dozen or so sentences generated.⁽⁵⁴⁾
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.49]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.49
;   [Footnote 54]:   http://sicp-book.com/book-Z-H-28.html#footnote_Temp_628
;   4.3.2 Examples of Nondeterministic Programs - p426
;   ------------------------------------------------------------------------

(-start- "4.49")

(println"
    The professor lectures to a student by a professor by a class.

    A student lectures with the cat with a class in a professor.

    The cat sleeps with the cat in a class by a cat.

    A professor sleeps with the class in the student with a student.

    The professor studies in the professor in a professor by the professor.

    A class eats for a cat in the class for the student.
")

(define (require p)
  (if (not p) (amb)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define (random-word word-list)
  (let ((words (cdr word-list)))
    (list-ref words (random (length words)))))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (random-word word-list))))

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

(--end-- "4.49")

