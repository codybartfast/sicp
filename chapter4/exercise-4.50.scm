#lang sicp

(#%require "common.scm")

;   Exercise 4.50
;   =============
;   
;   Implement a new special form ramb that is like amb except that it
;   searches alternatives in a random order, rather than from left to right.
;   Show how this can help with Alyssa's problem in exercise [4.49].
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.50]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.50
;   [Exercise 4.49]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.49
;   4.3.3 Implementing the <tt>Amb</tt> Evaluator - p436
;   ------------------------------------------------------------------------

(-start- "4.50")

(println "
I believe Alyssa's problem can be restated as saying that all the generated
sentences have the same structure, ramb can be used to find sentences with
different structures.

ramb can be implemented with:

  (define (analyze-ramb exp)
    (analyze
     (make-amb (randomize-list (amb-choices exp)))))

Then, by changing 'amb' to 'ramb' in the parsing code we will attempt to
match extended expresions for verb and noun phrases half the time instead of
always attempting to just match a shorter phrase first.

i.e.:

  (define (maybe-extend noun-phrase)
    (ramb noun-phrase
          (maybe-extend (...

and

  (define (maybe-extend noun-phrase)
    (ramb noun-phrase
          (maybe-extend (...

This gives us sentences with this structure:

  (sentence
   (noun-phrase
    (simple-noun-phrase (article the) (noun professor))
    (prep-phrase
     (prep with)
     (noun-phrase
      (simple-noun-phrase (article a) (noun cat))
      (prep-phrase
       (prep in)
       (noun-phrase
        (simple-noun-phrase (article a) (noun student))
        (prep-phrase (prep for) (simple-noun-phrase (article a) (noun cl ...
   (verb studies))

As well as the previous structure:

  (sentence
   (simple-noun-phrase (article the) (noun student))
   (verb-phrase
    (verb-phrase
     (verb-phrase (verb studies) (prep-phrase (prep by) (simple-noun-phr ...
     (prep-phrase (prep with) (simple-noun-phrase (article the) (noun pr ...
    (prep-phrase (prep in) (simple-noun-phrase (article a) (noun profess ...

Demo:
")

(#%require "ea-analyzing-50.scm")
(put-evaluators)

(define word-prog
  '(begin

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
       ;(require (memq (car *unparsed*) (cdr word-list)))
       (let ((found-word (random-word word-list)))
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
         (ramb noun-phrase
               (maybe-extend (list 'noun-phrase
                                   noun-phrase
                                   (parse-prepositional-phrase)))))
       (maybe-extend (parse-simple-noun-phrase)))

     (define (parse-verb-phrase)
       (define (maybe-extend verb-phrase)
         (ramb verb-phrase
               (maybe-extend (list 'verb-phrase
                                   verb-phrase
                                   (parse-prepositional-phrase)))))
       (maybe-extend (parse-word verbs)))

     (define (parse-prepositional-phrase)
       (list 'prep-phrase
             (parse-word prepositions)
             (parse-noun-phrase)))

     (parse
      '(the professor lectures to the student in the class with the cat))

     ))

(eval word-prog)


(--end-- "4.50")
