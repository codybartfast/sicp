#lang sicp

(#%require "common.scm")

;   Exercise 4.59
;   =============
;   
;   Ben Bitdiddle has missed one meeting too many. Fearing that his habit of
;   forgetting meetings could cost him his job, Ben decides to do something
;   about it.  He adds all the weekly meetings of the firm to the Microshaft
;   data base by asserting the following:
;   
;   (meeting accounting (Monday 9am))
;   (meeting administration (Monday 10am))
;   (meeting computer (Wednesday 3pm))
;   (meeting administration (Friday 1pm))
;   
;   Each of the above assertions is for a meeting of an entire division. Ben
;   also adds an entry for the company-wide meeting that spans all the
;   divisions.  All of the company's employees attend this meeting.
;   
;   (meeting whole-company (Wednesday 4pm))
;   
;   a. On Friday morning, Ben wants to query the data base for all the
;   meetings that occur that day.  What query should he use?
;   
;   b. Alyssa P. Hacker is unimpressed.  She thinks it would be much more
;   useful to be able to ask for her meetings by specifying her name.  So
;   she designs a rule that says that a person's meetings include all
;   whole-company meetings plus all meetings of that person's division. Fill
;   in the body of Alyssa's rule.
;   
;   (rule (meeting-time ?person ?day-and-time)
;         <rule-body>)
;   
;   c. Alyssa arrives at work on Wednesday morning and wonders what meetings
;   she has to attend that day.  Having defined the above rule, what query
;   should she make to find this out?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.59]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.59
;   4.4.1 Deductive Information Retrieval - p450
;   ------------------------------------------------------------------------

(-start- "4.59")

(println
 "
    a. (meeting ?group (Friday ?time))

    b. (rule (meeting-time ?person ?day-and-time)
             (or (meeting whole-company ?day-and-time)
                 (and (job ?person (?division . ?div-tail))
                      (meeting ?division ?day-and-time))))

    c. (meeting-time (Hacker Alyssa P) (Wednesday ?time))
")

(--end-- "4.59")

