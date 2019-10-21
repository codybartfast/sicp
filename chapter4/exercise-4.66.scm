#lang sicp

(#%require "common.scm")

;   Exercise 4.66
;   =============
;   
;   Ben has been generalizing the query system to provide statistics about
;   the company.  For example, to find the total salaries of all the
;   computer programmers one will be able to say
;   
;   (sum ?amount
;        (and (job ?x (computer programmer))
;             (salary ?x ?amount)))
;   
;   In general, Ben's new system allows expressions of the form
;   
;   (accumulation-function <variable>
;                          <query pattern>)
;   
;   where accumulation-function can be things like sum, average, or maximum.
;   Ben reasons that it should be a cinch to implement this.  He will simply
;   feed the query pattern to qeval.  This will produce a stream of frames. 
;   He will then pass this stream through a mapping function that extracts
;   the value of the designated variable from each frame in the stream and
;   feed the resulting stream of values to the accumulation function.  Just
;   as Ben completes the implementation and is about to try it out, Cy walks
;   by, still puzzling over the wheel query result in exercise [4.65].  When
;   Cy shows Ben the system's response, Ben groans, "Oh, no, my simple
;   accumulation scheme won't work!"
;   
;   What has Ben just realized?  Outline a method he can use to salvage the
;   situation.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.66]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.66
;   [Exercise 4.65]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.65
;   4.4.3 Is Logic Programming Mathematical Logic? - p467
;   ------------------------------------------------------------------------

(-start- "4.66")

(println
 "
Ben has realized that there may be multiple query results that are
identical.  The query to find the salary of all computer programmers will
work, as the <query pattern> will only return each computer programmer just
once.  However, if a similar query were run to find the salary of all wheels
then it would return the wrong value as Oliver would be returned four times
and his salary would be counted four time.

A general method for salvaging the situation is probably the use of a unique
or distinct function to remove duplicates from the result of the querry
pattern.  E.g., uniqueness would reduce:

  (wheel (Warbucks Oliver))
  (wheel (Bitdiddle Ben))
  (wheel (Warbucks Oliver))
  (wheel (Warbucks Oliver))
  (wheel (Warbucks Oliver))

to:

  (wheel (Warbucks Oliver))
  (wheel (Bitdiddle Ben))

This would give us the correct number of wheels (if we accumulatd with
count).  If we want the salary to be correct then we would futher require
that the ?amount be included in the result, and that only ?amount values in
the result are fed to the accumulator (I.e. only regard values after removal
of duplicates).

But having values like this in the result and then requiring uniqueness
creates another issue.  Imagine we doing an accumulation of loc (lines of
code) written by middle managers.  Then

  (loc (Bitdiddle Ben) 1024)

and 

  (loc (Bitdiddle Ben) 1025)

might be returned if Ben's record were updated while the query was running.
This might result in 2049 loc being accumulated for Ben's work.  Fortunately
we can rely on middle management never to allow skewed statistics that
create an artificially high impression of their contribution.

But just in case, we might want to specify which 'fields' of a result should
be looked at when considering uniqueness.
")
(--end-- "4.66")

