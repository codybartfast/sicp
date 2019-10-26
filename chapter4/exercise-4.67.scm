#lang sicp

(#%require "common.scm")

;   Exercise 4.67
;   =============
;   
;   Devise a way to install a loop detector in the query system so as to
;   avoid the kinds of simple loops illustrated in the text and in exercise
;   [4.64].  The general idea is that the system should maintain some sort
;   of history of its current chain of deductions and should not begin
;   processing a query that it is already working on.  Describe what kind of
;   information (patterns and frames) is included in this history, and how
;   the check should be made.  (After you study the details of the
;   query-system implementation in section [4.4.4], you may want to modify
;   the system to include your loop detector.)
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.67]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.67
;   [Section 4.4.4]: http://sicp-book.com/book-Z-H-29.html#%_sec_4.4.4
;   [Exercise 4.64]: http://sicp-book.com/book-Z-H-29.html#%_thm_4.64
;   4.4.3 Is Logic Programming Mathematical Logic? - p467
;   ------------------------------------------------------------------------

(-start- "4.67")

(println
 "
I think the history could be an 'identity' for the a query and the value of
the frame passed to that query.  Then, before evaluating a query, we can
check if the query is already in the history, and if it is, check if the
historic fame is the same as the current frame.

If evaluating a query with a given frame results in the same query and frame
being evaluated again then we will have infinite loop.  However it is normal
for the same query to be nested in itself (but with an updated frame).

As for the 'identity' of the query to use in the history, I suspect using
the query object itself is probably best 'key' for the history table.  I.e.
use 'eq?' for object equality.

I don't know enought about how we're going to implement things yet, but I'm
assuming the history can be passed around like the environment in a regular
scheme interpretor.
")

(--end-- "4.67")

