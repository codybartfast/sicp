#lang sicp

(#%require "common.scm")

;   Exercise 3.32
;   =============
;   
;   The procedures to be run during each time segment of the agenda are kept
;   in a queue.  Thus, the procedures for each segment are called in the
;   order in which they were added to the agenda (first in, first out). 
;   Explain why this order must be used.  In particular, trace the behavior
;   of an and-gate whose inputs change from 0,1 to 1,0 in the same segment
;   and say how the behavior would differ if we stored a segment's
;   procedures in an ordinary list, adding and removing procedures only at
;   the front (last in, first out).
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.32]: http://sicp-book.com/book-Z-H-22.html#%_thm_3.32
;   3.3.4 A Simulator for Digital Circuits - p285
;   ------------------------------------------------------------------------

(-start- "3.32")

(prn
"The output value tha is calculated before adding the action to the agenda.
So, when the first input to 'and' changes to '1' the intermediate output
value of '1' is stored in the action.  If actions were performed LIFO then
this would be the final value of the output, but would be wrong.  The value
calculated after changing the second (last) input is the one we need
assigned las to the output value.")

(--end-- "3.32")

