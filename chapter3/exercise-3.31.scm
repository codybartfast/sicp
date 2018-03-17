#lang sicp

(#%require "common.scm")

;   Exercise 3.31
;   =============
;   
;   The internal procedure accept-action-procedure! defined in make-wire
;   specifies that when a new action procedure is added to a wire, the
;   procedure is immediately run.  Explain why this initialization is
;   necessary.  In particular, trace through the half-adder example in the
;   paragraphs above and say how the system's response would differ if we
;   had defined accept-action-procedure! as
;   
;   (define (accept-action-procedure! proc)
;     (set! action-procedures (cons proc action-procedures)))
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.31]: http://sicp-book.com/book-Z-H-22.html#%_thm_3.31
;   3.3.4 A Simulator for Digital Circuits - p282
;   ------------------------------------------------------------------------

(-start- "3.31")

(prn "The shorter version of accept-action-procedure! would work if we could
assume that all wires have a value of zero when all the inputs are zero. If
that were the case the call to procedure might be redundent becaue (by
assumption) calling procedure wouldn't change any values from the
default value of zero.  However, this is not the case. E.g. without a call
to procedure the output from Inverter (in the half-adder) would start with a
wrong value of zero.  Further, even after input-1 is changed, 'sum' would be
wrong (zero) because the output from the inverter will still be zero because
the input to the inverter has not changed and so the procedure is not called
on the inverter and so its output is not 'corrected'.

(This differs from most people's answer to this question, hmm. (Actually
it's not that different, most answer point out that with the shorter version
'it doesn't run' (isn't evaluated) initially.  Whereas I'm saying why it
matters that it doesn't run.).)")

(--end-- "3.31")

