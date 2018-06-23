#lang sicp

(#%require "common.scm")

;   Exercise 3.42
;   =============
;   
;   Ben Bitdiddle suggests that it's a waste of time to create a new
;   serialized procedure in response to every withdraw and deposit message. 
;   He says that make-account could be changed so that the calls to
;   protected are done outside the dispatch procedure.  That is, an account
;   would return the same serialized procedure (which was created at the
;   same time as the account) each time it is asked for a withdrawal
;   procedure.
;   
;   (define (make-account balance)
;     (define (withdraw amount)
;       (if (>= balance amount)
;           (begin (set! balance (- balance amount))
;                  balance)
;           "Insufficient funds"))
;     (define (deposit amount)
;       (set! balance (+ balance amount))
;       balance)
;     (let ((protected (make-serializer)))
;       (let ((protected-withdraw (protected withdraw))
;             (protected-deposit (protected deposit)))
;         (define (dispatch m)
;           (cond ((eq? m 'withdraw) protected-withdraw)
;                 ((eq? m 'deposit) protected-deposit)
;                 ((eq? m 'balance) balance)
;                 (else (error "Unknown request -- MAKE-ACCOUNT"
;                              m))))
;         dispatch)))
;   
;   Is this a safe change to make?  In particular, is there any difference
;   in what concurrency is allowed by these two versions of make-account ?
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.42]: http://sicp-book.com/book-Z-H-23.html#%_thm_3.42
;   3.4.2 Mechanisms for Controlling Concurrency - p307
;   ------------------------------------------------------------------------

(-start- "3.42")

(prn "I think the new design prevents a withdrawal happening at the same
time as a depost.  The question is whether it prevents two deposit
operations, or two withdrawl operations, happining at the same time.  The
orignal design creates a new protected method with *every* call to deposit
or withdrawl so no two can interleave.  The new design only has two
protected functions, if there are two calls to withdrawl they are using the
same protected function.

'Make-serializer' prevents two different protected funcions from running
concurrently.  Does it pevent two instances of the same protected function
from running concurrently?

If yes, then the change is safe, otherwise the change is unsafe.

My guess is many implementions would make this unsafe.

Update: this proably is safe.  If the implemtation is done with a mutex as
implied later in the chapter then access to the mutex would not be
'compromised' just because it's the same function that's trying ot access
it. (Ex 3.45 also implies it's safe).
")

(--end-- "3.42")

