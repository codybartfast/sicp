#lang sicp

(#%require "common.scm")

;   Exercise 3.44
;   =============
;   
;   Consider the problem of transferring an amount from one account to
;   another.  Ben Bitdiddle claims that this can be accomplished with the
;   following procedure, even if there are multiple people concurrently
;   transferring money among multiple accounts, using any account mechanism
;   that serializes deposit and withdrawal transactions, for example, the
;   version of make-account in the text above.
;   
;   (define (transfer from-account to-account amount)
;     ((from-account 'withdraw) amount)
;     ((to-account 'deposit) amount))
;   
;   Louis Reasoner claims that there is a problem here, and that we need to
;   use a more sophisticated method, such as the one required for dealing
;   with the exchange problem.  Is Louis right?  If not, what is the
;   essential difference between the transfer problem and the exchange
;   problem?  (You should assume that the balance in from-account is at
;   least amount.)
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.44]: http://sicp-book.com/book-Z-H-23.html#%_thm_3.44
;   3.4.2 Mechanisms for Controlling Concurrency - p310
;   ------------------------------------------------------------------------

(-start- "3.44")

(prn "
Surpising I think Louis is wrong.

This is similar to the second part of the previous question.  The 'danger'
came from the potential for the values to change between reading, deciding
the amount to change, and then setting.

In this case we don't 'care' what the existing balance is, the amount to
transfer is independent of the existing value[*] so we don't mind if they
change before, between or after the two operations.

[* the exception to 'not caring' would be if there was an insufficient
balance, then the withdrawl would not happen but the deposit would.]

")
(--end-- "3.44")

