#lang sicp

(#%require "common.scm")

;   Exercise 3.43
;   =============
;   
;   Suppose that the balances in three accounts start out as $10, $20, and
;   $30, and that multiple processes run, exchanging the balances in the
;   accounts.  Argue that if the processes are run sequentially, after any
;   number of concurrent exchanges, the account balances should be $10, $20,
;   and $30 in some order. Draw a timing diagram like the one in figure
;   [3.29] to show how this condition can be violated if the exchanges are
;   implemented using the first version of the account-exchange program in
;   this section.  On the other hand, argue that even with this exchange
;   program, the sum of the balances in the accounts will be preserved. 
;   Draw a timing diagram to show how even this condition would be violated
;   if we did not serialize the transactions on individual accounts.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.43]: http://sicp-book.com/book-Z-H-23.html#%_thm_3.43
;   [Figure 3.29]:   http://sicp-book.com/book-Z-H-23.html#%_fig_3.29
;   3.4.2 Mechanisms for Controlling Concurrency - p309
;   ------------------------------------------------------------------------

(-start- "3.43")

(prn "
Argue should be $10, $20, and $30 in some order
===============================================
No two exchange processes can overlap so each can be reasoned about
independently.  Each process (when run independently) changes the order, but
not the values.  Since each individual process leaves the
combination unchanged any series of processes will also leave the
combination unchanged.


This condition can be violated with first version of code
=========================================================
This is the usual story:

(a, b, c)

Exchange a b     Exchange b c
------------     ------------
         (10, 20, 30)
Read 10, 20
                 Read 20, 30
<- Trnsfr 10
         (20, 10, 30)
                 <- Trnsfr 10
         (20, 20, 20)

Total value is unchanged because because although the value of 'difference'
could be wrong (e.g. changes made to accoun2 between the reading of account1
and account2)  the same vauue ('difference') is passed to both the the
withdraw and deposit accounts.


This condition is violated if we do not serialize the transaction
=================================================================

In the above example we the to transfers can happen at the same time.  This
means we could have a withdrawl and a depost to account2 at the same time.

Withdraw 10           Deposit 10
===========           ==========
                 20
Read balance 20
                      Read blance 20
                      New balance 30
                      set! 30
                 30
New balance 10
set! 10
                 10

As the other two accounts are only touched by one process their values will
the same as the above example so the final balances will be:
         (20, 10, 20)
I.e., $10 has been destroyed.
")


(--end-- "3.43")

