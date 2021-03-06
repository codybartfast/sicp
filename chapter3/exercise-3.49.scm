#lang sicp

(#%require "common.scm")

;   Exercise 3.49
;   =============
;   
;   Give a scenario where the deadlock-avoidance mechanism described above
;   does not work.  (Hint: In the exchange problem, each process knows in
;   advance which accounts it will need to get access to.  Consider a
;   situation where a process must get access to some shared resources
;   before it can know which additional shared resources it will require.)
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.49]: http://sicp-book.com/book-Z-H-23.html#%_thm_3.49
;   3.4.2 Mechanisms for Controlling Concurrency - p315
;   ------------------------------------------------------------------------

(-start- "3.49")

(prn "
Consider a program that attempts to find a chain of home owners who wish to
exchange their houses e.g. A's house -> B, B -> C, ...., Y -> Z, Z -> A.

Also consider that you each house owner changes their mind about which house
they want each time they are asked.  So 'A' has to be locked between finding
out what they want to get and fullfilling that request.

If I lock 'N' and find that it wants to buy 'M', then to I need to releae N
before I lock M, but when I relock N it no longer wants to mbuy M.
")

(--end-- "3.49")

