#lang sicp

(#%require "common.scm")

;   Exercise 3.38
;   =============
;   
;   Suppose that Peter, Paul, and Mary share a joint bank account that
;   initially contains $100.  Concurrently, Peter deposits $10, Paul
;   withdraws $20, and Mary withdraws half the money in the account, by
;   executing the following commands:
;   
;   Peter: (set! balance (+ balance 10))           
;   Paul:  (set! balance (- balance 20))           
;   Mary:  (set! balance (- balance (/ balance 2)))
;   
;   a. List all the different possible values for balance after these three
;   transactions have been completed, assuming that the banking system
;   forces the three processes to run sequentially in some order.
;   
;   b. What are some other values that could be produced if the system
;   allows the processes to be interleaved? Draw timing diagrams like the
;   one in figure [3.29] to explain how these values can occur.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.38]: http://sicp-book.com/book-Z-H-23.html#%_thm_3.38
;   [Figure 3.29]:   http://sicp-book.com/book-Z-H-23.html#%_fig_3.29
;   3.4.1 The Nature of Time in Concurrent Systems - p303
;   ------------------------------------------------------------------------

(-start- "3.38")

(prn "Part A
======

Peter -> Paul -> Mary -> 45
Peter -> Mary -> Paul -> 35
Paul -> Pater -> Mary -> 45
Paul -> Mary -> Peter -> 50
Mary -> Peter -> Paul -> 40
Mary -> Paul -> Peter -> 40

So possible values are 35, 40, 45 and 50.

Part B
======

In this simple case interleving could have the effect of 'skipping' one or
two commands so additional results are:
     
Peter -> Paul -> 90
Paul -> Peter -> 90
Peter -> Mary -> 55
Mary -> Peter -> 60
Paul -> Mary -> 40
Mary -> Paul -> 40
Peter -> 110
Paul -> 80
Mary -> 50

So new values that we could get are 55, 60, 80, 90 and 110.
(As well new ways to get 40 and 50).

To get 55:
==========
Peter        Paul         Mary
Read  100
                        Read  100
                        Write  50
Write 110
            Read  110
            Write  55


To get 60:
==========
Peter        Paul         Mary
                        Read  100
            Read 100
            Write 80
                        Write  50
Read   50
Write  60


To get 80:
==========
Peter        Paul         Mary
            Read 100
Read  100
Write 110
                        Read  100
                        Write  50
            Write 80


To get 90:
==========
Peter        Paul         Mary
Read  100
                        Read  100
                        Write  50
Write 110
            Read 110
            Write 90


To get 110:
===========
Peter        Paul         Mary
Read  100
                        Read  100
                        Write  50
            Read 100
            Write 80
Write 110
     
Addendum 1:
==========
mary -> peter -> mary -> paul -> 25

To get 25:
==========
Peter        Paul         Mary
                        Read  100
Read  100
Write 110
                        Read  110
                        Write  45
            Read  45
            Write 25
   
Addendum 2:
===========
mary -> paul -> mary -> peter -> 70
     
To get 70:     
==========
Peter        Paul         Mary
                        Read  100
            Read  100
            Write 80
                        Read   80
                        Write  60
Read  60
Write 70
   
")
(--end-- "3.38")

