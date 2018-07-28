#lang sicp

(#%require "common.scm")

;   Exercise 3.48
;   =============
;   
;   Explain in detail why the deadlock-avoidance method described above,
;   (i.e., the accounts are numbered, and each process attempts to acquire
;   the smaller-numbered account first) avoids deadlock in the exchange
;   problem.  Rewrite serialized-exchange to incorporate this idea. (You
;   will also need to modify make-account so that each account is created
;   with a number, which can be accessed by sending an appropriate message.)
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.48]: http://sicp-book.com/book-Z-H-23.html#%_thm_3.48
;   3.4.2 Mechanisms for Controlling Concurrency - p314
;   ------------------------------------------------------------------------

(-start- "3.48")

(prn "

Suppose an item needs to lock to resouces, h & t. And we consider the rest
of the system as 'the rest'.

If the-item fails to obtain a lock on h then it will not contribute to a
deadlock.

If the-item does obtain h then if there is a chain of dependencies that
depends on h and t within the-rest then it cannot already have a lock on t,
and it cannot get one (because h is locked). If there is something that
depends on t but not h then that doesn't create a deadlock becuase it can be
released without h having to be released first.


(define (make-new-serialization-id)
  (define next-id 0)
  (define mutex (make-mutex))
  (lambda ()
      (mutex 'acquire)
      (define id next-id)
      (set! next-id (+ next-id 1))
      (mutex 'release)
      next-id))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        \"Insufficient funds\"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define new-serialization-id (make-new-serialization-id))
  (let ((balance-serializer (make-serializer))
        (serialization-id (new-serialization-id)))
    (define (dispatch m)      
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'serialization-id) serialization-id)
            (else (error \"Unknown request -- MAKE-ACCOUNT\"
                         m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (cond
      ((< (account1 'serialization-id) (account2 'serialization-id))
       ((serializer1 (serializer2 exchange))
        account1
        account2))
      (else
       ((serializer2 (serializer1 exchange))
        account1
        account2)))))
")


(--end-- "3.48")

