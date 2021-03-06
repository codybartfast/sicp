#lang sicp

(#%require "common.scm")

;   Exercise 3.4
;   ============
;   
;   Modify the make-account procedure of exercise [3.3] by adding another
;   local state variable so that, if an account is accessed more than seven
;   consecutive times with an incorrect password, it invokes the procedure
;   call-the-cops.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.4]:  http://sicp-book.com/book-Z-H-20.html#%_thm_3.4
;   [Exercise 3.3]:  http://sicp-book.com/book-Z-H-20.html#%_thm_3.3
;   3.1.1 Local State Variables - p225
;   ------------------------------------------------------------------------

(-start- "3.4")

(define (call-the-cops)
  "I'm calling the cops but don't say anything.")

(define (make-account initial-password balance)
  (define consec-bad-passwd 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch given-password m)
    (cond ((< 7 consec-bad-passwd) ; Check if we're locked out
           (call-the-cops)
           (lambda (_) "I'm working on that..."))
          ((eq? initial-password given-password) ; Check given password
           (set! consec-bad-passwd 0)
           (cond ((eq? m 'withdraw) withdraw)
                 ((eq? m 'deposit) deposit)
                 (else (error "Unknown request -- MAKE-ACCOUNT"
                              m))))
          (else
           (set! consec-bad-passwd (+ 1 consec-bad-passwd))
           (lambda (_) "Incorrect Password"))))
  dispatch)

(define acc (make-account 'passw0rd 100))
((acc 'passw0rd 'withdraw) 10)
((acc 'Password 'deposit) 10)
((acc 'Password 'deposit) 10)
((acc 'Password 'deposit) 10)
((acc 'passw0rd 'withdraw) 10)
((acc 'Password 'deposit) 10)
((acc 'Password 'deposit) 10)
((acc 'Password 'deposit) 10)
((acc 'passw0rd 'withdraw) 10)
((acc 'Password 'deposit) 10)
((acc 'Password 'deposit) 10)
((acc 'Password 'deposit) 10)
((acc 'passw0rd 'withdraw) 10)
((acc 'Password 'deposit) 10)
((acc 'Password 'deposit) 10)
((acc 'Password 'deposit) 10)
((acc 'Password 'deposit) 10)
((acc 'Password 'deposit) 10)
((acc 'Password 'deposit) 10)
((acc 'Password 'deposit) 10)
((acc 'Password 'deposit) 10)
((acc 'Password 'deposit) 10)
;; check stays locked even if correct
;; password is provided evntually.

((acc 'Passw0rd 'deposit) 10)
((acc 'Passw0rd 'withdraw) 10)

(--end-- "3.4")

