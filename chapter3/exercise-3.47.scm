#lang sicp

(#%require "common.scm")

;   Exercise 3.47
;   =============
;   
;   A semaphore (of size n) is a generalization of a mutex.  Like a mutex, a
;   semaphore supports acquire and release operations, but it is more
;   general in that up to n processes can acquire it concurrently. 
;   Additional processes that attempt to acquire the semaphore must wait for
;   release operations.  Give implementations of semaphores
;   
;   a. in terms of mutexes
;   
;   b. in terms of atomic test-and-set! operations.
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.47]: http://sicp-book.com/book-Z-H-23.html#%_thm_3.47
;   3.4.2 Mechanisms for Controlling Concurrency - p313
;   ------------------------------------------------------------------------

(-start- "3.47")

;; HELPERS
;; =======

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))


(prn "
Using mutex:
============

(define (make-semaphore max)
  (let ((mutex (make-mutex))
        (count 0))
    (define (dispatch m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (cond ((< count max)
                    (set! count (+ count 1))
                    (mutex 'release))
                   (else
                    (mutex 'release)
                    (dispatch 'acquire))))        
            ((eq? m 'release)
             (mutex 'acquire)
             (set! count (- count 1))
             (mutex 'release))
            (else (error \"Unknown request -- SAMAPHORE\" m))))
    dispatch)))


Using test-and-set!
===================

(define (make-semaphore max)
  (let ((cell (list false))
        (count 0))
    (define (dispatch m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (dispatch'acquire))
             (cond ((< count max)
                    (set! count (+ count 1))
                    (clear! cell))
                   (else
                    (clear! cell)
                    (dispatch 'acquire))))        
            ((eq? m 'release)
             (if (test-and-set! cell)
                 (dispatch'release))
             (set! count (- count 1))
             (clear! cell))
            (else (error \"Unknown request -- SAMAPHORE\" m))))
    dispatch))
")

(--end-- "3.47")

