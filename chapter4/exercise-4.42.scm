#lang sicp

(#%require "common.scm")

;   Exercise 4.42
;   =============
;   
;   Solve the following "Liars" puzzle (from Phillips 1934): 
;   
;       Five schoolgirls sat for an examination.  Their parents -- so
;       they thought -- showed an undue degree of interest in the
;       result.  They therefore agreed that, in writing home about the
;       examination, each girl should make one true statement and one
;       untrue one.  The following are the relevant passages from their
;       letters:
;   
;       * Betty: "Kitty was second in the examination.  I was only
;       third."
;   
;       * Ethel: "You'll be glad to hear that I was on top.  Joan was
;       second."
;   
;       * Joan: "I was third, and poor old Ethel was bottom."
;   
;       * Kitty: "I came out second.  Mary was only fourth."
;   
;       * Mary: "I was fourth.  Top place was taken by Betty."
;   
;       What in fact the order in which the five girls were placed?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.42]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.42
;   4.3.2 Examples of Nondeterministic Programs - p420
;   ------------------------------------------------------------------------

(-start- "4.42")

(define (require p)
  (if (not p) (amb)))

(define (require-one p q)
  (require (if p (not q) q)))              

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (liars-puzzle)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require-one (= kitty 2) (= betty 3))
    (require-one (= ethel 1) (= joan 2))
    (require-one (= joan 3) (= ethel 5))
    (require-one (= kitty 2) (= mary 4))
    (require-one (= mary 4) (= betty 1))
    (require (distinct? (list betty ethel joan kitty mary)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))
    
(liars-puzzle)          
        
(--end-- "4.42")

