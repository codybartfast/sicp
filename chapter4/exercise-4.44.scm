#lang sicp

(#%require "common.scm")

;   Exercise 4.44
;   =============
;   
;   Exercise [2.42] described the "eight-queens puzzle" of placing queens on
;   a chessboard so that no two attack each other. Write a nondeterministic
;   program to solve this puzzle.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.44]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.44
;   [Exercise 2.42]: http://sicp-book.com/book-Z-H-28.html#%_thm_2.42
;   4.3.2 Examples of Nondeterministic Programs - p420
;   ------------------------------------------------------------------------

(-start- "4.44")

(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (new-queen col)
  (cons col (amb 1 2 3 4 5 6 7 8)))

(define (8queens)
  (define (iter queens)
    (require (distinct? (map cdr queens)))
    (require (distinct? (map (lambda (q) (- (car q) (cdr q))) queens)))
    (require (distinct? (map (lambda (q) (+ (car q) (cdr q))) queens)))
    (if (= 8 (length queens))
        queens
        (iter (cons (new-queen (+ 1 (length queens))) queens))))
  (iter '()))
        
(8queens)

(--end-- "4.44")

