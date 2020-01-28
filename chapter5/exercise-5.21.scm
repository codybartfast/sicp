#lang sicp

(#%require "common.scm")

;   Exercise 5.21
;   =============
;   
;   Implement register machines for the following procedures. Assume that
;   the list-structure memory operations are available as machine
;   primitives.
;   
;   a. Recursive count-leaves:
;   
;   (define (count-leaves tree)
;     (cond ((null? tree) 0)
;           ((not (pair? tree)) 1)
;           (else (+ (count-leaves (car tree))
;                    (count-leaves (cdr tree))))))
;   
;   b. Recursive count-leaves with explicit counter:
;   
;   (define (count-leaves tree)
;     (define (count-iter tree n)
;       (cond ((null? tree) n)
;             ((not (pair? tree)) (+ n 1))
;             (else (count-iter (cdr tree)
;                               (count-iter (car tree) n)))))
;     (count-iter tree 0))
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.21]: http://sicp-book.com/book-Z-H-33.html#%_thm_5.21
;   5.3.1 Memory as Vectors - p539
;   ------------------------------------------------------------------------

(-start- "5.21")

(#%require "machine-19.scm")
(println
 "
Recursive Count-Leaves
======================
")

(let ((machine (make-machine
                (list
                  (list '+ +)
                  (list 'null? null?)
                  (list 'pair? pair?)
                  (list 'not not)
                  (list 'car car)
                  (list 'cdr cdr))
                '((assign continue (label done))
                  count-leaves
                    (test (op null?) (reg tree))
                    (branch (label null-tree))
                    (assign t (op pair?) (reg tree))
                    (test (op not) (reg t))
                    (branch (label non-pair-tree))
                    (save continue)
                    (save tree)
                    (assign tree (op car) (reg tree))
                    (assign continue (label after-left))
                    (goto (label count-leaves))
                  after-left
                    (restore tree)
                    (assign tree (op cdr) (reg tree))
                    (assign continue (label after-right))
                    (save val)
                    (goto (label count-leaves))
                  after-right
                    (assign t (reg val))
                    (restore val)
                    (assign val (op +) (reg val) (reg t))
                    (restore continue)
                    (goto (reg continue))
                  null-tree
                    (assign val (const 0))
                    (goto (reg continue))
                  non-pair-tree
                    (assign val (const 1))
                    (goto (reg continue))
                  done))))

  (set-register-contents! machine 'tree '(1 (((1 1) 1) 1) 1 1))
  (start machine)
  (get-register-contents machine 'val))

(println
 "
Iterative Count-Leaves
======================
")

(let ((machine (make-machine
                (list
                  (list '+ +)
                  (list 'null? null?)
                  (list 'pair? pair?)
                  (list 'car car)
                  (list 'cdr cdr))
                '((assign val (const 0))
                  (assign continue (label done))
                  count-leaves
                    (test (op null?) (reg tree))
                    (branch (label null-tree))
                    (test (op pair?) (reg tree))
                    (branch (label have-pair))
                    (assign val (op +) (reg val) (const 1))
                    (goto (reg continue))
                  null-tree
                    (goto (reg continue))
                  have-pair
                    (save continue)
                    (save tree)
                    (assign tree (op car) (reg tree))
                    (assign continue (label after-left))
                    (goto (label count-leaves))
                  after-left
                    (restore tree)
                    (assign tree (op cdr) (reg tree))
                    (restore continue)
                    (goto (label count-leaves))
                  done))))

  (set-register-contents! machine 'tree '(1 (((1 1) 1) 1) 1 1))
  (start machine)
  (get-register-contents machine 'val))

(println "")

(--end-- "5.21")

