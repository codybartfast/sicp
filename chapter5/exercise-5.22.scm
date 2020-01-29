#lang sicp

(#%require "common.scm")

;   Exercise 5.22
;   =============
;   
;   Exercise [3.12] of section [3.3.1] presented an append procedure that
;   appends two lists to form a new list and an append! procedure that
;   splices two lists together.  Design a register machine to implement each
;   of these procedures.  Assume that the list-structure memory operations
;   are available as primitive operations.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.22]: http://sicp-book.com/book-Z-H-33.html#%_thm_5.22
;   [Section 3.3.1]: http://sicp-book.com/book-Z-H-22.html#%_sec_3.3.1
;   [Exercise 3.12]: http://sicp-book.com/book-Z-H-33.html#%_thm_3.12
;   5.3.1 Memory as Vectors - p539
;   ------------------------------------------------------------------------

(-start- "5.22")

(#%require "machine-19.scm")

(println
 "
Recursive Append
================
")

(let ((machine (make-machine
                (list
                  (list 'null? null?)
                  (list 'car car)
                  (list 'cdr cdr)
                  (list 'cons cons))
                '((assign continue (label done))
                  append
                    (test (op null?) (reg x))
                    (branch (label x-null))
                    (assign head (op car) (reg x))
                    (assign x (op cdr) (reg x))
                    (save continue)
                    (assign continue (label cons))
                    (save head)
                    (goto (label append))
                  x-null
                    (goto (reg continue))
                  cons
                    (restore head)
                    (restore continue)
                    (assign y (op cons) (reg head) (reg y))
                    (goto (reg continue))
                  done))))

  (set-register-contents! machine 'x '(a b c d))
  (set-register-contents! machine 'y '(e f g h))
  (start machine)
  (get-register-contents machine 'y))

(println
 "
Splice Append
=============
")

(let ((machine (make-machine
                (list
                  (list 'null? null?)
                  (list 'set-cdr! set-cdr!)
                  (list 'cdr cdr))
                '((save x)
                  last-pair
                    (assign x-cdr (op cdr) (reg x))
                    (test (op null?) (reg x-cdr))
                    (branch (label splice))
                    (assign x (reg x-cdr))
                    (goto (label last-pair))
                  splice
                    (perform (op set-cdr!) (reg x) (reg y))
                    (restore x)))))

  (set-register-contents! machine 'x '(a b c d))
  (set-register-contents! machine 'y '(e f g h))
  (start machine)
  (get-register-contents machine 'x))

(println "")

(--end-- "5.22")

