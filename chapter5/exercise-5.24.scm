#lang sicp

(#%require "common.scm")

;   Exercise 5.24
;   =============
;   
;   Implement cond as a new basic special form without reducing it to if. 
;   You will have to construct a loop that tests the predicates of
;   successive cond clauses until you find one that is true, and then use
;   ev-sequence to evaluate the actions of the clause.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.24]: http://sicp-book.com/book-Z-H-34.html#%_thm_5.24
;   5.4.3 Conditionals, Assignments, and Definitions - p560
;   ------------------------------------------------------------------------

(-start- "5.24")

(println
 "
Untested Code:

 (list
  (list 'have-clauses? (lambda (exp) (not (null? exp))))
  (list 'clauses-first car)
  (list 'clauses-rest cdr)
  (list 'error error))

 ((save continue)
  (assign exp (op cond-clauses) (reg exp))
  cond-have-clause?
    (test (op have-clauses?) (reg exp))
    (branch (label cond-check-clause))
    (goto (label cond-no-clauses))

  cond-check-clause
    (save exp)                                   ;; clauses list
    (assign exp (op clauses-first) (reg exp))
    (save exp)                                   ;; clause
    (test (op cond-else-clause?) (reg exp))
    (branch (label cond-else))
    (save env)
    (assign exp (op cond-predicate) (reg exp))   ;; predicate
    (assign continue (label cond-after-predicate))
    (goto (label eval-dispatch))

  cond-after-predicate
    (restore env)
    (restore exp)                                ;; clause
    (test (op true?) (reg val))
    (branch (label cond-actions))
    (restore exp)                                ;; clauses list
    (assign exp (op clauses-rest) (reg exp))     ;; rest of clauses
    (goto (label cond-have-clause?))

  cond-else
    (restore exp)                                ;; clause
    (assign unev (reg exp))
    (restore exp)                                ;; clauses list
    (assign val (op clauses-rest) (reg exp))
    (test (op have-clauses?) (reg val))
    (branch (label error-else-not-last))
    (save exp)                                   ;; clauses list
    (assign exp (reg unev))                      ;; clause
  cond-actions
    (assign exp (op cond-actions) (reg exp))     ;; actions
    (assign continue (label cond-after-actions))
    (goto (label ev-sequence))

  cond-no-clauses
    (assign val (const 'unspecified))
  cond-after-actions
    (restore continue)
    (goto (reg continue))

  error-else-not-last
    (restore (reg continue))
    (perform (op error) (const \"ELSE clause isn't last -- COND\")))
"
)

(--end-- "5.24")

