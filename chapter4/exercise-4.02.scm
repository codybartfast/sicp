#lang sicp

(#%require "common.scm")

;   Exercise 4.2
;   ============
;   
;   Louis Reasoner plans to reorder the cond clauses in eval so that the
;   clause for procedure applications appears before the clause for
;   assignments.  He argues that this will make the interpreter more
;   efficient: Since programs usually contain more applications than
;   assignments, definitions, and so on, his modified eval will usually
;   check fewer clauses than the original eval before identifying the type
;   of an expression.
;   
;   a. What is wrong with Louis's plan?  (Hint: What will Louis's evaluator
;   do with the expression (define x 3)?)
;   
;   b. Louis is upset that his plan didn't work. He is willing to go to any
;   lengths to make his evaluator recognize procedure applications before it
;   checks for most other kinds of expressions. Help him by changing the
;   syntax of the evaluated language so that procedure applications start
;   with call.  For example, instead of (factorial 3) we will now have to
;   write (call factorial 3) and instead of (+ 1 2) we will have to write
;   (call + 1 2).
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.2]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.2
;   4.1.2 Representing Expressions - p374
;   ------------------------------------------------------------------------

(-start- "4.2")
(println "
A. What's wrong with Louis's Plan?
==================================

I'm probably missing something because the problem seems obvious even by
Louis's standards.  If define, set!, if or begin could simply be applied
then they wouldn't need special forms. All of these would therefore break
in fact they would never be called as their 'cond' clauses are unreachable
and they don't exist as regular named variable procedures.

B. Explicit Call
================

(define (application? exp)
        (if (tagged-list? exp 'call)
            (if (pair? (cdr exp))
                true
                (error \"CALL doesn't have operator\"))))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
")

(--end-- "4.2")

