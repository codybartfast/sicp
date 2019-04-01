#lang sicp

(#%require "common.scm")

;   Exercise 4.3
;   ============
;   
;   Rewrite eval so that the dispatch is done in data-directed style. 
;   Compare this with the data-directed differentiation procedure of
;   exercise [2.73]. (You may use the car of a compound expression as the
;   type of the expression, as is appropriate for the syntax implemented in
;   this section.) .
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.3]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.3
;   [Exercise 2.73]: http://sicp-book.com/book-Z-H-26.html#%_thm_2.73
;   4.1.2 Representing Expressions - p374
;   ------------------------------------------------------------------------

(-start- "4.3")

(println "

    (define (eval exp env)
      (cond
        ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((get 'eval (expression-type exp))
         ((get 'eval (expression-type exp)) exp env))
        (else (error \"Unknown expression type -- EVAL\" exp))))

Where expression-type is:

    (define (expression-type exp)
      (cond
        ((assignment? exp) 'assignment)
        ((definition? exp) 'definition)
        ((if? exp) 'if)
        ((lambda? exp) 'lambda)
        ((begin? exp) 'begin)
        ((cond? exp) 'cond)
        ((application? exp) 'call)))

To avoid calling get twice the get condition could be replaced with:

    ((get 'eval (expression-type exp))
     => (lambda (evaluator) (evaluator exp env)))

but we aren't introduced to that syntax until later in this section.

'exercise-4.03-extra' verifies the above works by implementing it with the
eval/apply code from the book's text.  This answer has been modified in the
light of that exercise, but I think the only major difference is that I
orignally thought if eval were to be this short/simple then we needed a 
'call tag for application (as in the  previous answer).  But expression-type
doesn't need the 'call tag to deduce an expression should be a function
application.

Comparing it to Ex 2.73 I'm primarily struck by the similarity. Simple cases
are handled specifically and then more complicated cases are handled in a
standard way which allows the data directed approach.  With 2.73 the data /
operands is a well defined list of values, here though the data / exp can
have any structure so long as it makes sense to the specific evaluator.
")

(--end-- "4.3")

