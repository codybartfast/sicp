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

Since the question says we may use the car of compound expressions that
implies (to me at least) that we should be assuming we have a tag for
application expressions like the 'call tag used in the previous question.
In that case I think _think_ eval could be as simple as the following.

(define expression-type car)
(define expression-body cdr)

(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((get 'eval (expression-type exp))
     => (lambda (evaluator) (evaluator exp env)))
    (else (error \"Unknown expression type -- EVAL\" exp))))

Comparing it to Ex 2.73 I'm primarily struck by the similarity. Simple cases
are handled specifically and then more complicated cases are handled in a
standard way which allows the data directed approach.  With 2.73 the data /
operands is a well defined list of values, here though the data / exp can
have any structure so long as it makes sense to the specific evaluator (and
it has the type tag).")

(--end-- "4.3")

