#lang sicp

(#%require "common.scm")

;   Exercise 4.6
;   ============
;   
;   Let expressions are derived expressions, because
;   
;   (let ((<var₁> <exp₁>) ... (<var_(n)> <exp_(n)>))
;     <body>)
;   
;   is equivalent to
;   
;   ((lambda (<var₁> ... <var_(n)>)
;      <body>)
;    <exp₁>
;    ...
;    <exp_(n)>)
;   
;   Implement a syntactic transformation let->combination that reduces
;   evaluating let expressions to evaluating combinations of the type shown
;   above, and add the appropriate clause to eval to handle let expressions.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.6]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.6
;   4.1.2 Representing Expressions - p375
;   ------------------------------------------------------------------------

(-start- "4.6")



(--end-- "4.6")

