#lang sicp

(#%require "common.scm")

;   Exercise 4.8
;   ============
;   
;   "Named let" is a variant of let that has the form
;   
;   (let <var> <bindings> <body>)
;   
;   The <bindings> and <body> are just as in ordinary let, except that <var>
;   is bound within <body> to a procedure whose body is <body> and whose
;   parameters are the variables in the <bindings>.  Thus, one can
;   repeatedly execute the <body> by invoking the procedure named <var>. 
;   For example, the iterative Fibonacci procedure (section [1.2.2]) can be
;   rewritten using named let as follows:
;   
;   (define (fib n)
;     (let fib-iter ((a 1)
;                    (b 0)
;                    (count n))
;       (if (= count 0)
;           b
;           (fib-iter (+ a b) a (- count 1)))))
;   
;   Modify let->combination of exercise [4.6] to also support named let.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.8]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.8
;   [Section 1.2.2]: http://sicp-book.com/book-Z-H-11.html#%_sec_1.2.2
;   [Exercise 4.6]:  http://sicp-book.com/book-Z-H-26.html#%_thm_4.6
;   4.1.2 Representing Expressions - p376
;   ------------------------------------------------------------------------

(-start- "4.8")



(--end-- "4.8")

