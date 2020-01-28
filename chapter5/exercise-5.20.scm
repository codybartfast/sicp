#lang sicp

(#%require "common.scm")

;   Exercise 5.20
;   =============
;
;   Draw the box-and-pointer representation and the memory-vector
;   representation (as in figure [5.14]) of the list structure produced by
;
;   (define x (cons 1 2))
;   (define y (list x x))
;
;   with the free pointer initially p1.  What is the final value of free ?
;   What pointers represent the values of x and y ?
;
;   ------------------------------------------------------------------------
;   [Exercise 5.20]: http://sicp-book.com/book-Z-H-33.html#%_thm_5.20
;   [Figure 5.14]:   http://sicp-book.com/book-Z-H-33.html#%_fig_5.14
;   5.3.1 Memory as Vectors - p539
;   ------------------------------------------------------------------------

(-start- "5.20")

(println
 "
          [.|.] --> [.|/]
         3 │       2 │
           └────┐ ┌──┘
                v v
               [.|.]
              1 │ │
                V V
                1 2

Index      0  1  2  3  4  5
         ┌──┬──┬──┬──┬──┬──┐
the-cars │  │n1│p1│p1│  │  │
         ├──┼──┼──┼──┼──┼──┤
the-cdrs │  │n2│e0│p2│  │  │
         └──┴──┴──┴──┴──┴──┘

free = p4
x    = p1
y    = p3

Looking at various answers on the interweb there seems to be litte agreement
on the order in which values are allocated to memory. A common variation is:

Index      0  1  2  3  4  5
         ┌──┬──┬──┬──┬──┬──┐
the-cars │  │n1│p1│p1│  │  │
         ├──┼──┼──┼──┼──┼──┤
the-cdrs │  │n2│p3│e0│  │  │
         └──┴──┴──┴──┴──┴──┘

This doesn't look right to me because the address that (p1 . e0) is stored
in is not known until after (p1 . e0) is saved to memory:

  (perform
   (op vector-set!) (reg the-cars) (reg free) (reg <reg2>))
  (perform
   (op vector-set!) (reg the-cdrs) (reg free) (reg <reg3>))
  (assign <reg1> (reg free))  ;; <-- address not avialable until here
  (assign free (op +) (reg free) (const 1))

So the contents of the inner cons must already be stored before the outer
cons can use the address of the inner cons as its cdr.

Generally, when looking at Lisp expressions, I would imagine values are
stored in the order that operations complete, not in the order that they are
called.
")

(--end-- "5.20")

