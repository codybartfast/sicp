#lang sicp

(#%require "common.scm")

;   Exercise 5.39
;   =============
;   
;   Write a procedure lexical-address-lookup that implements the new lookup
;   operation.  It should take two arguments -- a lexical address and a
;   run-time environment -- and return the value of the variable stored at
;   the specified lexical address.  Lexical-address-lookup should signal an
;   error if the value of the variable is the symbol *unassigned*.⁽⁴⁶⁾ Also
;   write a procedure lexical-address-set! that implements the operation
;   that changes the value of the variable at a specified lexical address.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.39]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.39
;   [Footnote 46]:   http://sicp-book.com/book-Z-H-35.html#footnote_Temp_826
;   5.5.6 Lexical Addressing - p602
;   ------------------------------------------------------------------------

(-start- "5.39")

(println
 "
Assuming the address is a cons'ed pair and that we are using the original
frame structure from the text.

  (define (skip lst n)
    (if (= n 0)
        lst
        (skip (cdr lst) (- n 1))))

  (define (frame-values frame) (cdr frame))

  (define (lex-frame-number addr) (car addr))
  (define (lex-displacement addr) (cdr addr))

  (define (lexical-address-lookup addr env)
    (let ((frame (list-ref env (lex-frame-number addr))))
      (let ((value (list-ref (frame-values frame) (lex-displacement addr))))
        (if (= value '*unassigned*)
            (error \"Unassigned lex-address:\" addr)
            value))))

  (define (lexical-address-set! addr value env)
    (let ((frame (list-ref env (lex-frame-number addr))))
      (let ((val-head (skip (frame-values frame) (lex-displacement addr))))
        (set-car! val-head value)))

")

(--end-- "5.39")

