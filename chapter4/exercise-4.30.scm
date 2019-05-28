#lang sicp

(#%require "common.scm")

;   Exercise 4.30
;   =============
;   
;   Cy D. Fect, a reformed C programmer, is worried that some side effects
;   may never take place, because the lazy evaluator doesn't force the
;   expressions in a sequence. Since the value of an expression in a
;   sequence other than the last one is not used (the expression is there
;   only for its effect, such as assigning to a variable or printing), there
;   can be no subsequent use of this value (e.g., as an argument to a
;   primitive procedure) that will cause it to be forced.  Cy thus thinks
;   that when evaluating sequences, we must force all expressions in the
;   sequence except the final one.  He proposes to modify eval-sequence from
;   section [4.1.1] to use actual-value rather than eval:
;   
;   (define (eval-sequence exps env)
;     (cond ((last-exp? exps) (eval (first-exp exps) env))
;           (else (actual-value (first-exp exps) env)
;                 (eval-sequence (rest-exps exps) env))))
;   
;   a. Ben Bitdiddle thinks Cy is wrong. He shows Cy the for-each procedure
;   described in exercise [2.23], which gives an important example of a
;   sequence with side effects:
;   
;   (define (for-each proc items)
;     (if (null? items)
;         'done
;         (begin (proc (car items))
;                (for-each proc (cdr items)))))
;   
;   He claims that the evaluator in the text (with the original
;   eval-sequence) handles this correctly:
;   
;   ;;; L-Eval input:
;   (for-each (lambda (x) (newline) (display x))
;             (list 57 321 88))
;   57
;   321
;   88
;   ;;; L-Eval value:
;   done
;   
;   Explain why Ben is right about the behavior of for-each.
;   
;   b. Cy agrees that Ben is right about the for-each example, but says that
;   that's not the kind of program he was thinking about when he proposed
;   his change to eval-sequence. He defines the following two procedures in
;   the lazy evaluator:
;   
;   (define (p1 x)
;     (set! x (cons x '(2)))
;     x)
;   
;   (define (p2 x)
;     (define (p e)
;       e
;       x)
;     (p (set! x (cons x '(2)))))
;   
;   What are the values of (p1 1) and (p2 1) with the original
;   eval-sequence? What would the values be with Cy's proposed change to
;   eval-sequence?
;   
;   c. Cy also points out that changing eval-sequence as he proposes does
;   not affect the behavior of the example in part a. Explain why this is
;   true.
;   
;   d. How do you think sequences ought to be treated in the lazy evaluator?
;   Do you like Cy's approach, the approach in the text, or some other
;   approach?
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.30]: http://sicp-book.com/book-Z-H-27.html#%_thm_4.30
;   [Section 4.1.1]: http://sicp-book.com/book-Z-H-26.html#%_sec_4.1.1
;   [Exercise 2.23]: http://sicp-book.com/book-Z-H-27.html#%_thm_2.23
;   4.2.2 An Interpreter with Lazy Evaluation - p407
;   ------------------------------------------------------------------------

(-start- "4.30")

(#%require "ea-data-directed-27.scm")
(put-evaluators)

(define program
  '(begin

     (println "
Part A
------")
     (define (for-each proc items)
       (if (null? items)
           'done
           (begin (proc (car items))
                  (for-each proc (cdr items)))))

     (for-each (lambda (x) (println x))
               (list 57 321 88))

     (println "
Part B
------")
     (define (p1 x)
       (set! x (cons x '(2)))
       x)

     (define (p2 x)
       (define (p e)
         e
         x)
       (p (set! x (cons x '(2)))))
     
     (println (p1 'dog))
     (println (p2 'cat))
     
     ))

(eval program the-global-environment)


(println "
Part A
======
When for-each is called it in turn makes a call to proc.  So proc will be
evaluated before it is applied and, unless it ignores its arguments, (car
list) will be evaluated during the call.

Part B
======
With the original eval-sequence we get X and (X 2).
With Cy's change we get (X 2) and (X 2).
Where X is the value of evaluating x.

Part C
======
The updated eval-sequence only has an affect if an expression in the
sequence is a thunk, i.e., an argument, but in Part A none of the
expressions in the sequence are thunks.

Part D
======
I wouldn't make Cy's change.  If we have lazy evaluation then then we should
be consistent.  Also, we're predominantly working with a functional paradigm
so making an exception just for the benefit of side effects seems doubly
wrong.  However, we could add the equivalent of force-it to the language so
that we can explicitly call a procedure for its side effects.

")

(--end-- "4.30")

