#lang sicp

(#%require "common.scm")

;   Exercise 5.10
;   =============
;   
;   Design a new syntax for register-machine instructions and modify the
;   simulator to use your new syntax.  Can you implement your new syntax
;   without changing any part of the simulator except the syntax procedures
;   in this section?
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.10]: http://sicp-book.com/book-Z-H-32.html#%_thm_5.10
;   5.2.3 Generating Execution Procedures for Instructions - p529
;   ------------------------------------------------------------------------

(-start- "5.10")

(#%require "machine-10.scm")

(println
 "This is only a trivial change - it allows labels to be referenced directly
without the 'label' tag. E.g.:

  (branch (label base-case)) ==> (branch base-case)

and

  (assign continue (label expn-done)) ==> (assign continue expn-done)

To implement this change only two changes were required:

  (define (label-exp? exp) (tagged-list? exp 'label))
  (define (label-exp-label exp) (cadr exp))

were replaced with:

  (define (label-exp? exp) (symbol? exp))
  (define (label-exp-label exp) exp)

So the controller text for recursive exponentiation becomes:

  (assign continue expn-done)  ;; changed
  test
  (test (op =) (reg n) (const 0))
  (branch base-case)  ;; changed
  (save continue)
  (assign continue after-expn)  ;; changed
  (assign n (op sub) (reg n) (const 1))
  (goto test)  ;; changed
  after-expn
  (restore continue)
  (assign p (op mul) (reg p) (reg b))
  (goto (reg continue))
  base-case
  (assign p (const 1))
  (goto (reg continue))
  expn-done
")

(define (expn b n)
  (let ((machine (make-machine
                  '(b n p continue)
                  (list
                   (list 'sub -)
                   (list 'mul *)
                   (list '= =))
                  '((assign continue expn-done)
                    test
                    (test (op =) (reg n) (const 0))
                    (branch base-case)
                    (save continue)
                    (assign continue after-expn)
                    (assign n (op sub) (reg n) (const 1))
                    (goto test)
                    after-expn
                    (restore continue)
                    (assign p (op mul) (reg p) (reg b))
                    (goto (reg continue))
                    base-case
                    (assign p (const 1))
                    (goto (reg continue))
                    expn-done))))
    (set-register-contents! machine 'b b)
    (set-register-contents! machine 'n n)
    (set-register-contents! machine 'p 1)
    (start machine)
    (get-register-contents machine 'p)))

(println "Evidence we didn't break things (expect 243): " (expn 3 5) "
")

(--end-- "5.10")

