#lang sicp

(#%require "common.scm")

;   Exercise 5.34
;   =============
;   
;   Compile the iterative factorial procedure
;   
;   (define (factorial n)
;     (define (iter product counter)
;       (if (> counter n)
;           product
;           (iter (* counter product)
;                 (+ counter 1))))
;     (iter 1 1))
;   
;   Annotate the resulting code, showing the essential difference between
;   the code for iterative and recursive versions of factorial that makes
;   one process build up stack space and the other run in constant stack
;   space.
;   
;   Figure:
;   
;   ;; construct the procedure and skip over code for the procedure body
;     (assign val
;             (op make-compiled-procedure) (label entry2) (reg env))
;     (goto (label after-lambda1))
;   
;   entry2     ; calls to factorial will enter here
;     (assign env (op compiled-procedure-env) (reg proc))
;     (assign env
;             (op extend-environment) (const (n)) (reg argl) (reg env))
;   ;; begin actual procedure body
;     (save continue)
;     (save env)
;   
;   ;; compute (= n 1)
;     (assign proc (op lookup-variable-value) (const =) (reg env))
;     (assign val (const 1))
;     (assign argl (op list) (reg val))
;     (assign val (op lookup-variable-value) (const n) (reg env))
;     (assign argl (op cons) (reg val) (reg argl))
;     (test (op primitive-procedure?) (reg proc))
;     (branch (label primitive-branch17))
;   compiled-branch16
;     (assign continue (label after-call15))
;     (assign val (op compiled-procedure-entry) (reg proc))
;     (goto (reg val))
;   primitive-branch17
;     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;   
;   after-call15   ; val now contains result of (= n 1)
;     (restore env)
;     (restore continue)
;     (test (op false?) (reg val))
;     (branch (label false-branch4))
;   true-branch5  ; return 1
;     (assign val (const 1))
;     (goto (reg continue))
;   
;   false-branch4 ;; compute and return (* (factorial (- n 1)) n) (assign
;   proc (op lookup-variable-value) (const *) (reg env)) (save continue)
;     (save proc)   ; save * procedure
;     (assign val (op lookup-variable-value) (const n) (reg env))
;     (assign argl (op list) (reg val))
;     (save argl)   ; save partial argument list for *
;   
;   ;; compute (factorial (- n 1)), which is the other argument for *
;     (assign proc
;             (op lookup-variable-value) (const factorial) (reg env))
;     (save proc)  ; save factorial procedure
;   
;   Figure 5.17: Compilation of the definition of the factorial procedure
;   (continued on next page).
;   
;   Figure:
;   
;   ;; compute (- n 1), which is the argument for factorial
;     (assign proc (op lookup-variable-value) (const -) (reg env))
;     (assign val (const 1))
;     (assign argl (op list) (reg val))
;     (assign val (op lookup-variable-value) (const n) (reg env))
;     (assign argl (op cons) (reg val) (reg argl))
;     (test (op primitive-procedure?) (reg proc))
;     (branch (label primitive-branch8))
;   compiled-branch7
;     (assign continue (label after-call6))
;     (assign val (op compiled-procedure-entry) (reg proc))
;     (goto (reg val))
;   primitive-branch8
;     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;   
;   after-call6   ; val now contains result of (- n 1)
;     (assign argl (op list) (reg val))
;     (restore proc) ; restore factorial
;   ;; apply factorial
;     (test (op primitive-procedure?) (reg proc))
;     (branch (label primitive-branch11))
;   compiled-branch10
;     (assign continue (label after-call9))
;     (assign val (op compiled-procedure-entry) (reg proc))
;     (goto (reg val))
;   primitive-branch11
;     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;   
;   after-call9      ; val now contains result of (factorial (- n 1))
;     (restore argl) ; restore partial argument list for *
;     (assign argl (op cons) (reg val) (reg argl))
;     (restore proc) ; restore *
;     (restore continue)
;   ;; apply * and return its value
;     (test (op primitive-procedure?) (reg proc))
;     (branch (label primitive-branch14))
;   compiled-branch13
;   ;; note that a compound procedure here is called tail-recursively
;     (assign val (op compiled-procedure-entry) (reg proc))
;     (goto (reg val))
;   primitive-branch14
;     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;     (goto (reg continue))
;   after-call12
;   after-if3
;   after-lambda1
;   ;; assign the procedure to the variable factorial
;     (perform
;      (op define-variable!) (const factorial) (reg val) (reg env))
;     (assign val (const ok))
;   
;   Figure 5.17: (continued)
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.34]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.34
;   5.5.5 An Example of Compiled Code - p594
;   ------------------------------------------------------------------------

(-start- "5.34")

(println
 "
The annotation below is probably more verbose than intended by the question
because I found it a useful way to understand the process.

In the 'recursive' version the recursive call is here:

   compiled-branch10
     (assign continue (label after-call9))
     (assign val (op compiled-procedure-entry) (reg proc))
     (goto (reg val))

This call is made during the evaluation of the arguments to '*'.  As such
proc (*), argl (value of n-1), env and continue are all stored on the stack
before the call is made.

By contrast, in the 'iterative' version the recursive call is made here:

  compiled-branch21
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))

Because the application of the factorial is the last action in the procedure
(instead of an intermediate action in the evaluation of arguments) no state
is saved.

Anotated Iterative Process
==========================

  ;; Construct 'factorial' procedure and skip over its code
    (assign val (op make-compiled-procedure) (label entry1) (reg env))
    (goto (label after-lambda2))

  ;; Calls to 'factorial' procedure enter here
entry1
  ;; Set main procedure env and extend with arguments
    (assign env (op compiled-procedure-env) (reg proc))
    (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  ;; Construct 'iter' procedure and skip over its code
    (assign val (op make-compiled-procedure) (label entry3) (reg env))
    (goto (label after-lambda4))

  ;; Calls to 'iter' procedure enter here
entry3

  ;; Set 'iter' procedure env and extend with arguments
    (assign env (op compiled-procedure-env) (reg proc))
    (assign env
      (op extend-environment) (const (product counter))
                              (reg argl)
                              (reg env))

  ;; We need env after calling '>' and continue to return to caller
    (save continue)
    (save env)

  ;; Assign '>' to proc
    (assign proc (op lookup-variable-value) (const >) (reg env))

  ;; Create argl for call to '>'
  ;; Lookup 'n' for LAST argument
    (assign val (op lookup-variable-value) (const n) (reg env))
    (assign argl (op list) (reg val))

  ;; Lookup counter
    (assign val (op lookup-variable-value) (const counter) (reg env))
    (assign argl (op cons) (reg val) (reg argl))

  ;; \"Apply-dispatch\" - check if '>' is primitive
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch8))

  ;; Branch taken if '>' is a compiled ('compound') procedure
compiled-branch9
    (assign continue (label after-call10))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))

  ;; Branch taken if '>' is a primitive procedure
primitive-branch8
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  ;; After call to '>', if's predicate result is in val
after-call10
    (restore env)
    (restore continue)

  ;; Goto to if's consequent or alternative branch
    (test (op false?) (reg val))
    (branch (label false-branch6))

  ;; If's consequent branch
  ;; Assign product to val and return to caller
true-branch5
    (assign val (op lookup-variable-value) (const product) (reg env))
    (goto (reg continue))

  ;; If's alternative branch
  ;; Make iterative call to 'iter' proc
false-branch6

  ;; Lookup 'iter' procedure
    (assign proc (op lookup-variable-value) (const iter) (reg env))

  ;; Need to protect these while evaluating argl
    (save continue)
    (save proc)
    (save env)

  ;; Lookup '+' for LAST argument
    (assign proc (op lookup-variable-value) (const +) (reg env))
    (assign val (const 1))

  ;; Create argl
    (assign argl (op list) (reg val))

  ;; Lookup 'counter'
    (assign val (op lookup-variable-value) (const counter) (reg env))
    (assign argl (op cons) (reg val) (reg argl))

  ;; \"Apply-dispatch\" - check if '+' is primitive
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch14))

  ;; Branch taken if '+' is a compiled ('compound') procedure
compiled-branch15
    (assign continue (label after-call16))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))

  ;; Branch taken if '+' is a primitive procedure
primitive-branch14
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  ;; After call to '+' put result in argl
after-call16
    (assign argl (op list) (reg val))
    (restore env)
    (save argl)

  ;; Evaluate first argument by evaluating '*'
    (assign proc (op lookup-variable-value) (const *) (reg env))
    (assign val (op lookup-variable-value) (const product) (reg env))
    (assign argl (op list) (reg val))
    (assign val (op lookup-variable-value) (const counter) (reg env))
    (assign argl (op cons) (reg val) (reg argl))

  ;; \"Apply-dispatch\" - check if '*' is primitive
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch11))

  ;; Branch if '*' is a compiled procedure
compiled-branch12
    (assign continue (label after-call13))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))

  ;; Branch if '*' is a primitive procedure
primitive-branch11
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  ;; After evaluating second arg to '*', result in val
  ;; Add that to argl
after-call13
    (restore argl)
    (assign argl (op cons) (reg val) (reg argl))
    (restore proc)
    (restore continue)

  ;; \"Apply-dispatch\" - check if 'iter' is a compiled procedure
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch17))

  ;; Branch that will be called for iterative 'iter' - unless it is modified
compiled-branch18
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))

  ;; Branch that would be called for iterative 'iter' if it were primitive
primitive-branch17
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (goto (reg continue))

  ;; Nothing after iterative call to 'iter'
after-call19

  ;; Nothing after 'if'
after-if7

  ;; After lambda for 'iter'
  ;; Assign procedure to variable 'iter'
after-lambda4
    (perform (op define-variable!) (const iter) (reg val) (reg env))

  ;; Put return value 'ok' in to val
    (assign val (const ok))

  ;; Make initial procedure call to 'iter' \"(iter 1 1)\"
    (assign proc (op lookup-variable-value) (const iter) (reg env))
    (assign val (const 1))
    (assign argl (op list) (reg val))
    (assign val (const 1))
    (assign argl (op cons) (reg val) (reg argl))
  ;; \"Apply-dispatch\" - check if 'iter' is a primitive procedure
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch20))

compiled-branch21
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))

  ;; Branch that would be taken if initial 'iter' were primitive
primitive-branch20
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (goto (reg continue))

  ;; Nothing after initial call to 'iter'
after-call22

  ;; assign the factorial procedure to the variable 'factorial'
after-lambda2
    (perform (op define-variable!) (const factorial) (reg val) (reg env))
    (assign val (const ok))
")

;(#%require "compiler-33.scm")
;
;(compile
; '(define (factorial n)
;    (define (iter product counter)
;      (if (> counter n)
;          product
;          (iter (* counter product)
;                (+ counter 1))))
;    (iter 1 1))
; 'val
; 'next)

(--end-- "5.34")

