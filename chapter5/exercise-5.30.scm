#lang sicp

(#%require "common.scm")

;   Exercise 5.30
;   =============
;
;   Our evaluator currently catches and signals only two kinds of errors --
;   unknown expression types and unknown procedure types.  Other errors will
;   take us out of the evaluator read-eval-print loop.  When we run the
;   evaluator using the register-machine simulator, these errors are caught
;   by the underlying Scheme system.  This is analogous to the computer
;   crashing when a user program makes an error.⁽³²⁾ It is a large project
;   to make a real error system work, but it is well worth the effort to
;   understand what is involved here.
;
;   a. Errors that occur in the evaluation process, such as an attempt to
;   access an unbound variable, could be caught by changing the lookup
;   operation to make it return a distinguished condition code, which cannot
;   be a possible value of any user variable.  The evaluator can test for
;   this condition code and then do what is necessary to go to signal-error.
;   Find all of the places in the evaluator where such a change is necessary
;   and fix them.  This is lots of work.
;
;   b. Much worse is the problem of handling errors that are signaled by
;   applying primitive procedures, such as an attempt to divide by zero or
;   an attempt to extract the car of a symbol.  In a professionally written
;   high-quality system, each primitive application is checked for safety as
;   part of the primitive.  For example, every call to car could first check
;   that the argument is a pair.  If the argument is not a pair, the
;   application would return a distinguished condition code to the
;   evaluator, which would then report the failure.  We could arrange for
;   this in our register-machine simulator by making each primitive
;   procedure check for applicability and returning an appropriate
;   distinguished condition code on failure. Then the primitive-apply code
;   in the evaluator can check for the condition code and go to signal-error
;   if necessary.  Build this structure and make it work. This is a major
;   project.
;
;   ------------------------------------------------------------------------
;   [Exercise 5.30]: http://sicp-book.com/book-Z-H-34.html#%_thm_5.30
;   [Footnote 32]:   http://sicp-book.com/book-Z-H-34.html#footnote_Temp_793
;   5.4.4 Running the Evaluator - p565
;   ------------------------------------------------------------------------

(-start- "5.30")

(println
 "
Part A
======

We need to use an object other than a symbol to tag an error and then use
referential equality, eq?, for comparison.  Otherwise it is possible to
construct an variable value that could be mistken for an error (because
symbols are interened).

  (eq? 'error 'error) is true, but

  (eq? '(error) '(error)) is false.

Hence:

 (define error-obj
   '(error-obj))

To minimize changes in the ec-evaluator just two new primitive operations
were needed:

(define (is-error? val)
  (and (pair? val)
       (eq? (car val) error-obj)))

(define (display-error error)
  (display (cadr error))
  (display \". Args:\")
  (map (lambda (arg)
         (display \" '\")
         (display arg)
         (display \"'\"))
       (caddr error)))

Then the ec-evaluator's signal-error can call display-error if val is an
error.  E.g.

Check if the response is an error:

  ev-variable
    (assign val (op lookup-variable-value) (reg exp) (reg env))
    (test (op is-error?) (reg val))
    (branch (label unbound-variable))
    (goto (reg continue))

In the error method put the error type in the unev instead of val:

  unbound-variable
    (assign unev (const unbound-variable-error))
    (goto (label signal-error))

Signal error then calls display-error if it's a tagged error otherwise it
just displays the error type as usuaal:

  signal-error
    (perform (op display) (const \"ERROR: \"))
    (test (op is-error?) (reg val))
    (branch (label signal-error-with-error-obj))
    (perform (op displayln) (reg val))
    (goto (label eceval-end))

  signal-error-with-error-obj
    (perform (op display) (reg unev))
    (perform (op display) (const \", \"))
    (perform (op display-error) (reg val))
    (perform (op displayln) (const \"\"))
    (goto (label eceval-end))

The primitive procedures and operations call a make-error method:

  (define (make-error message args)
    (list error-obj message args))

  (define (lookup-variable-value var env)
              ...
      (if (eq? env the-empty-environment)
        (make-error \"unbound variable:\" (list var))
        ...

Demo
----

This code assigns a structurally identical object to the variable b and then
retrieves it successfully without it being considered an error:

  (define a 'apple)
  (define b (list '(error-obj) \"Unbound variable:\" (list 'b)))
  (define c 'cherry)
  (list a b c )

Output:

  DONE: (apple ((error-obj) Unbound variable: (b)) cherry)

This demonstrates the behaviour when a variable is unbound:

  (define a 'apple)
  (define c 'cherry)
  (list a b c)

Output:

  ERROR: unbound-variable-error, unbound variable:. Args: 'b'
")

(#%require "machine-19.scm")
(#%require "ec-evaluator-30.scm")

(define (run prog)
  (define (printReg reg before after)
    (println "--reg--: " reg ": " before " --> " after))
  (let ((eceval
         (make-machine
          eceval-operations
          explicit-control-evaluator)))

    (set-register-contents! eceval 'exp prog)
    (set-register-contents! eceval 'env (the-global-environment))
    (ignore (start eceval))))

(define prog1
  '(begin
     (define a 'apple)
     (define b (list '(error-obj) "Unbound variable:" (list 'b)))
     (define c 'cherry)
     (list a b c )))

(define prog2
  '(begin
     (define a 'apple)
     (define c 'cherry)
     (list a b c)))

(run prog1)
(run prog2)

(println
 "
Part B
======

The same error handling can be used to check calls to primitive methods.
Nearly all the checking of arguments to primitive procedure is checking the
number of arguments and their type.  A few helper methods:

  (define (arg-length-not args len proc-name)
  (define (arg-length-less-than args len proc-name)
  (define (not-all-args-satisfy all-args check check-desc proc-name)

can then create relevant errors with detailed messages for a renge of
'checked' primitive procedures (the above return #true when there's a
problem).  E.g.:

  (define (checked-subtract . args)
    (cond ((arg-length-less-than args 1 \"-\"))
          ((not-all-args-satisfy args number? \"numbers\" \"-\"))
          (else (apply - args))))

These checked procedures can then be installed in the global environment
instead of the raw ones:

  (define primitive-procedures
    (list (list 'car checked-car)
          (list 'cdr checked-cdr)
          (list 'cons checked-cons)
          (list 'null? checked-null?)
          (list 'list list)
          (list '+ checked-+)
          (list '- checked-subtract)
          (list '* checked-*)
          (list '/ checked-/)
          (list '< checked-<)
          (list '> checked->)
          (list '= checked-=)
          (list 'eq? checked-eq?)

Demo
====

  (- 'not-a-number)

Output:

  ERROR: primitive-procedure-error, - requires numbers. Args: 'not-a-number'

and

  (-)

Output:

  ERROR: primitive-procedure-error, - expected at least 1 args, but got 0.

")

(define prog3
  '(begin
     (- 'not-a-number)))
(run prog3)

(define prog4
  '(begin
     (-)))
(run prog4)

(--end-- "5.30")

