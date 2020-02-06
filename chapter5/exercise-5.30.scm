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

If we use an 'object' as the condition code then, even if a variable value
is structurally identical, it will not have referential equality. E.g. we
could use:

  (define lookup-variable-value-error-obj
    '(lookup-variable-value-error-obj))

And in lookup-variable-value we can return this instead of raising an error:

  (define (lookup-variable-value var env)
              ...
      (if (eq? env the-empty-environment)
          lookup-variable-value-error-obj
          ...

We then make this object available to the ec-evaluator by adding an accessor
to the list of primitive operations along with eq? to check reference
equality:

  (define eceval-operations
     ...
     (list 'eq? eq?)
     (list 'lookup-variable-value-error-obj
           (lambda () lookup-variable-value-error-obj))
     ...

Then in the ec-evaluator we can check if lookup-variable-value returns this
specific object:

  ev-variable
    (assign val (op lookup-variable-value) (reg exp) (reg env))
    (assign unev (op lookup-variable-value-error-obj))
    (test (op eq?) (reg val) (reg unev))
    (branch (label unbound-variable))
    (goto (reg continue))

And branches to unbound-variable:

  unbound-variable
    (assign val (const unbound-variable-error))
    (assign unev (reg exp))
    (goto (label signal-error-detail))

in order to display both the type of error and the error detail (the
unbound variable) I added signal-error-detail that also displays the
contents of unev register:

  signal-error-detail
    (perform (op print) (const \"ERROR: \"))
    (perform (op print) (reg val))
    (perform (op print) (const \": \"))
    (perform (op println) (reg unev))
    (goto (label eceval-end))

Demo
----

This code assigns a structurally identical object to the variable b and then
retrieves it successfully

  (define a 'apple)
  (define b '(lookup-variable-value-error-obj))
  (define c 'cherry)
  (cons a (cons b (cons c '())))

Output:

  eceval DONE - val: (apple (lookup-variable-value-error-obj) cherry)

This tests the behaviour when a variable is unbound:

  (define a 'apple)
  (define c 'cherry)
  (cons a (cons b (cons c '())))

Output:

  ERROR: unbound-variable-error: b


 ")


(#%require "machine-19.scm")
(#%require "ec-evaluator-30.scm")

(define prog1
  '(begin
     (define a 'apple)
     (define b (list '(error-obj) "Unbound variable:" (list 'b)))
     (define c 'cherry)
     (cons a (cons b (cons c '())))
     ))

(define prog2
  '(begin
     (define a 'apple)
     (define c 'cherry)
     (cons a (cons b (cons c '())))
     ))

(define (run prog)
  (define (printReg reg before after)
    (println "--reg--: " reg ": " before " --> " after))
  (let ((eceval
         (make-machine
          eceval-operations
          explicit-control-evaluator)))

    (set-register-contents! eceval 'exp prog)
    (set-register-contents! eceval 'env (the-global-environment))
;    (trace-on! eceval println)
;    (reg-trace-on! eceval 'exp printReg)
;    (reg-trace-on! eceval 'proc printReg)
;    (reg-trace-on! eceval 'argl printReg)
;    (reg-trace-on! eceval 'env printReg)
;    (reg-trace-on! eceval 'val printReg)
;    (reg-trace-on! eceval 'unev printReg)

    (ignore (start eceval))
    ;(println (stack-stats eceval))
    ))

(run prog1)
(run prog2)

(--end-- "5.30")

