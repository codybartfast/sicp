#lang sicp

;; Based on ec-evaluator-30, for Ex 5.32, adds optimization for simple apply
;; where where procedure expression is a symbol.

;; =========================================================================
;; Primitive Operations
;; =========================================================================
;;
;; If I understand correctly, these would normally be implemented on the
;; ecval machine, but are instead provided here to simplify the
;; implementation of the machine, and to clarify its overall structure.
;;
;; Not to be confused with Primitive PROCEDURES, which are always
;; implemented outside of the machine.
;;
;; Apart from reversing arguments in apply-primitive-procedure, and adding
;; some extra primitve procedures this is just lifted from Section 4.1.

;; 4.1.2  Representing Expressions

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;; 4.1.3  Evaluator Data Structures

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (make-error "unbound variable:" (list var))
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; 4.1.4  Running the Evaluator as a Program

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;; Ex 5.30 helper methods for checking primitive procedures

(define (arg-length-not args len proc-name)
  (if (not (= (length args) len))
      (make-error
       (string-append
        proc-name " expected " (number->string len)
        " args, but got " (number->string (length args))) args)
      #f))

(define (arg-length-less-than args len proc-name)
  (if (< (length args) len)
      (make-error
       (string-append
        proc-name " expected at least " (number->string len)
        " args, but got " (number->string (length args))) args)
      #f))

(define (not-all-args-satisfy all-args check check-desc proc-name)
  (define (iter args)
    (if (null? args)
        #f
        (if (check (car args))
            (iter (cdr args))
            (make-error
             (string-append
              proc-name " requires " check-desc)
             all-args))))
  (iter all-args))

;; Ex 5.30 checked versions of primitive procedures

(define (checked-car . args)
  (cond ((arg-length-not args 1 "car") => values)
        ((not-all-args-satisfy args pair? "a pair" "car"))
        (else (apply car args))))

(define (checked-cdr . args)
  (cond ((arg-length-not args 1 "cdr") => values)
        ((not-all-args-satisfy args pair? "a pair" "cdr"))
        (else (apply cdr args))))

(define (checked-cons . args)
  (cond ((arg-length-not args 2 "cons") => values)
        (else (apply cons args))))

(define (checked-null? . args)
  (cond ((arg-length-not args 1 "null?") => values)
        (else (apply null? args))))

(define (checked-+ . args)
  (cond ((not-all-args-satisfy args number? "numbers" "+"))
        (else (apply + args))))

(define (checked-subtract . args)
  (cond ((arg-length-less-than args 1 "-") => values)
        ((not-all-args-satisfy args number? "numbers" "-") => values)
        (else (apply - args))))

(define (checked-* . args)
  (cond ((not-all-args-satisfy args number? "numbers" "*"))
        (else (apply * args))))

(define (checked-/ . args)
  (define (non-zero? n) (not (zero? n)))
  (cond
    ((arg-length-less-than args 1 "/"))
    ((not-all-args-satisfy args number? "numbers" "/"))
    ((not-all-args-satisfy (cdr args) non-zero? "non-zero divisors" "/"))
    ((and (= (length args) 1)
           (not-all-args-satisfy args non-zero? "non-zero divisors" "/")))
     (else (apply / args))))

(define (checked-< . args)
  (cond ((arg-length-less-than args 1 "<"))
        ((not-all-args-satisfy args real? "real numbers" "<"))
        (else (apply < args))))

(define (checked-> . args)
  (cond ((arg-length-less-than args 1 ">"))
        ((not-all-args-satisfy args real? "real numbers" ">"))
        (else (apply > args))))

(define (checked-= . args)
  (cond ((arg-length-less-than args 1 "="))
        ((not-all-args-satisfy args number? "numbers" "="))
        (else (apply = args))))

(define (checked-eq? . args)
  (cond ((arg-length-not args 2 "eq?"))
        (else (apply eq? args))))

;; Ex 5.30 checked versions of methods installed

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

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
        ))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc)
   (reverse args))) ;; <-- reverse arguments for eceval

(define (the-global-environment) (setup-environment))


;; Error handling for Ex 5.30
;; ==========================

(define error-obj
  '(error-obj))
(define (make-error message args)
  (list error-obj message args))
(define (is-error? val)
  (and (pair? val)
       (eq? (car val) error-obj)))
(define (display-error error)
  (display (cadr error))
  (display ". Args:")
  (map (lambda (arg)
         (display " '")
         (display arg)
         (display "'"))
       (caddr error)))


;; Primitive Operations
;; ====================
;;
;; Listed in the order that errors arose when trying to run the eceval
;; machine without any primitive operations.

(define eceval-operations
  (list
   (list 'self-evaluating? self-evaluating?)
   (list 'variable? symbol?)
   (list 'quoted? (lambda (exp) (tagged-list? exp 'quote)))
   (list 'assignment? (lambda (exp) (tagged-list? exp 'set!)))
   (list 'definition? definition?)
   (list 'if? (lambda (exp) (tagged-list? exp 'if)))
   (list 'lambda? (lambda (exp) (tagged-list? exp 'lambda)))
   (list 'begin? (lambda (exp) (tagged-list? exp 'begin?)))
   (list 'application? pair?)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'text-of-quotation cadr)
   (list 'lambda-parameters cadr)
   (list 'lambda-body cddr)
   (list 'make-procedure make-procedure)
   (list 'operator car)
   (list 'operands cdr)
   (list 'empty-arglist (lambda () '()))
   (list 'no-operands? null?)
   (list 'first-operand car)
   (list 'last-operand? (lambda (exp) (null? (cdr exp))))
   (list 'adjoin-arg cons)
   (list 'rest-operands cdr)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'compound-procedure? (lambda (exp) (tagged-list? exp 'procedure)))
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'procedure-parameters cadr)
   (list 'procedure-environment cadddr)
   (list 'extend-environment
         (lambda (vars vals base-env)
            (extend-environment
             vars
             (reverse vals)              ;; <-- reverse arguments
             base-env)))
   (list 'procedure-body caddr)
   (list 'begin-actions cdr)
   (list 'first-exp car)
   (list 'last-exp? (lambda (exp) (null? (cdr exp))))
   (list 'rest-exps cdr)
   (list 'if-predicate cadr)
   (list 'true? (lambda (exp) (not (eq? exp #false))))
   (list 'if-alternative if-alternative)
   (list 'if-consequent caddr)
   (list 'assignment-variable cadr)
   (list 'assignment-value caddr)
   (list 'set-variable-value! set-variable-value!)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'define-variable! define-variable!)
   (list 'add-binding-to-frame! add-binding-to-frame!)
   (list 'display display)
   (list 'displayln (lambda (exp) (display exp) (newline)))
   ;; Ex 5.24
   (list 'cond-clauses cdr)
   (list 'no-clauses? null?)
   (list 'cond? (lambda (exp) (tagged-list? exp 'cond)))
   (list 'clauses-first car)
   (list 'clauses-rest cdr)
   (list 'cond-else-clause? (lambda (exp) (eq? (car exp) 'else)))
   (list 'cond-predicate car)
   (list 'cond-actions cdr)
   ; Ex 5.30
   (list 'eq? eq?)
   (list 'is-error? is-error?)
   (list 'display-error display-error)
   ))


;; =========================================================================
;; Primitive Operations
;; =========================================================================
;;
;; Copied from Section 5.4 The Explicit-Control Evaluator, with a few
;; small changes (2 lines at the top, and 7 bottom as I'm not
;; running from a REPL.

(define explicit-control-evaluator
  '(
    (assign continue (label eceval-done))
    (goto (label ev-begin))

    ;; 5.4.1 The Core of the Evaluator
    ;; ===============================

  eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))
    (test (op variable?) (reg exp))
    (branch (label ev-variable))
    (test (op quoted?) (reg exp))
    (branch (label ev-quoted))
    (test (op assignment?) (reg exp))
    (branch (label ev-assignment))
    (test (op definition?) (reg exp))
    (branch (label ev-definition))
    (test (op if?) (reg exp))
    (branch (label ev-if))
    (test (op cond?) (reg exp))
    (branch (label ev-cond))
    (test (op lambda?) (reg exp))
    (branch (label ev-lambda))
    (test (op begin?) (reg exp))
    (branch (label ev-begin))
    (test (op application?) (reg exp))
    (branch (label ev-application))
    (goto (label unknown-expression-type))

    ;; Evaluating Simple Expressions

  ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))
  ev-variable
    (assign val (op lookup-variable-value) (reg exp) (reg env))
    (test (op is-error?) (reg val))
    (branch (label unbound-variable))
    (goto (reg continue))
  ev-quoted
    (assign val (op text-of-quotation) (reg exp))
    (goto (reg continue))
  ev-lambda
    (assign unev (op lambda-parameters) (reg exp))
    (assign exp (op lambda-body) (reg exp))
    (assign val (op make-procedure)
            (reg unev) (reg exp) (reg env))
    (goto (reg continue))

    ;; Evaluating procedure applications

  ev-application
    (save continue)
    (assign unev (op operands) (reg exp))
    (assign exp (op operator) (reg exp))
    (test (op variable?) (reg exp))
    (branch (label ev-appl-operator-lookup))
    (save env)
    (save unev)
    (assign continue (label ev-appl-did-operator-eval))
    (goto (label eval-dispatch))

  ev-appl-operator-lookup
    (assign continue (label ev-appl-did-operator-lookup))
    (goto (label ev-variable))

  ev-appl-did-operator-eval
    (restore unev)
    (restore env)
  ev-appl-did-operator-lookup
    (assign argl (op empty-arglist))
    (assign proc (reg val))
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)

  ev-appl-operand-loop
    (save argl)
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label ev-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label ev-appl-accumulate-arg))
    (goto (label eval-dispatch))

  ev-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))

  ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))
    ev-appl-accum-last-arg
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)
    (goto (label apply-dispatch))

    ;; Procedure application

  apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label compound-apply))
    (goto (label unknown-procedure-type))

  primitive-apply
    (assign val (op apply-primitive-procedure)
            (reg proc)
            (reg argl))
    (test (op is-error?) (reg val))
    (branch (label primitive-procedure-error))
    (restore continue)
    (goto (reg continue))

  compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env (op extend-environment)
            (reg unev) (reg argl) (reg env))
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))

    ;; 5.4.2 Sequence Evaluation and Tail Recursion

  ev-begin
    (assign unev (op begin-actions) (reg exp))
    (save continue)
    (goto (label ev-sequence))

  ev-sequence
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch (label ev-sequence-last-exp))
    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))
  ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))
  ev-sequence-last-exp
    (restore continue)
    (goto (label eval-dispatch))


    ;; 5.4.3 Conditionals, Assignments and Definitions
    ;; ===============================================

  ev-if
    (save exp)                    ; save expression for later
    (save env)
    (save continue)
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label eval-dispatch))  ; evaluate the predicate

  ev-if-decide
    (restore continue)
    (restore env)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label ev-if-consequent))

  ev-if-alternative
    (assign exp (op if-alternative) (reg exp))
    (goto (label eval-dispatch))
  ev-if-consequent
    (assign exp (op if-consequent) (reg exp))
    (goto (label eval-dispatch))

    ;; Assignments and definitions
    ;; ===========================

  ev-assignment
    (assign unev (op assignment-variable) (reg exp))
    (save unev)                   ; save variable for later
    (assign exp (op assignment-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-assignment-1))
    (goto (label eval-dispatch))  ; evaluate the assignment value
  ev-assignment-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform
     (op set-variable-value!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))

  ev-definition
    (assign unev (op definition-variable) (reg exp))
    (save unev)                   ; save variable for later
    (assign exp (op definition-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-definition-1))
    (goto (label eval-dispatch))  ; evaluate the definition value
  ev-definition-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform
     (op define-variable!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))

  unknown-expression-type
    (assign val (const unknown-expression-type-error))
    (goto (label signal-error))
  unknown-procedure-type
    (restore continue)    ; clean up stack (from apply-dispatch)
    (assign val (const unknown-procedure-type-error))
    (goto (label signal-error))
  signal-error
    ;; These last 7 lines differ from book because not in REPL
    (perform (op display) (const "ERROR: "))
    (test (op is-error?) (reg val))
    (branch (label signal-error-with-error-obj))
    (perform (op displayln) (reg val))
    (goto (label eceval-end))

  signal-error-with-error-obj
    (perform (op display) (reg unev))
    (perform (op display) (const ", "))
    (perform (op display-error) (reg val))
    (perform (op displayln) (const ""))
    (goto (label eceval-end))

  eceval-done
    (perform (op display) (const "DONE: "))
    (perform (op displayln) (reg val))
    (goto (label eceval-end))

;; =========================================================================
;; End of Text Implementation
;; =========================================================================

;; Ex 5.24 - cond
;; ==============

  ev-cond
    (save continue)                               ; save final destination
    (assign exp (op cond-clauses) (reg exp))      ; drop cond label
  ev-cond-have-clause?
    (test (op no-clauses?) (reg exp))             ; no clauses?
    (branch (label ev-cond-no-clauses))           ;      --> no clauses
    (assign unev (op clauses-first) (reg exp))    ; get first clause
    (test (op cond-else-clause?) (reg unev))      ; else clause?
    (branch (label ev-cond-else))                 ;      --> else
    (save exp)                                    ; save clauses list
    (save unev)                                   ; save clause
    (save env)                                    ; save env
    (assign exp (op cond-predicate) (reg unev))   ; get predicate
    (assign continue (label ev-cond-after-predicate))
    (goto (label eval-dispatch))                  ; --> eval predicate

  ev-cond-after-predicate
    (restore env)                                 ; restore env
    (restore unev)                                ; restore clause
    (restore exp)                                 ; restore clauses list
    (test (op true?) (reg val))                   ; is predicate true?
    (branch (label ev-cond-actions))              ;      --> actions
    (assign exp (op clauses-rest) (reg exp))      ; drop first clause
    (goto (label ev-cond-have-clause?))           ; --> try-again

  ev-cond-else
    (assign val (op clauses-rest) (reg exp))      ; get clauses after else
    (test (op no-clauses?) (reg val))             ; no clauses after else?
    (branch (label ev-cond-actions))              ;      --> actions
    (restore (reg continue))                      ; restore continue
    (assign val (const ELSE-clause-isnt-last--COND))
    (goto (label signal-error))                   ; --> raise error

  ev-cond-actions
    (assign unev (op cond-actions) (reg unev))    ; store actions for ev-seq
    (goto (label ev-sequence))                    ; --> ev-sequence

  ev-cond-no-clauses
    (restore continue)                            ; restore continue
    (assign exp (const false))                    ; name of false variable
    (goto (label ev-variable))                    ; --> lookup false value

;; Ex 5.30
;; =======

  unbound-variable
    (assign unev (const unbound-variable-error))
    (goto (label signal-error))

  primitive-procedure-error
    (assign unev (const primitive-procedure-error))
    (goto (label signal-error))


;; The End =================================================================
  eceval-end
  ))


(#%provide
 explicit-control-evaluator
 eceval-operations
 the-global-environment)



