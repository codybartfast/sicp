#lang sicp

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
        (error "Unbound variable" var)
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

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '< <)
        (list '= =)
        (list 'eq? eq?)
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

(define the-global-environment (setup-environment))


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
   (list 'extend-environment extend-environment)
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
   (list 'print display)
   (list 'println (lambda (exp) (display exp) (newline)))
   ;; Ex 5.24
   (list 'cond-clauses cdr)
   (list 'have-clauses? (lambda (exp) (not (null? exp))))
   (list 'cond? (lambda (exp) (tagged-list? exp 'cond)))
   (list 'clauses-first car)
   (list 'clauses-rest cdr)
   (list 'cond-else-clause? (lambda (exp) (eq? (car exp) 'else)))
   (list 'cond-predicate car)
   (list 'cond-actions cdr)
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
    (save env)
    (assign unev (op operands) (reg exp))
    (save unev)
    (assign exp (op operator) (reg exp))
    (assign continue (label ev-appl-did-operator))
    (goto (label eval-dispatch))

    ev-appl-did-operator
    (restore unev)                  ; the operands
    (restore env)
    (assign argl (op empty-arglist))
    (assign proc (reg val))         ; the operator
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
    (perform (op print) (const "ERROR: "))
    (perform (op println) (reg val))
    (goto (label eceval-end))
    
    eceval-done
    (perform (op print) (const "eceval DONE - val: "))
    (perform (op println) (reg val))
    (goto (label eceval-end))

;; =========================================================================
;; End of Text Implementation
;; =========================================================================

;; Ex 5.24 - cond
;; ==============

    ev-cond
    (save continue)
    (assign exp (op cond-clauses) (reg exp))
    ev-cond-have-clause?
    (test (op have-clauses?) (reg exp))
    (branch (label ev-cond-check-clause))
    (goto (label ev-cond-no-clauses))

    ev-cond-check-clause
    (save exp)                                   ;; clauses list
    (assign exp (op clauses-first) (reg exp))
    (save exp)                                   ;; clause
    (test (op cond-else-clause?) (reg exp))
    (branch (label ev-cond-else))
    (save env)
    (assign exp (op cond-predicate) (reg exp))   ;; predicate
    (assign continue (label ev-cond-after-predicate))
    (goto (label eval-dispatch))

    ev-cond-after-predicate
    (restore env)
    (restore exp)                                ;; clause
    (test (op true?) (reg val))
    (branch (label ev-cond-actions))
    (restore exp)                                ;; clauses list
    (assign exp (op clauses-rest) (reg exp))     ;; rest of clauses
    (goto (label ev-cond-have-clause?))

    ev-cond-else
    (restore exp)                                ;; clause
    (assign unev (reg exp))
    (restore exp)                                ;; clauses list
    (assign val (op clauses-rest) (reg exp))
    (test (op have-clauses?) (reg val))
    (branch (label ev-cond-error-else-not-last))
    (save exp)                                   ;; clauses list
    (assign exp (reg unev))                      ;; clause
    ev-cond-actions
    (restore unev)                               ;; discard clauses list
    (assign unev (op cond-actions) (reg exp))     ;; actions
    (assign continue (label ev-cond-after-actions))  ;;; TAIL RECURSION??
    (save continue)
    (goto (label ev-sequence))

    ev-cond-no-clauses
    (assign val (const false)) ;; mimicing behavour of if w/o alternate.
    ev-cond-after-actions
    (restore continue)
    (goto (reg continue))

    ev-cond-error-else-not-last
    (restore (reg continue))
    (assign val (const else-not-last-clause--COND))
    (goto (label signal-error))




;; The End =================================================================
    eceval-end
    ))


(#%provide
 explicit-control-evaluator
 eceval-operations
 the-global-environment
 )
 
  
    
