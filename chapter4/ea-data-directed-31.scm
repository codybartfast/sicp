#lang sicp

;; data-directed-24 with a few extra primitives to run Ex 1.22 code.

;; 'Logging' for debug use ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define debug false)

(define (log . parts)
  (if debug
      (underlying-apply writeln parts)))

(define (writeln . parts)
  (for-each display parts)
  (newline))

;; Data-Directed Eval ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(#%require "ea-underlying-apply.scm")
(#%require "ea-evaluators.scm")

(define expression-type car)

(define (eval exp env)
  (log "evaluating: " exp)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    (else
     (if (pair? exp)
         (let ((evaluator (get 'eval (expression-type exp))))
           (if evaluator
               (evaluator exp env)
               (begin
                 (log "about to apply: " exp)
                 (apply (actual-value (operator exp) env)
                        (operands exp)
                        env))))
         ((error "Unknown expression type -- EVAL" exp))))))

(define (put-evaluators)
  (define (eval-lambda exp env)
    (make-procedure (lambda-parameter-defs exp)
                    (lambda-body exp)
                    env))
  (define (eval-begin exp env)
    (eval-sequence (begin-actions exp) env))
  (define (eval-cond exp env)
    (eval (cond->if exp) env))

  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda eval-lambda)
  (put 'eval 'begin eval-begin)
  (put 'eval 'cond eval-cond)
  
  (put 'eval 'and eval-and)
  (put 'eval 'or eval-or)
  (put 'eval 'let eval-let)
  (put 'eval 'let* eval-let*)
  (put 'eval 'letrec eval-letrec)
  (put 'eval 'unbind! eval-unbind!)
  (put 'eval 'delay eval-delay)
  (put 'eval 'force eval-force)
  (put 'eval 'cons-stream eval-cons-stream)
  (put 'eval 'stream-null? eval-stream-null?)
  (put 'eval 'stream-car eval-stream-car)
  (put 'eval 'stream-cdr eval-stream-cdr)
  )


;; Shared by exercise extensions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-call cons)
(define (make-definition name params body)
  (cons 'define
        (cons (cons name params)
              body)))

;; Ex 4.04 and, or ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define first-predicate cadr)
(define second-predicate caddr)

(define (eval-and exp env)
  (if (true? (eval (first-predicate exp) env))
           (true? (eval (second-predicate exp) env))
           false))

(define (eval-or exp env)
  (if (true? (eval (first-predicate exp) env))
           true
           (true? (eval (second-predicate exp) env))))

;; Ex 4.05 calling-cond ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (calling-cond? exp)
  (eq? (cadr exp) '=>))
(define calling-cond-actions cddr)

(define (clause->exp clause predicate-value)
  (if (calling-cond? clause)
      (make-call (sequence->exp (calling-cond-actions clause))
                 ;; ... with predicate value
                 (list predicate-value))
      (sequence->exp (cond-actions clause))))

;; Ex 4.06-4.08 let, let*, named-let, letrec;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; let helpers
(define let-name cadr)
(define (named-let? exp)
  (symbol? (let-name exp)))
(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))
(define (let-pairs exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))

(define let-pair-id car)
(define let-pair-value cadr)
(define (let-params exp)
  (map let-pair-id
       (let-pairs exp)))
(define (let-values exp)
  (map let-pair-value
       (let-pairs exp)))

;; let, named-let
(define (let->combination exp )
  (if (named-let? exp)
      (make-begin
       (list
        (make-definition (let-name exp)
                         (let-params exp)
                         (let-body exp))
        (make-call (let-name exp)
                   (let-values exp))))
      (make-call
       (make-lambda (let-params exp)
                    (let-body exp))
       (let-values exp))))

(define (eval-let exp env)
  (eval (let->combination exp) env))

;; let*
(define (make-let pairs body)
  (cons 'let (cons pairs body)))

(define (let*->nested-lets exp)
  (define (wrap-lets pairs body)
    (make-let (list (car pairs))
              (if (pair? (cdr pairs))
                  (list (wrap-lets (cdr pairs) body))
                  body)))
  (let ((pairs (let-pairs exp)))
    (if (pair? pairs)
        (wrap-lets pairs (let-body exp))
        (make-let pairs (let-body exp)))))
        
(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))

;; letrec
(define (letrec->let exp)
  (make-let
   (map (lambda (pair) (list (let-pair-id pair) '*unassigned*))
        (let-pairs exp))
   (append (map (lambda (pair)
                  (list 'set! (let-pair-id pair) (let-pair-value pair)))
                (let-pairs exp))
           (let-body exp))))

(define (eval-letrec exp env)
  (eval (letrec->let exp) env))

;; Ex 4.13 Unbind! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-frame-var var frame)
  (define (scan frame-pairs)
    (let ((rest (cdr frame-pairs)))
      (cond ((null? rest)
             #f)
            ((eq? var (car (car rest)))
             (set-cdr! frame-pairs (cddr frame-pairs))
             #t)
            (else (scan (cdr frame-pairs))))))
  (scan frame))
              
(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (if (not (remove-frame-var var frame))
        (error "Unbound variable -- UNBIND!" var))))

(define (eval-unbind! exp env)
  (make-unbound! (cadr exp) env))

;; Ex 4.18 Stream primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delay
(define (delay->lambda exp)
  (make-lambda '() (cdr exp)))
(define (eval-delay exp env)
  (eval (delay->lambda exp) env))

;; force
(define (eval-force exp env)
  (eval (cdr exp) env))

;; cons-stream
(define (cons-stream->cons exp)
  (list 'cons (cadr exp) (list 'delay (caddr exp))))
(define (eval-cons-stream exp env)
  (eval (cons-stream->cons exp) env))

;; stream-null?
(define (stream-null?->null? exp)
  (cons 'null? (cdr exp)))
(define (eval-stream-null? exp env)
  (eval (stream-null?->null? exp) env))

;; stream-car
(define (eval-stream-car exp env)
  (eval (cons 'car (cdr exp)) env))
  
;; stream-cdr
(define (eval-stream-cdr exp env)
  (eval (list 'force (list 'cdr (cadr exp))) env))

;; Ex 4.31 Selective laziness ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (param-def-parameter param-def)
  (if (symbol? param-def)
      param-def
      (car param-def)))

(define (param-def-style param-def)
  (if (symbol? param-def)
      'default
      (cadr param-def)))

(define (param-defs->params parameter-defs)
  (map param-def-parameter parameter-defs))

(define (exp->arg parameter-def exp env)
  (let ((param-style (param-def-style parameter-def))) 
    (cond ((equal? param-style 'default)
           (eval exp env))
          ((equal? param-style 'lazy-memo)
           (delay-memo-it exp env))
          ((equal? param-style 'lazy)
           (delay-it exp env))
          (else error "Unknown parameter style:" param-style))))


;; Mainly unchanged from ea-text ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (let ((parameter-defs (procedure-parameter-defs procedure)))
           (eval-sequence
            (procedure-body procedure)
            (extend-environment
             (param-defs->params parameter-defs)
             (list-of-delayed-args parameter-defs arguments  env)
             (procedure-environment procedure)))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))
(define (list-of-delayed-args parameter-defs exps env)
  (cond ((no-operands? exps) '())
        (else
         (if (no-parameter-defs? parameter-defs)
             (error "Too few parameter-defs"))
         (cons
          (exp->arg (first-parameter-def parameter-defs)
                    (first-operand exps)
                    env)
          (list-of-delayed-args
           (rest-parameter-defs parameter-defs)
           (rest-operands exps)
           env)))))

(define (delay-memo-it exp env)
  (list 'thunk-memo exp env))

(define (thunk-memo? obj)
  (tagged-list? obj 'thunk-memo))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))
(define (force-it obj)
  (cond ((thunk? obj)
         (actual-value (thunk-exp obj) (thunk-env obj)))
        ((thunk-memo? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (error "here")
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((equal? exp 'undefined) true)     ; extra
        ((equal? exp '*unassigned*) true)  ; extra
        ((boolean? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tex-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameter-defs exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameter-defs body)
  (cons 'lambda (cons parameter-defs body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define no-parameter-defs? null?)
(define first-parameter-def car)
(define rest-parameter-defs cdr)
         
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;; Modified for Ex 4.05
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND-IF"
                       clauses))
            (let ((predicate-value (cond-predicate first)))
              (make-if predicate-value
                       (clause->exp first predicate-value)
                       (expand-clauses rest)))))))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (scan-out-defines exp)
  (define (definition? exp)
    (tagged-list? exp 'define))
  (define (scan exp new-members vars)
    (if (null? exp)
        (cons new-members vars)
        (let ((member (car exp)))
          (if (definition? member)
              (scan (cdr exp)
                    (cons
                     (list 'set!
                           (definition-variable member)
                           (definition-value member))
                     new-members)
                    (cons (definition-variable member) vars))
              (scan (cdr exp)
                    (cons member new-members)
                    vars)))))
  (let* ((scan-rslt (scan exp '() '()))
         (new-body (reverse (car scan-rslt)))
         (vars (reverse (cdr scan-rslt)))
         (let-pairs (map (lambda (var) (list var '*unassigned*)) vars)))
    (if (null? vars)
        exp
        (list (make-let let-pairs new-body)))))

(define (make-procedure parameter-defs body env)
  (list 'procedure parameter-defs body env))
  ;(list 'procedure parameters (scan-out-defines body) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameter-defs p) (cadr  p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define the-empty-environment '())
(define (first-frame env) (car env))

;; Frame stuff - these should be the only procs with knowledge of the frame
;; structure

(define (make-frame variables values)
  (define frame (list '*frame*))
  (define (iter vars vals)
    (cond ((pair? vars)
           (add-binding-to-frame! (car vars) (car vals) frame)
           (iter (cdr vars) (cdr vals)))
          (else frame)))
  (iter variables values))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

(define (get-frame-val var frame)
  (define (scan frame-pairs)
    (cond ((null? frame-pairs) #f)
          ((eq? var (car (car frame-pairs)))
           (list (cdr (car frame-pairs))))
          (else (scan (cdr frame-pairs)))))
  (scan (cdr frame)))

(define (set-frame-val! var val frame)
  (define (scan frame-pairs)
    (cond ((null? frame-pairs) #f)
          ((eq? var (car (car frame-pairs)))
           (set-cdr! (car frame-pairs) val)
           #t)
          (else (scan (cdr frame-pairs)))))
  (scan (cdr frame)))

;; end frame interface

;; convenience method for iterating over the separate frames
(define (scan-env env f)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        #f
        (let ((rslt (f (first-frame env))))        
          (if  rslt
               rslt
               (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (let ((rslt (scan-env env
                        (lambda (frame)
                          (get-frame-val var frame)))))
    (if rslt
        (let ((val (car rslt)))
          (if (eq? val '*unassigned*)
              (error "Unassigned variable:" var)
              val))
        (error "Unbound variable:" var))))
 
(define (set-variable-value! var val env)
  (if (not (scan-env env
                     (lambda (frame)
                       (set-frame-val! var val frame))))
      (error "Unbound variable -- SET!:" var)))        

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (if (not (set-frame-val! var val frame))
        (add-binding-to-frame! var val frame))))


;;list of primitives directly mapped to underlying apply
(define primitive-procedures
  (list
   (cons '* *)
   (cons '+ +)
   (cons '- -) ;;
   (cons '> >) ;;
   (cons 'car car)
   (cons 'cdr cdr)
   (cons 'cons cons)
   (cons 'equal? equal?)
   (cons 'list list)
   (cons 'null? null?)
   (cons 'square (lambda (x) (* x x)))
   (cons 'println writeln)
   (cons '= =)
   (cons 'remainder remainder)
   ))

(define primitive-procedure-names
  (map car primitive-procedures))

(define primitive-procedure-objects
  (map cdr primitive-procedures))

(define (primitive-procedure? proc)
  (define (iter procs)
    (if (null? procs)
        false
        (if (eq? (car procs) proc)
            true
            (iter (cdr procs)))))
  (iter primitive-procedure-objects))


(define (setup-environment)
  (let ((initial-env
         (extend-environment primitive-procedure-names
                             primitive-procedure-objects
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
  (if (primitive-procedure? proc)
      (underlying-apply proc args)
      (error "APPLY PRIMITIVE - unknown procedure" proc)))

(#%provide (all-defined)
           put)


;; Ex 4.14, 4.27 Driver Loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameter-defs object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

