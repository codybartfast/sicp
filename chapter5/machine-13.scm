#lang sicp


;; Machine 13: Ex 5.13 add get-make-register to implicitly create registers
;;
;; Machine 12: Ex 5.12 add build-path-info for analysis of data paths
;;
;; Machine 09:
;;   - Ex 5.8 check for duplicate labels in extract-labels
;;   - Ex 5.9 check labels aren't passed to operations in make-operation-exp


;; 5.2.1 the Machine Model
;; =======================

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    (build-path-info machine)
    machine))

;; Registers

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;; The Stack

(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;; The Basic Machine

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (path-info (make-path-info)))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (contains-register? name)
        (if (assoc name register-table) #t #f))
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'get-instruction-sequence)
               the-instruction-sequence)
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'contains-register?) contains-register?)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'get-path-info) path-info)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (start machine)
  (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (get-make-register machine reg-name)
  (if (not ((machine 'contains-register?) reg-name))
      ((machine 'allocate-register) reg-name))
  (get-register machine reg-name))

(define (get-instruction-sequence machine)
  (machine 'get-instruction-sequence))
(define (get-path-info machine)
  (machine 'get-path-info))

;; 5.2.2 The Assembler
;; ===================

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (cond ((symbol? next-inst)
                  (if (label-defined? labels next-inst)
                      (error "Duplicate label -- ASSEMBLE" next-inst))
                  (receive insts
                           (cons (make-label-entry next-inst
                                                   insts)
                                 labels)))
                 (else (receive (cons (make-instruction next-inst)
                              insts)
                        labels))))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))
(define (instruction-text inst)
  (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

(define (label-defined? labels label-name)
  (if (assoc label-name labels)
      #true
      #false))

;; 5.2.3 Generating Execution Procedures for Instructions
;; ======================================================

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

;; Assign Instructions

(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-make-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()                ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;; Test, Branch and Goto Instructions

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-make-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; Other Instructions

(define (make-save inst machine stack pc)
  (let ((reg (get-make-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-make-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))

;; Execution Procedures for Subexpressions

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-make-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (or (register-exp? e) (constant-exp? e))
                    (make-primitive-exp e machine labels)
                    (error "Invalid Argument for operation -- ASSEMBLE" e)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

;; From elsewhere
;; ==============

;; 4.1.2

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; For Ex 5.12
;; ===========

;; Path-info object

(define (make-path-info)
  (let ((insts '())
        (entry-regs '())
        (stack-regs '())
        (reg-sources '()))
    (define (dispatch message)
      (cond
        ((eq? message 'get-insts) insts)
        ((eq? message 'set-insts)
         (lambda (value) (set! insts value)))

        ((eq? message 'get-entry-regs) entry-regs)
        ((eq? message 'set-entry-regs)
         (lambda (value) (set! entry-regs value)))

        ((eq? message 'get-stack-regs) stack-regs)
        ((eq? message 'set-stack-regs)
         (lambda (value) (set! stack-regs value)))

        ((eq? message 'get-reg-sources) reg-sources)
        ((eq? message 'set-reg-sources)
         (lambda (value) (set! reg-sources value)))

        (else
         (error "Unknown request -- PATH-INFO" message))))
    dispatch))

(define (get-insts path-info)
  (path-info 'get-insts))
(define (set-insts! path-info value)
  ((path-info 'set-insts) value))

(define (get-entry-regs path-info)
  (path-info 'get-entry-regs))
(define (set-entry-regs! path-info value)
  ((path-info 'set-entry-regs) value))

(define (get-stack-regs path-info)
  (path-info 'get-stack-regs))
(define (set-stack-regs! path-info value)
  ((path-info 'set-stack-regs) value))

(define (get-reg-sources path-info)
  (path-info 'get-reg-sources))
(define (set-reg-sources! path-info value)
  ((path-info 'set-reg-sources) value))

;; Build path-info lists
;; =====================

;; helpers

(define (distinct list)
  (define (contains? list item)
    (if (null? list)
        #f
        (if (equal? item (car list))
            #t
            (contains? (cdr list) item))))
  (define (iter orig dist-list)
    (if (null? orig)
        dist-list
        (if (contains? dist-list (car orig))
            (iter (cdr orig) dist-list)
            (iter (cdr orig) (cons (car orig) dist-list)))))
  (iter list '()))

(define (sort key less-than? items)
  (define (combine left right)
    (if (null? left)
        right
        (combine (cdr left) (cons (car left) right))))
  (define (insert item sorted)
    (define (iter item left right)
      (if (null? right)
          (combine left (list item))
          (if (less-than? (key item) (key (car right)))
              (combine left (cons item right))
              (iter item (cons (car right) left) (cdr right)))))
    (iter item '() sorted))
  (define (iter orig sorted)
    (if (null? orig)
        sorted
        (iter (cdr orig) (insert (car orig) sorted))))
  (iter items '()))

(define (filter predicate? list)
  (define (iter orig filtered)
    (if (null? orig)
        (reverse filtered)
        (if (predicate? (car orig))
            (iter (cdr orig) (cons (car orig) filtered))
            (iter (cdr orig) filtered))))
  (iter list '()))

;; build path info

(define (build-path-info machine)
  (let* ((raw-insts (get-instruction-sequence machine))
         (path-info (get-path-info machine))
         (insts (raw-insts->insts raw-insts)))
    (set-insts! path-info insts)
    (set-entry-regs! path-info (insts->entry-regs insts))
    (set-stack-regs! path-info (insts->stack-regs insts))
    (set-reg-sources! path-info (insts->reg-sources insts))))

(define (raw-insts->insts raw-insts)
  (sort (lambda (inst) (symbol->string (car inst)))
        string<?
        (distinct
         (map car raw-insts))))

(define (insts->entry-regs insts)
  (distinct
   (map
    (lambda (inst) (cadr (cadr inst)))
    (filter (lambda (inst)
              (and (eq? 'goto (car inst))
                   (eq? 'reg (car (cadr inst)))))
            insts))))

(define (insts->stack-regs insts)
  (distinct
   (map cadr
        (filter (lambda (inst)
                  (or (eq? 'save (car inst))
                      (eq? 'restore (car inst))))
                insts))))

(define (insts->reg-sources insts)
  (let ((assign-insts
         (filter (lambda (inst) (eq? 'assign (car inst)))
                 insts)))
    (define (reg-sources reg)
      (distinct
       (map
        (lambda (inst)
          (if (eq? 'op (caaddr inst))
              (cddr inst)
              (caddr inst)))
        (filter (lambda (inst) (eq? reg (cadr inst)))
                assign-insts))))
    (map
     (lambda (reg) (list reg (reg-sources reg)))
     (distinct
      (map cadr assign-insts)))))


;; And finally...
;; ==============

(#%provide
 make-machine
 set-register-contents!
 get-register-contents
 get-path-info
 get-insts
 get-entry-regs
 get-stack-regs
 get-reg-sources
 start)
