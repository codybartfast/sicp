#lang sicp

(#%require "common.scm")

;   Exercise 5.9
;   ============
;
;   The treatment of machine operations above permits them to operate on
;   labels as well as on constants and the contents of registers. Modify the
;   expression-processing procedures to enforce the condition that
;   operations can be used only with registers and constants.
;
;   ------------------------------------------------------------------------
;   [Exercise 5.9]:  http://sicp-book.com/book-Z-H-32.html#%_thm_5.9
;   5.2.3 Generating Execution Procedures for Instructions - p529
;   ------------------------------------------------------------------------

(-start- "5.9")

(#%require "machine-09.scm")

(define (expn b n)
  (let ((machine (make-machine
                  '(b n p continue)
                  (list
                   (list 'sub -)
                   (list 'mul *)
                   (list '= =))
                  '((assign continue (label expn-done))
                    test
                    (test (op =) (reg n) (const 0))
                    (branch (label base-case))
                    (save continue)
                    (assign continue (label after-expn))
                    (assign n (op sub) (reg n) (const 1))
                    (goto (label test))
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

(println "The check can be enforced in make-operation-expr:

  (define (make-operation-exp exp machine labels operations)
    (let ((op (lookup-prim (operation-exp-op exp) operations))
          (aprocs
           (map (lambda (e)
                  (if (or (register-exp? e) (constant-exp? e)) ;; <-- check
                      (make-primitive-exp e machine labels)
                      (error
                       \"Invalid Argument for operation -- ASSEMBLE\" e)))
                (operation-exp-operands exp))))
      (lambda ()
        (apply op (map (lambda (p) (p)) aprocs)))))

")

(println "Evidence we didn't break things (expect 243): " (expn 3 5))

(println "
But now we expect an INVALID ARGUMENT error:
                     ================")

(let ((mac
       (make-machine
        '(a)
        (list (list 'add +))
        '(start
          (assign a (label there)) ;; valid
          (assign a (op add) (const 1) (label there))
          there))))

  (start mac)
  (get-register-contents mac 'a))

(--end-- "5.9")

