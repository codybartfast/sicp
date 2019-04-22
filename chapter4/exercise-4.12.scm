#lang sicp

(#%require "common.scm")

;   Exercise 4.12
;   =============
;   
;   The procedures set-variable-value!, define-variable!, and
;   lookup-variable-value can be expressed in terms of more abstract
;   procedures for traversing the environment structure. Define abstractions
;   that capture the common patterns and redefine the three procedures in
;   terms of these abstractions.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.12]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.12
;   4.1.3 Evaluator Data Structures - p380
;   ------------------------------------------------------------------------

(-start- "4.12")

(println "
Following Ex 4.11 the first abstraction that comes to mind is one that
allows the abstraction of the frame and the separation of procs that have
'know' how the frame is implemented and which together form an abstraction
of the frame.  They are:

    make-frame
    add-binding-to-frame!
    get-frame-val
    set-frame-val!

by re-implementing these four procedures we can change the implementation of
frames.

Also, now that the frame access is separated we can abstract the process of
iterating over them:

    (define (scan-env env f)
      (define (env-loop env)
        (if (eq? env the-empty-environment)
            #f
            (let ((rslt (f (first-frame env))))        
              (if  rslt
                   rslt
                   (env-loop (enclosing-environment env))))))
      (env-loop env))

which iteratively calls 'f' with on successive frames until it 'f' is
successful.  Then set-variable-value! can be implmented succinctly as:

    (define (set-variable-value! var val env)
      (if (not (scan-env env
                         (lambda (frame)
                           (set-frame-val! var val frame))))
          (error \"Unbound variable -- SET!:\" var))) 

This is implemented in ea-data-directed-12 used here:
")

(#%require "ea-data-directed-12.scm")
(put-evaluators)
(#%require "ea-pick-fruit-expression.scm")

(println "Checking with data-directed eval '12':")
(check-fruit
 (apply (eval
         pick-fruit
         the-global-environment)
        '()))

(println "
The following are the only procedures that need to change to reimplement
frames with the original pair of lists (instead of list of pairs):

    (define (make-frame variables values)
      (cons variables values))
    (define (frame-variables frame) (car frame))
    (define (frame-values frame) (cdr frame))

    (define (add-binding-to-frame! var val frame)
      (set-car! frame (cons var (car frame)))
      (set-cdr! frame (cons val (cdr frame))))

    (define (get-frame-val var frame)
      (define (scan vars vals)
        (cond ((null? vars) #f)
              ((eq? var (car vars))
               (list (car vals)))
              (else (scan (cdr vars) (cdr vals)))))
      (scan (frame-variables frame)
            (frame-values frame)))

    (define (set-frame-val! var val frame)
      (define (scan vars vals)
        (cond ((null? vars) #f)
              ((eq? var (car vars))
               (set-car! vals val)
               #t)
              (else (scan (cdr vars) (cdr vals)))))
      (scan (frame-variables frame)
            (frame-values frame)))

")


(--end-- "4.12")

