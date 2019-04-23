#lang sicp

(#%require "common.scm")

;   Exercise 4.13
;   =============
;   
;   Scheme allows us to create new bindings for variables by means of
;   define, but provides no way to get rid of bindings.  Implement for the
;   evaluator a special form make-unbound! that removes the binding of a
;   given symbol from the environment in which the make-unbound! expression
;   is evaluated. This problem is not completely specified.  For example,
;   should we remove only the binding in the first frame of the environment?
;   Complete the specification and justify any choices you make.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.13]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.13
;   4.1.3 Evaluator Data Structures - p380
;   ------------------------------------------------------------------------

(-start- "4.13")

(println "
I've chosen to only check and remove the binding from the first frame.  This
seems safer to me.  Mutable variables are bad enough but (set! x ...) should
at least always refer to the same x.  But a mistake with unbind! could
remove an x and then remove a different x from a parent environment and so
on.  In contrast it seems right the life-cycle of a variable should be at
the same level, i.e., define and unbind! should be at the same level.
")


(#%require "ea-data-directed-12.scm")
(put-evaluators)

(println "
-----------------------------
")

;; Using the frame pairs implementation introduced in Ex 4.12
(define (remove-frame-var var frame)
  (define (scan frame-pairs)
    (let ((rest (cdr frame-pairs)))
      (cond ((null? rest)
             #f)
            ((eq? var (car (car rest)))
             (set-cdr! frame-pairs (cddr frame-pairs))
             #t)
            (else (scan (cdr frame-pairs))))))
  (scan frame)) ;; Only the cadr is expected to be a pair so we
                ;; can pass a frame even though its car is the
                ;; '*frame* token and not a pair.
              
(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (if (not (remove-frame-var var frame))
        (error "Unbound variable -- UNBIND!" var))))

(define (eval-unbind! exp env)
  (make-unbound! (cadr exp) env))
(put 'eval 'unbind! eval-unbind!)
  
(define expression
  '(begin
     (define x "xXx")
     (define y "yYy")
     (define z "zZz")
     (println x)
     (println y)
     (println z)

     (println "
unbinding y
")
     (unbind! y)
     
     (println x)
     (println z)
     (println "Expect error: \"Unbound variable: y\"")
     (println y)))

(eval expression the-global-environment)

(--end-- "4.13")

