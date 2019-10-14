#lang sicp

(#%require "common.scm")

;   Exercise 4.54
;   =============
;   
;   If we had not realized that require could be implemented as an ordinary
;   procedure that uses amb, to be defined by the user as part of a
;   nondeterministic program, we would have had to implement it as a special
;   form.  This would require syntax procedures
;   
;   (define (require? exp) (tagged-list? exp 'require))
;   
;   (define (require-predicate exp) (cadr exp))
;   
;   and a new clause in the dispatch in analyze
;   
;   ((require? exp) (analyze-require exp))
;   
;   as well the procedure analyze-require that handles require expressions. 
;   Complete the following definition of analyze-require.
;   
;   (define (analyze-require exp)
;     (let ((pproc (analyze (require-predicate exp))))
;       (lambda (env succeed fail)
;         (pproc env
;                (lambda (pred-value fail2)
;                  (if <??>
;                      <??>
;                      (succeed 'ok fail2)))
;                fail))))
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.54]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.54
;   4.3.3 Implementing the <tt>Amb</tt> Evaluator - p437
;   ------------------------------------------------------------------------

(-start- "4.54")

(println "
  (define (analyze-require exp)
    (let ((pproc (analyze (require-predicate exp))))
      (lambda (env succeed fail)
        (pproc env
               (lambda (pred-value fail2)
                 (if (not pred-value)
                     (fail2)
                     (succeed 'ok fail2)))
               fail))))

Test with the 8 queens problems...
")

(#%require "ea-analyzing-54.scm")
(put-evaluators)

(define 8queens
  '(begin

;     (define (require p)
;       (if (not p) (amb)))

     (define (member? item list)
       (cond ((null? list) #false)
             ((equal? item (car list)) #true)
             (else (member? item (cdr list)))))

     (define (map proc items)
       (if (null? items)
           '()
           (cons (proc (car items))
                 (map proc (cdr items)))))
 
     (define (distinct? items)
       (cond ((null? items) true)
             ((null? (cdr items)) true)
             ((member? (car items) (cdr items)) false)
             (else (distinct? (cdr items)))))

     (define (new-queen col)
       (cons col (amb 1 2 3 4 5 6 7 8)))

     (define (8queens)
       (define (iter queens)
         (require (distinct? (map cdr queens)))
         (require (distinct? (map (lambda (q) (- (car q) (cdr q))) queens)))
         (require (distinct? (map (lambda (q) (+ (car q) (cdr q))) queens)))
         (if (= 8 (length queens))
             queens
             (iter (cons (new-queen (+ 1 (length queens))) queens))))
       (iter '()))
          
     (8queens)

     ))

(eval 8queens)


(--end-- "4.54")

