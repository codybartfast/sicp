#lang sicp

(#%require "common.scm")

;   Exercise 4.14
;   =============
;   
;   Eva Lu Ator and Louis Reasoner are each experimenting with the
;   metacircular evaluator.  Eva types in the definition of map, and runs
;   some test programs that use it.  They work fine.  Louis, in contrast,
;   has installed the system version of map as a primitive for the
;   metacircular evaluator.  When he tries it, things go terribly wrong. 
;   Explain why Louis's map fails even though Eva's works.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.14]: http://sicp-book.com/book-Z-H-26.html#%_thm_4.14
;   4.1.4 Running the Evaluator as a Program - p384
;   ------------------------------------------------------------------------

(-start- "4.14")

(println "
I imagine the problem is that map takes a procedure as an argument.  If it
is implemented as a primitive then map is called in the context of the
metacircular evaluator not the interpreted environment.  That would be fine
except in this case it also means the procedure passed as an agument to map,
which is defined in the interpreted environment, is evaluated in
the metacircular evaluator's environment.


;;; M-Eval input:
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

;;; M-Eval value:
#<void>

;;; M-Eval input:
(map square (list 1 2 3))

;;; M-Eval value:
(1 4 9)

;;; M-Eval input:
")

(#%require "ea-data-directed-12.scm")
(put-evaluators)

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
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))
(driver-loop)

(--end-- "4.14")
