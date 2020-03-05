#lang sicp

(#%require "common.scm")

;   Exercise 5.48
;   =============
;   
;   The compile-and-go interface implemented in this section is awkward,
;   since the compiler can be called only once (when the evaluator machine
;   is started).  Augment the compiler-interpreter interface by providing a
;   compile-and-run primitive that can be called from within the
;   explicit-control evaluator as follows:
;   
;   ;;; EC-Eval input:
;   (compile-and-run
;    '(define (factorial n)
;       (if (= n 1)
;           1
;           (* (factorial (- n 1)) n))))
;   ;;; EC-Eval value:
;   ok
;   ;;; EC-Eval input:
;   (factorial 5)
;   ;;; EC-Eval value:
;   120
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.48]: http://sicp-book.com/book-Z-H-35.html#%_thm_5.48
;   5.5.7 Interfacing Compiled Code to the Evaluator - p609
;   ------------------------------------------------------------------------

(-start- "5.48")

(define source
  '(begin
     'hello
    )
  )

(define commands
  '(begin
     (compile-and-go
      (define (factorial n)
        (if (= n 1)
            1
            (* (factorial (- n 1)) n))))
     ;(factorial 5)
     ))

(#%require "machine-48.scm")
(#%require "compiler-48.scm")
(#%require "ec-evaluator-48.scm")

(define (print-reg reg before after)
  (println "REG: " reg "  " before " ---> " after))

(define (compile-and-go)
  (let* ((eceval
          (make-machine
           eceval-operations
           explicit-control-evaluator))
         (instructions
          (assemble-instructions
           (assemble (statements
                      (compile source empty-ctenv 'val 'return))
                     eceval))))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'exp commands)
    ;(set-register-contents! eceval 'flag true)
    ;(trace-on! eceval println)
    ;(reg-trace-on! eceval 'exp print-reg)
    ;(set-breakpoint eceval 'external-entry 1)
    (start eceval)
    ))

(ignore (compile-and-go))
;(compile-and-go '(define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n))))

(--end-- "5.48")

