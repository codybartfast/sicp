#lang sicp

(#%require "common.scm")

;   Exercise 5.18
;   =============
;   
;   Modify the make-register procedure of section [5.2.1] so that registers
;   can be traced. Registers should accept messages that turn tracing on and
;   off.  When a register is traced, assigning a value to the register
;   should print the name of the register, the old contents of the register,
;   and the new contents being assigned.  Extend the interface to the
;   machine model to permit you to turn tracing on and off for designated
;   machine registers.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.18]: http://sicp-book.com/book-Z-H-32.html#%_thm_5.18
;   [Section 5.2.1]: http://sicp-book.com/book-Z-H-32.html#%_sec_5.2.1
;   5.2.4 Monitoring Machine Performance - p532
;   ------------------------------------------------------------------------

(-start- "5.18")

(#%require "machine-18.scm")

(println
 "
This is just a variation on the previous trace.  Main bits:

  (define (write-null . message-parts) '())

  (define (make-register name)
    (let ((contents '*unassigned*))
      (define write-trace write-null)
      (define (trace-on sink)
        (set! write-trace sink))
      (define (trace-off)
        (set! write-trace write-null)) 
      (define (dispatch message)
        (cond ((eq? message 'get) contents)
              ((eq? message 'set)
               (lambda (value)
                 (write-trace name contents value)
                 (set! contents value)))
              ((eq? message 'trace-on) trace-on)
              ((eq? message 'trace-off) trace-off)
              (else
               (error \"Unknown request -- REGISTER\" message))))
      dispatch))

Usage:

  (reg-trace-on!
    machine
    'val
    (lambda (reg before after)
      (println \"---reg---: \" reg \": \" before \" -> \" after)))

Demo:
")

(define (fib-trace)
  (let ((machine (make-machine
                  (list
                 (list '+ +)
                 (list '- -)
                 (list '< <))
                '((assign continue (label fib-done))
                  fib-loop
                  (test (op <) (reg n) (const 2))
                  (branch (label immediate-answer))
                  (save continue)
                  (assign continue (label afterfib-n-1))
                  (save n)
                  (assign n (op -) (reg n) (const 1))
                  (goto (label fib-loop))
                  afterfib-n-1
                  (restore n)
                  (restore continue)
                  (assign n (op -) (reg n) (const 2))
                  (save continue)
                  (assign continue (label afterfib-n-2))
                  (save val)
                  (goto (label fib-loop))
                  afterfib-n-2
                  (assign n (reg val))
                  (restore val)
                  (restore continue)
                  (assign val (op +) (reg val) (reg n))
                  (goto (reg continue))
                  immediate-answer
                  (assign val (reg n))
                  (goto (reg continue))
                  fib-done))))

    (set-register-contents! machine 'n 3)
    (start machine)
    (println "(fib 3): " (machine-stats machine))

    (println "
Switching register tracing for 'val' ...
")
    (reg-trace-on! machine 'val
                   (lambda (reg before after)
                     (println "---reg---: " reg ": " before " -> " after)))
    (set-register-contents! machine 'n 4)
    (start machine)
    (println "(fib 4): " (machine-stats machine))
    (println "
Switching trace off ...
")
    (reg-trace-off! machine 'val)

    (set-register-contents! machine 'n 5)
    (start machine)
    (println "(fib 5): " (machine-stats machine))

    (set-register-contents! machine 'n 10)
    (start machine)
    (println "(fib 10): " (machine-stats machine))

    (set-register-contents! machine 'n 15)
    (start machine)
    (println "(fib 15): " (machine-stats machine))

    (set-register-contents! machine 'n 20)
    (start machine)
    (println "(fib 20): " (machine-stats machine))

    (set-register-contents! machine 'n 25)
    (start machine)
    (println "(fib 25): " (machine-stats machine)))

  )

(fib-trace)

(--end-- "5.18")

