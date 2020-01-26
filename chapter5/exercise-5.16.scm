#lang sicp

(#%require "common.scm")

;   Exercise 5.16
;   =============
;   
;   Augment the simulator to provide for instruction tracing. That is,
;   before each instruction is executed, the simulator should print the text
;   of the instruction.  Make the machine model accept trace-on and
;   trace-off messages to turn tracing on and off.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.16]: http://sicp-book.com/book-Z-H-32.html#%_thm_5.16
;   5.2.4 Monitoring Machine Performance - p532
;   ------------------------------------------------------------------------

(-start- "5.16")

(#%require "machine-16.scm")

(println
 "
Rather than printing directly from the machine it seems neater to me to pass
a message sink to the trace-on! procedure.  The key bit from
make-new-machine:

      (define write-trace
        (lambda (message) '()))
      (define (trace-on sink)
        (set! write-trace sink))
      (define (trace-off)
        (set! write-trace (lambda (message) '())))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (write-trace (caar insts))    ;; <--
                ((instruction-execution-proc (car insts)))
                (set! inst-count (+ inst-count 1))
                (execute)))))

Demo (note - the instruction count, 97, matches the number of trace
messages):
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
Switching trace on ...
")
    (trace-on! machine
               (lambda (message)
                 (println "--trace--: " message)))
    (set-register-contents! machine 'n 4)
    (start machine)
    (println "(fib 4): " (machine-stats machine))
    (println "
Switching trace off ...
")
    (trace-off! machine)

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
(println "")

(--end-- "5.16")

