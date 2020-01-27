#lang sicp

(#%require "common.scm")

;   Exercise 5.19
;   =============
;   
;   Alyssa P. Hacker wants a breakpoint feature in the simulator to help her
;   debug her machine designs.  You have been hired to install this feature
;   for her.  She wants to be able to specify a place in the controller
;   sequence where the simulator will stop and allow her to examine the
;   state of the machine.  You are to implement a procedure
;   
;   (set-breakpoint <machine> <label> <n>)
;   
;   that sets a breakpoint just before the nth instruction after the given
;   label.  For example,
;   
;   (set-breakpoint gcd-machine 'test-b 4)
;   
;   installs a breakpoint in gcd-machine just before the assignment to
;   register a.  When the simulator reaches the breakpoint it should print
;   the label and the offset of the breakpoint and stop executing
;   instructions.  Alyssa can then use get-register-contents and
;   set-register-contents! to manipulate the state of the simulated machine.
;   She should then be able to continue execution by saying
;   
;   (proceed-machine <machine>)
;   
;   She should also be able to remove a specific breakpoint by means of
;   
;   (cancel-breakpoint <machine> <label> <n>)
;   
;   or to remove all breakpoints by means of
;   
;   (cancel-all-breakpoints <machine>)
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.19]: http://sicp-book.com/book-Z-H-32.html#%_thm_5.19
;   5.2.4 Monitoring Machine Performance - p532
;   ------------------------------------------------------------------------

(-start- "5.19")

(println
 "
Breath

Demo:
")

(#%require "machine-19.scm")

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
    (println (set-breakpoint machine 'afterfib-n-2 4))
    (start machine)
    (println "(fib 3): " (machine-stats machine))
    (proceed-machine machine)
    (println "(fib 3): " (machine-stats machine))
    (proceed-machine machine)
    (println "(fib 3): " (machine-stats machine))
    (println (get-register-contents machine 'val))

;    (println "
;Switching register tracing for 'val' ...
;")
;    (reg-trace-on! machine 'val
;                   (lambda (reg before after)
;                     (println "---reg---: " reg ": " before " -> " after)))
;    (set-register-contents! machine 'n 4)
;    (start machine)
;    (println "(fib 4): " (machine-stats machine))
;    (println "
;Switching trace off ...
;")
;    (reg-trace-off! machine 'val)
;
;    (set-register-contents! machine 'n 5)
;    (start machine)
;    (println "(fib 5): " (machine-stats machine))
;
;    (set-register-contents! machine 'n 10)
;    (start machine)
;    (println "(fib 10): " (machine-stats machine))
;
;    (set-register-contents! machine 'n 15)
;    (start machine)
;    (println "(fib 15): " (machine-stats machine))
;
;    (set-register-contents! machine 'n 20)
;    (start machine)
;    (println "(fib 20): " (machine-stats machine))
;
;    (set-register-contents! machine 'n 25)
;    (start machine)
;    (println "(fib 25): " (machine-stats machine)))

    ))

(fib-trace)

(--end-- "5.19")

