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
I updated the instruction object to maintain a bool to say if it is a
breakpoint and, if so, the instruction's original location (label . offset).

;; The breakpoint-controller then provides procedures for modifying this:

  (define (make-breakpoint-controller labels)
    (define (set label offset)
      (set-instruction-break!
       (list-ref (lookup-label labels label) offset)
       #t (cons label offset)))
    (define (cancel label offset)
      (set-instruction-break!
       (list-ref (lookup-label labels label) offset) #f '()))
    (define (cancel-all)
      (map
       (lambda (label)
         (map
          (lambda (inst)
            (set-instruction-break! inst #f '()))
          (filter
           (lambda (inst) (not (symbol? inst)))
           (cdr label))))
       labels))
    (define (dispatch message)
      (cond
        ((eq? message 'set) set)
        ((eq? message 'cancel) cancel)
        ((eq? message 'cancel-all) cancel-all)))
    dispatch)

;; For proceed I renamed the main execute procuedure to execute-proceed
;; this takes boolean indicating whether it should check for a breakpoint.
;; Exectue calls this with #true but proceed calls it with false so the
;; instruction with the breakpoin can be executed.

  (define (execute-proceed check-break)
    (let ((insts (get-contents pc)))
      (cond ((null? insts)
              ...
            ((and check-break (instruction-break? (car insts)))
             (let ((desc  (instruction-break-desc (car insts))))
               (display \"--break--:  label: \")
               (display (car desc))
               (display \" offset: \")
               (display (cdr desc))
               (newline)
               'stopped))
            (else
             (write-trace (instruction-text (car insts)))
             ((instruction-execution-proc (car insts)))
             (set! inst-count (+ inst-count 1))
             (execute)))))
  (define (proceed) (execute-proceed #false))
  (define (execute) (execute-proceed #true))

Demo (fib 3):
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
    (set-breakpoint machine 'afterfib-n-2 4)

    (define total-inst-count 0)
    (define (add-inst-count stats)
      (set! total-inst-count
            (+ total-inst-count
               (cadar stats)))
      total-inst-count)
    (define (print-status machine)
      (println
       "inst-count: " (add-inst-count (machine-stats machine))
       ", val:" (get-register-contents machine 'val))
      (println ""))

    (start machine)
    (print-status machine)
    (proceed-machine machine)
    (print-status machine)
    (println (proceed-machine machine))
    (print-status machine)))

(fib-trace)

(--end-- "5.19")

