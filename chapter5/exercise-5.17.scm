#lang sicp

(#%require "common.scm")

;   Exercise 5.17
;   =============
;   
;   Extend the instruction tracing of exercise [5.16] so that before
;   printing an instruction, the simulator prints any labels that
;   immediately precede that instruction in the controller sequence.  Be
;   careful to do this in a way that does not interfere with instruction
;   counting (exercise [5.15]). You will have to make the simulator retain
;   the necessary label information.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.17]: http://sicp-book.com/book-Z-H-32.html#%_thm_5.17
;   [Exercise 5.16]: http://sicp-book.com/book-Z-H-32.html#%_thm_5.16
;   [Exercise 5.15]: http://sicp-book.com/book-Z-H-32.html#%_thm_5.15
;   5.2.4 Monitoring Machine Performance - p532
;   ------------------------------------------------------------------------

(-start- "5.17")

(println
 "
Modified extract-labels to include the label in the list of instructions.
Then updated  execute to check if an 'instruction' is a symbol or a regular
instruction:

  (define (extract-labels text receive)
    (if (null? text)
        (receive '() '())
        (extract-labels (cdr text)
         (lambda (insts labels)
           (let ((next-inst (car text)))
             (cond ((symbol? next-inst)
                    (if (label-defined? labels next-inst)
                        (error \"Duplicate label -- ASSEMBLE\" next-inst))
                    (receive (cons next-inst insts)          ;; <--
                             (cons (make-label-entry
                                    next-inst
                                    (cons next-inst insts))  ;; <--
                                   labels)))
                   (else (receive (cons (make-instruction next-inst)
                                insts)
                          labels))))))))

  (define (update-insts! insts labels machine)
    (let ((pc (get-register machine 'pc))
          (flag (get-register machine 'flag))
          (stack (machine 'stack))
          (ops (machine 'operations)))
      (for-each
       (lambda (inst)
         (if (not (symbol? inst))                            ;; <--
             (set-instruction-execution-proc!
              inst
              (make-execution-procedure
               (instruction-text inst) labels machine
               pc flag stack ops))))
       insts)))

  (define (make-new-machine)
      ...
      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts)
                 'done)
                ((symbol? (car insts))                       ;; <--
                 (write-trace (car insts))
                 (advance-pc pc)
                 (execute))
                (else
                 (write-trace (caar insts))
                 ((instruction-execution-proc (car insts)))
                 (set! inst-count (+ inst-count 1))
                 (execute)))))

Demo (note - the instruction count is still 97, but there are now 120 trace
messages):
")

(#%require "machine-17.scm")

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

(--end-- "5.17")

