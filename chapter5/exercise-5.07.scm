#lang sicp

(#%require "common.scm")

;   Exercise 5.7
;   ============
;   
;   Use the simulator to test the machines you designed in exercise [5.4].
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.7]:  http://sicp-book.com/book-Z-H-32.html#%_thm_5.7
;   [Exercise 5.4]:  http://sicp-book.com/book-Z-H-32.html#%_thm_5.4
;   5.2 A Register-Machine Simulator - p515
;   ------------------------------------------------------------------------

(-start- "5.7")

(#%require "machine-07.scm")

(define expn-rec
  (make-machine
   '(b n p continue)
   (list
    (list 'sub -)
    (list 'mul *)
    (list '= =))
   '((assign continue (label expn-done))
     test
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (assign continue (label after-expn))
     (assign n (op sub) (reg n) (const 1))
     (goto (label test))
     after-expn
     (restore continue)
     (assign p (op mul) (reg p) (reg b))
     (goto (reg continue))
     base-case
     (assign p (const 1))
     (goto (reg continue))
     expn-done)))

(define expn-itr
  (make-machine
   '(b n p)
   (list
    (list 'sub -)
    (list 'mul *)
    (list '= =))
   '(test
     (test (op =) (reg n) (const 0))
     (branch (label expn-done))
     (assign p (op mul) (reg p) (reg b))
     (assign n (op sub) (reg n) (const 1))
     (goto (label test))
     expn-done)))

(define (expn machine b n)
  (set-register-contents! machine 'b b)
  (set-register-contents! machine 'n n)
  (set-register-contents! machine 'p 1)
  (start machine)
  (get-register-contents machine 'p))

(let ((b 3)
      (p 5))
  (println b " raised to power of " p " using RECURSIVE machine: "
           (expn expn-rec b p))
  (println b " raised to power of " p " using ITERATIVE machine: "
           (expn expn-itr b p)))

(--end-- "5.7")

