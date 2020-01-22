#lang sicp

(#%require "common.scm")

;   Exercise 5.8
;   ============
;
;   The following register-machine code is ambiguous, because the label here
;   is defined more than once:
;
;   start
;     (goto (label here))
;   here
;     (assign a (const 3))
;     (goto (label there))
;   here
;     (assign a (const 4))
;     (goto (label there))
;   there
;
;   With the simulator as written, what will the contents of register a be
;   when control reaches there?  Modify the extract-labels procedure so that
;   the assembler will signal an error if the same label name is used to
;   indicate two different locations.
;
;   ------------------------------------------------------------------------
;   [Exercise 5.8]:  http://sicp-book.com/book-Z-H-32.html#%_thm_5.8
;   5.2.2 The Assembler - p523
;   ------------------------------------------------------------------------

(-start- "5.8")
(#%require "machine-07.scm")

(println "I predict... the value will be 3.  Extract-labels starts by
calling itself recursively and then work is done as the recursion unwinds.
So instruction will be in the original order (the first labels is the last
to be processed when unwinding and so is cons'ed on the front of the list of
labels) and so the first label is the first to be found by assoc.
")

(println
 "The actual answer is: "
 (let ((everywhere
        (make-machine
         '(a)
         '()
         '(start
           (goto (label here))
           here
           (assign a (const 3))
           (goto (label there))
           here
           (assign a (const 4))
           (goto (label there))
           there))))

   (start everywhere)
   (get-register-contents everywhere 'a))
 ", so I guessed right for once!")

(println "
Duplicates can be checked for in extract-labels:

  (define (extract-labels text receive)
    (if (null? text)
        (receive '() '())
        (extract-labels (cdr text)
         (lambda (insts labels)
           (let ((next-inst (car text)))
             (cond ((symbol? next-inst)
                    (if (label-defined? labels next-inst)
                        (error "Duplicate label -- ASSEMBLE" next-inst))
                    (receive insts
                             (cons (make-label-entry next-inst
                                                     insts)
                                   labels)))
                   (else (receive (cons (make-instruction next-inst)
                                insts)
                          labels))))))))

  (define (label-defined? labels label-name)
    (if (assoc label-name labels)
        #true
        #false))

This is implemented in machine-09.
")

(--end-- "5.8")

