#lang sicp

(#%require "common.scm")

;   Exercise 4.27
;   =============
;   
;   Suppose we type in the following definitions to the lazy evaluator:
;   
;   (define count 0)
;   (define (id x)
;     (set! count (+ count 1))
;     x)
;   
;   Give the missing values in the following sequence of interactions, and
;   explain your answers.⁽³⁸⁾
;   
;   (define w (id (id 10)))
;   ;;; L-Eval input:
;   count
;   ;;; L-Eval value:
;   <response>
;   ;;; L-Eval input:
;   w
;   ;;; L-Eval value:
;   <response>
;   ;;; L-Eval input:
;   count
;   ;;; L-Eval value:
;   <response>
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.27]: http://sicp-book.com/book-Z-H-27.html#%_thm_4.27
;   [Footnote 38]:   http://sicp-book.com/book-Z-H-27.html#footnote_Temp_587
;   4.2.2 An Interpreter with Lazy Evaluation - p406
;   ------------------------------------------------------------------------

(-start- "4.27")

(#%require "ea-data-directed-27.scm")
(put-evaluators)
(#%require "ea-pick-fruit-expression.scm")

(check-fruit
 (apply
  (eval pick-fruit the-global-environment)
  '()
  the-global-environment))

(println "
Sample Output
=============

    ;;; M-Eval input:
    (define w (id (id 10)))

    ;;; M-Eval value:
    #<void>

    ;;; M-Eval input:
    count

    ;;; M-Eval value:
    1

    ;;; M-Eval input:
    w

    ;;; M-Eval value:
    #0=(thunk (id 10) #1=((*frame* (w . #0#) (id procedure (x) ((set! count
        (+ count 1)) x) #1#) (count . 1) (false . #f) (true . #t) (remainder
        . #<procedure:remainder>) (= . #<procedure:=>) (println .
        #<procedure:writeln>) (square . #<procedure:...-directed-27.scm:
        550:17>) (null? . #<procedure:null?>) (list . #<procedure:mlist>)
        (equal? . #<procedure:equal?>) (cons . #<procedure:mcons>) (cdr .
        #<procedure:mcdr>) (car . #<procedure:mcar>) (> . #<procedure:>>) (-
        . #<procedure:->) (+ . #<procedure:+>) (* . #<procedure:*>))))

    ;;; M-Eval input:
    count

    ;;; M-Eval value:
    1
")

(println "
Starting mc-evaluator...
========================")
(put-evaluators)
(driver-loop)

(--end-- "4.27")

