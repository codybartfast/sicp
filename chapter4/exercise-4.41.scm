#lang sicp

(#%require "common.scm")

;   Exercise 4.41
;   =============
;   
;   Write an ordinary Scheme program to solve the multiple dwelling puzzle.
;   
;   ------------------------------------------------------------------------
;   [Exercise 4.41]: http://sicp-book.com/book-Z-H-28.html#%_thm_4.41
;   4.3.2 Examples of Nondeterministic Programs - p420
;   ------------------------------------------------------------------------

(-start- "4.41")

(println "
Plain Scheme version of my ex 4.40 answer:
")

(define (multiple-dwelling)

  (define floors (list 1 2 3 4 5))

  (define (enum-fletchers fletchers)
    (and (not (null? fletchers))
         (or (let ((fletcher (car fletchers)))
               (and (not (= fletcher 5))
                    (not (= fletcher 1))
                    (enum-coopers floors fletcher)))
             (enum-fletchers (cdr fletchers)))))

  (define (enum-coopers coopers fletcher)
    (and (not (null? coopers))
         (or (let ((cooper (car coopers)))
               (and (not (= cooper 1))
                    (not (= (abs (- fletcher cooper)) 1))
                    (enum-millers floors cooper fletcher)))
             (enum-coopers (cdr coopers) fletcher))))

  (define (enum-millers millers cooper fletcher)
    (and (not (null? millers))
         (or (let ((miller (car millers)))
               (and (> miller cooper)
                    (enum-smiths floors miller cooper fletcher)))
             (enum-millers (cdr millers) cooper fletcher))))

  (define (enum-smiths smiths miller cooper fletcher)
    (and (not (null? smiths))
         (or (let ((smith (car smiths)))
               (and (not (= (abs (- smith fletcher)) 1))
                    (enum-bakers floors smith miller cooper fletcher)))
             (enum-smiths (cdr smiths) miller cooper fletcher))))

  (define (enum-bakers bakers smith miller cooper fletcher)
    (and (not (null? bakers))
         (or (let ((baker (car bakers)))
               (and (not (= baker 5))
                    (check-distinct baker smith miller cooper fletcher)))
             (enum-bakers (cdr bakers) smith miller cooper fletcher))))

  (define (check-distinct baker smith miller cooper fletcher)
    (if (distinct? (list baker smith miller cooper fletcher))
        (list (list 'baker baker)
              (list 'cooper cooper)
              (list 'fletcher fletcher)
              (list 'miller miller)
              (list 'smith smith))
        #f))

  (define (distinct? items)
    (cond ((null? items) true)
          ((null? (cdr items)) true)
          ((member (car items) (cdr items)) false)
          (else (distinct? (cdr items)))))

  (enum-fletchers floors))

(time (multiple-dwelling))

(--end-- "4.41")

