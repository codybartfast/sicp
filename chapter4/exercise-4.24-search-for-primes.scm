#lang sicp

; code from exercise 1.22 with a few tweaks.

(define search-for-primes-expr
  '(begin
     
     (define repeat-count 100)
     (println "Repeating prime check " repeat-count " times.
")
     
     (define (square n) (* n n))

     (define (smallest-divisor n)
       (define (find-divisor n test-divisor)
         (define (divides? a b)
           (= (remainder b a) 0))
         (cond ((> (square test-divisor) n) n)
               ((divides? test-divisor n) test-divisor)
               (else (find-divisor n (+ 1 test-divisor)))))
       (find-divisor n 2))

     (define (sd-all-prime? n)
       (= n (smallest-divisor n)))

     (define (report-prime-repeat prime? n repeat)
       (define (prime-repeat repeat-count)
         (cond ((= 0 repeat-count)
                (prime? n))
               (else
                (prime? n)           
                (prime-repeat (- repeat-count 1)))))
       (println "    " n)
       (prime-repeat repeat))

     (define (report-prime prime? n)
       (report-prime-repeat prime? n repeat-count))

     (define (search-for-primes even-start count)
       (println "First three primes above " even-start ":")
       (define prime? sd-all-prime?)
       (define (iter candidate count)
         (cond
           ((= count 0) 'done)
           ((prime? candidate)
            (report-prime prime? candidate)
            (iter (+ candidate 2) (- count 1)))
           (else
            (iter (+ candidate 2) count))))
       (iter (+ even-start 1) count))

     (search-for-primes 1000 3)
     (search-for-primes 10000 3)
     (search-for-primes 100000 3)
     (search-for-primes 1000000 3)
     (println "")
     ))

(#%provide search-for-primes-expr)