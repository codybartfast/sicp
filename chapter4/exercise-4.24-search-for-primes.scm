#lang sicp

(define search-for-primes
  '(begin
    (define (ignore x) (if false 'unreachable))

    (define (time proc)
      (let ((rslt (proc)))
        (println "it took time.")
        (println rslt)
        rslt))

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
      (println " *** ")
      (ignore (time (lambda () (prime-repeat repeat)))))

    (define repeat-count 3000)

    (define (report-prime prime? n)
      (report-prime-repeat prime? n repeat-count))

    (define (report-prime-single prime? n)
      (report-prime-repeat prime? n 1))

    (define (search-for-primes even-start count)
      (println "First three primes above " even-start ":") 
      (define prime? sd-all-prime?)
      (define (iter candidate count)
        (cond ((= count 0) (ignore 'unused))
              (else
               (cond ((prime? candidate)
                      (report-prime prime? candidate)
                      (iter (+ candidate 2) (- count 1)))
                     (else
                      (iter (+ candidate 2) count))))))
      (iter (+ even-start 1) count)
      (println "-- end --"))

    (search-for-primes 1000 3)
    (search-for-primes 10000 3)
    (search-for-primes 100000 3)
    (search-for-primes 1000000 3)
    ))

(#%provide search-for-primes)