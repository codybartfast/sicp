#lang sicp

(define line-length 72)
(define dash-line (make-string line-length #\-))
  
(define (println . parts)
  (for-each display parts)
  (newline))

(define (-start- ex-number)
  (println dash-line)
  (println "Output: Exercise " ex-number)
  (println dash-line))

(define (--end-- ex-number)
   (println))

(#%provide
 -start-
 --end--)
