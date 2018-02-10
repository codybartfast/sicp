#lang sicp

(#%require "common.scm")

;   Exercise 3.27
;   =============
;   
;   Memoization (also called tabulation) is a technique that enables a
;   procedure to record, in a local table, values that have previously been
;   computed.  This technique can make a vast difference in the performance
;   of a program.  A memoized procedure maintains a table in which values of
;   previous calls are stored using as keys the arguments that produced the
;   values.  When the memoized procedure is asked to compute a value, it
;   first checks the table to see if the value is already there and, if so,
;   just returns that value.  Otherwise, it computes the new value in the
;   ordinary way and stores this in the table.  As an example of
;   memoization, recall from section [1.2.2] the exponential process for
;   computing Fibonacci numbers:
;   
;   (define (fib n)
;     (cond ((= n 0) 0)
;           ((= n 1) 1)
;           (else (+ (fib (- n 1))
;                    (fib (- n 2))))))
;   
;   The memoized version of the same procedure is
;   
;   (define memo-fib
;     (memoize (lambda (n)
;                (cond ((= n 0) 0)
;                      ((= n 1) 1)
;                      (else (+ (memo-fib (- n 1))
;                               (memo-fib (- n 2))))))))
;   
;   where the memoizer is defined as
;   
;   (define (memoize f)
;     (let ((table (make-table)))
;       (lambda (x)
;         (let ((previously-computed-result (lookup x table)))
;           (or previously-computed-result
;               (let ((result (f x)))
;                 (insert! x result table)
;                 result))))))
;   
;   Draw an environment diagram to analyze the computation of (memo-fib 3). 
;   Explain why memo-fib computes the nth Fibonacci number in a number of
;   steps proportional to n. Would the scheme still work if we had simply
;   defined memo-fib to be (memoize fib)?
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.27]: http://sicp-book.com/book-Z-H-22.html#%_thm_3.27
;   [Section 1.2.2]: http://sicp-book.com/book-Z-H-11.html#%_sec_1.2.2
;   3.3.3 Representing Tables - p272
;   ------------------------------------------------------------------------

(-start- "3.27")

(prn "                                                      para: key
                                            para: key       para: value
            para: f                         para: table     para: table
         (let ((table (make-...   (...)        (...)           (...)
                @ @ ─┐             @ @ ─┐       @ @ ─┐          @ @ ─┐
                 ^   │              ^   │        ^   │           ^   │
Global Env ──┐   │   │              │   │        │   │           │   │
             v   │   v              │   v        │   v           │   v
┌──────────────────────────────────────────────────────────────────────────┐
│memoize: ───────┘                  │            │               │         │
│make-table: ───────────────────────┘            │               │         │
│lookup: ────────────────────────────────────────┘               │         │
│insert!: ───────────────────────────────────────────────────────┘         │
│                                                                          │
│(after call to memoize)                                                   │
│memo-fib: ────┐                                                           │
└──────────────────────────────────────────────────────────────────────────┘
               │                                                         ^
               │                                                         │
               │                        Call 'memoize (lambda (n)...'    │
               │                        ┌───────────────────────────┐    │
               │                    E1: │table: <table>             ├────┘
               V                        │f: ─┐                      │ 
              @ @──────────────────────>│    │                      │
              │                    ┌───>└───────────────────────────┘
              v                    │         │     ^         
      parameter: x                 │         V     │  
   (let ((previously-computed...   │        @ @────┘ 
     (or previously-computed-...   │        │        
         (let ((result (f x)))     │        V
           (insert! x result ...   │    parameter: n                        
           result))))))            │   (cond ((= n 0) 0)                    
                                   │         ((= n 1) 1)                    
                                   │         (else (+ (memo-fib (- n 1))    
                                   │                  (memo-fib (- n 2))))))
                                   │
          Call 'memo-fib 3'        │
    ┌───────────────────────────┐  │
    │x: 3                       ├──┘
E2: │                           │<────────────────────────────────────────┐
    └───────────────────────────┘<────────┐                               │
                                          │                               │
                                          │                               │
               Call 'lookup 3 table'      │                               │
        ┌───────────────────────────┐     │                               │
        │  key:  3                  │     │                               │
    E3: │table: <table>             ├─────┤                               │
        └───────────────────────────┘     │                               │
                                          │                               │
                                          │                               │
              Call 'f x'                  │                               │
        ┌───────────────────────────┐     │                               │
        │n: 3                       ├─────┘                               │
   E4:  │                           │<────────────────────────────────┐   │
        └───────────────────────────┘<────────┐                       │   │
                                              │                       │   │
                                              │                       │   │
                  Call 'memo-fib 2'           │                       │   │
            ┌───────────────────────────┐     │                       │   │
            │x: 2                       ├─────┘                       │   │
        E5: │                           │<────────────────────────┐   │   │
            └───────────────────────────┘<────────┐               │   │   │
                                                  │               │   │   │
                                                  │               │   │   │
                       Call 'lookup 2 table'      │               │   │   │
                ┌───────────────────────────┐     │               │   │   │
                │  key: 2                   │     │               │   │   │
            E6: │table: <table>             ├─────┤               │   │   │
                └───────────────────────────┘     │               │   │   │
                                                  │               │   │   │
                                                  │               │   │   │
                      Call 'f x'                  │               │   │   │
                ┌───────────────────────────┐     │               │   │   │
                │n: 2                       ├─────┘               │   │   │
           E7:  │                           │<────────────────┐   │   │   │
                └───────────────────────────┘<────────┐       │   │   │   │
                                                      │       │   │   │   │
                                                      │       │   │   │   │
                          Call 'memo-fib 1'           │       │   │   │   │
                    ┌───────────────────────────┐     │       │   │   │   │
                    │x: 1                       ├─────┘       │   │   │   │
                E8: │                           │<────────┐   │   │   │   │
                    └───────────────────────────┘         │   │   │   │   │
                                                          │   │   │   │   │
                                                          │   │   │   │   │
                               Call 'lookup 1 table'      │   │   │   │   │
                        ┌───────────────────────────┐     │   │   │   │   │
                        │  key: 1                   │     │   │   │   │   │
                    E9: │table: <table>             ├─────┤   │   │   │   │
                        └───────────────────────────┘     │   │   │   │   │
                                                          │   │   │   │   │
                                                          │   │   │   │   │
                              Call 'f x'                  │   │   │   │   │
                        ┌───────────────────────────┐     │   │   │   │   │
                        │n: 1                       ├─────┤   │   │   │   │
                   E10: │                           │     │   │   │   │   │
                        └───────────────────────────┘     │   │   │   │   │
                                                          │   │   │   │   │
                                                          │   │   │   │   │
                        Call 'insert! x result table'     │   │   │   │   │
                        ┌───────────────────────────┐     │   │   │   │   │
                        │  key: 1                   │     │   │   │   │   │
                   E11: │value: 1                   ├─────┘   │   │   │   │
                        │table: <table>             │         │   │   │   │
                        └───────────────────────────┘         │   │   │   │
                                                              │   │   │   │
                                                              │   │   │   │
                          Call 'memo-fib 0'                   │   │   │   │
                    ┌───────────────────────────┐             │   │   │   │
                    │x: 0                       ├─────────────┘   │   │   │
               E12: │                           │<────────┐       │   │   │
                    └───────────────────────────┘         │       │   │   │
                                                          │       │   │   │
                                                          │       │   │   │
                               Call 'lookup 0 table'      │       │   │   │
                        ┌───────────────────────────┐     │       │   │   │
                        │  key: 0                   │     │       │   │   │
                   E13: │table: <table>             ├─────┤       │   │   │
                        └───────────────────────────┘     │       │   │   │
                                                          │       │   │   │
                                                          │       │   │   │
                              Call 'f x'                  │       │   │   │
                        ┌───────────────────────────┐     │       │   │   │
                        │n: 0                       ├─────┤       │   │   │
                   E14: │                           │     │       │   │   │
                        └───────────────────────────┘     │       │   │   │
                                                          │       │   │   │
                                                          │       │   │   │
                        Call 'insert! x result table'     │       │   │   │
                        ┌───────────────────────────┐     │       │   │   │
                        │  key: 0                   │     │       │   │   │
                   E15: │value: 0                   ├─────┘       │   │   │
                        │table: <table>             │             │   │   │
                        └───────────────────────────┘             │   │   │
                                                                  │   │   │
                                                                  │   │   │
                Call 'insert! x result table'                     │   │   │
                ┌───────────────────────────┐                     │   │   │
                │  key: 2                   │                     │   │   │
           E16: │value: 1                   ├─────────────────────┘   │   │
                │table: <table>             │                         │   │
                └───────────────────────────┘                         │   │
                                                                      │   │
                                                                      │   │
                  Call 'memo-fib 1'                                   │   │
            ┌───────────────────────────┐                             │   │
            │x: 1                       ├─────────────────────────────┘   │
       E17: │                           │<────────┐                       │
            └───────────────────────────┘         │                       │
                                                  │                       │
                                                  │                       │
                       Call 'lookup 1 table'      │                       │
                ┌───────────────────────────┐     │                       │
                │  key: 1                   │     │                       │
           E18: │table: <table>             ├─────┘                       │
                └───────────────────────────┘                             │
                                                                          │
                                                                          │
        Call 'insert! x result table'                                     │
        ┌───────────────────────────┐                                     │
        │  key: 3                   │                                     │
   E19: │value: 2                   ├─────────────────────────────────────┘
        │table: <table>             │ 
        └───────────────────────────┘
")

(--end-- "3.27")

