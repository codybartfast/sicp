#lang sicp

(#%require "common.scm")

;   Exercise 5.5
;   ============
;
;   Hand-simulate the factorial and Fibonacci machines, using some
;   nontrivial input (requiring execution of at least one recursive call).
;   Show the contents of the stack at each significant point in the
;   execution.
;
;   ------------------------------------------------------------------------
;   [Exercise 5.5]:  http://sicp-book.com/book-Z-H-31.html#%_thm_5.5
;   5.1.4 Using a Stack to Implement Recursion - p512
;   ------------------------------------------------------------------------

(-start- "5.5")

(println
 "
Factorial
=========

State for factorial 4 at each label:

================    ================    ================    ================
fact-loop           fact-loop           fact-loop           fact-loop
----------------    ----------------    ----------------    ----------------
n=4 c=done          n=3 c=aft           n=2 c=aft           n=1 c=aft
----------------    ----------------    ----------------    ----------------
----------------    fact-done           fact-done           fact-done
                    4                   4                   4
                    ----------------    after-fact          after-fact
                                        3                   3
                                        ----------------    after-fact
                                                            2
                                                            ----------------


================    ================    ================    ================
base-case           after-fact          after-fact          after-fact
----------------    ----------------    ----------------    ----------------
n=1 c=aft           n=1 c=aft v=1       n=2 c=aft v=2       n=3 c=aft v=6
----------------    ----------------    ----------------    ----------------
fact-done           fact-done           fact-done           fact-done
4                   4                   4                   4
after-fact          after-fact          after-fact          ----------------
3                   3                   3
after-fact          after-fact          ----------------
2                   2
----------------    ----------------


================
fact-done
----------------
n=4 c=done v=24
----------------
----------------


Fibomacci
=========

State for 4th Fibonacci at each label

================    ================    ================    ================
fib-loop            fib-loop            fib-loop            fib-loop
----------------    ----------------    ----------------    ----------------
n=4 c=done          n=3 c=afn-1         n=2 c=afn-1         n=1 c=afn-1
----------------    ----------------    ----------------    ----------------
----------------    fib-done            fib-done            fib-done
                    4                   4                   4
                    ----------------    afterfib-n-1        afterfib-n-1
                                        3                   3
                                        ----------------    afterfib-n-1
                                                            2
                                                            ----------------


================    ================    ================    ================
immediate-answer    afterfib-n-1        fib-loop            immediate-answer
----------------    ----------------    ----------------    ----------------
n=1 c=afn-1         n=1 c=afn-1 v=1     n=0 c=afn-2 v=1     n=0 c=afn-2 v=1
----------------    ----------------    ----------------    ----------------
fib-done            fib-done            fib-done            fib-done
4                   4                   4                   4
afterfib-n-1        afterfib-n-1        afterfib-n-1        afterfib-n-1
3                   3                   3                   3
afterfib-n-1        afterfib-n-1        afterfib-n-1        afterfib-n-1
2                   2                   1                   1
----------------    ----------------    ----------------    ----------------


================    ================    ================    ================
afterfib-n-2        afterfib-n-1        fib-loop            immediate-answer
----------------    ----------------    ----------------    ----------------
n=0 c=afn-2 v=0     n=0 c=afn-1 v=1     n=1 c=afn-2 v=1     n=1 c=afn-2 v=1
----------------    ----------------    ----------------    ----------------
fib-done            fib-done            fib-done            fib-done
4                   4                   4                   4
afterfib-n-1        afterfib-n-1        afterfib-n-1        afterfib-n-1
3                   3                   1                   1
afterfib-n-1        ----------------    ----------------    ----------------
1
----------------


================    ================    ================    ================
afterfib-n-2        afterfib-n-1        fib-loop            fib-loop
----------------    ----------------    ----------------    ----------------
n=1 c=afn-2 v=1     n=1 c=afn-1 v=2     n=2 c=afn-2 v=2     n=1 c=afn-1 v=2
----------------    ----------------    ----------------    ----------------
fib-done            fib-done            fib-done            fib-done
4                   4                   2                   2
afterfib-n-1       -----------------    ----------------    afterfib-n-2
1                                                           2
----------------                                            ----------------


================    ================    ================    ================
immediate-answer    afterfib-n-1        fib-loop            immediate-answer
----------------    ----------------    ----------------    ----------------
n=1 c=afn-1 v=2     n=1 c=afn-1 v=1     n=0 c=afn-2 v=1     n=0 c=afn-2 v=1
----------------    ----------------    ----------------    ----------------
fib-done            fib-done            fib-done            fib-done
2                   2                   2                   2
afterfib-n-2        afterfib-n-2        afterfib-n-2        afterfib-n-2
2                   2                   1                   1
----------------    ----------------    ----------------    ----------------


================    ================    ================
afterfib-n-2        afterfib-n-2        fib-done
----------------    ----------------    ----------------
n=0 c=afn-2 v=0     n=0 c=afn-2 v=1     n=1 c=done v=3
----------------    ----------------    ----------------
fib-done            fib-done            ----------------
2                   2
afterfib-n-2        ----------------
1
----------------

Wish I had picked 3 instead of 4.
")

(--end-- "5.5")