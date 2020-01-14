#lang sicp

(#%require "common.scm")

;   Exercise 5.12
;   =============
;   
;   The simulator can be used to help determine the data paths required for
;   implementing a machine with a given controller.  Extend the assembler to
;   store the following information in the machine model:
;   
;   * a list of all instructions, with duplicates removed, sorted by
;   instruction type (assign, goto, and so on);
;   
;   * a list (without duplicates) of the registers used to hold entry points
;   (these are the registers referenced by goto instructions);
;   
;   * a list (without duplicates) of the registers that are saved or
;   restored;
;   
;   * for each register, a list (without duplicates) of the sources from
;   which it is assigned (for example, the sources for register val in the
;   factorial machine of figure [5.11] are (const 1) and ((op *) (reg n)
;   (reg val))).
;   
;   Extend the message-passing interface to the machine to provide access to
;   this new information.  To test your analyzer, define the Fibonacci
;   machine from figure [5.12] and examine the lists you constructed.
;   
;   ------------------------------------------------------------------------
;   [Exercise 5.12]: http://sicp-book.com/book-Z-H-32.html#%_thm_5.12
;   [Figure 5.11]:   http://sicp-book.com/book-Z-H-31.html#%_fig_5.11
;   [Figure 5.12]:   http://sicp-book.com/book-Z-H-31.html#%_fig_5.12
;   5.2.3 Generating Execution Procedures for Instructions - p530
;   ------------------------------------------------------------------------

(-start- "5.12")



(--end-- "5.12")

