# Chapter 08 - Defining our own macros

## Simple steps to writing a macro:
1. Write a **simple call** to the macro and the code it should expand into, or vice versa.
```lisp
;; from this simple call
(do-primes(p 0 19)
  (format t "~d " p))
  
;; to this expanded version
(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
   (format t "~d " p))
```
2. Write code that generates the handwritten expansion from the arguments in the sample call
3. Make sure the macro abstraction doesnt **leak**.
