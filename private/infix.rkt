#lang racket/base

(provide infix:)

(require "define.rkt"
         "aliases/division.rkt"
         "aliases/expt.rkt"
         "aliases/not-equal.rkt")

(define-infix-parser infix:
  binary    (or)
  binary    (and)
  unary     (not)
  variadic  (eq? eqv? equal? = not-eq? not-eqv? not-equal? !=)
  variadic  (< <= > >=)
  binary    (+ -)
  binary    (* / modulo quotient remainder // % quotient/remainder)
  binary    (^ expt))

(module+ test
  (require rackunit/chk)
  (chk
   (infix: 1) 1
   (infix: 1 + 2) 3
   (infix: 1 + 2 * 3) 7
   (infix: (1 + 2) * 3) 9
   (infix: 3 - 2 - 1) 0
   (infix: 3 - (2 - 1)) 2
   (infix: 2 / 3) 2/3
   (infix: 1 + 2 / 3) 5/3
   (infix: (1 + 2) / 3) 1
   (infix: 5 quotient 3) 1
   (infix: 5 remainder 3) 2
   (infix: 5 quotient/remainder 3) (values 1 2)
   (infix: 5 // 3) 1
   (infix: 5 % 3) 2
   (infix: 2 * 3 ^ 2) 18
   (infix: 'a) 'a
   (infix: 'a equal? 'b) #f
   (infix: 'a eqv? 'a) #t
   (infix: 3 - 2 = 1) #t
   (infix: 1 = 3 - 2) #t
   (infix: #f) #f
   (infix: not #f) #t
   (infix: not not #f) #f
   (infix: #t or #f) #t
   (infix: #f or #f) #f
   (infix: #t and #f) #f
   (infix: #t and #t) #t
   (infix: #t or #f and #f) #t
   (infix: #t and #t or #f) #t
   (infix: #t and #t or #f) #t
   (infix: not #f and #t) #t
   (infix: not not #f and #t) #f
   (infix: not #f or #f) #t
   (infix: not #f or not #f) #t
   (infix: #f and 1 / 0 > 2) #f
   (infix: 1 < 2) #t
   (infix: 1 > 2) #f
   (infix: 1 + 1 > 2) #f
   (infix: 1 < 1 + 2) #t
   (infix: 1 < 2 < 3) #t
   (infix: 1 > 2 > 3) #f
   (infix: 0 < 1 < 2) #t
   (infix: 0 < 2 < 2) #f
   (infix: 0 < 2 <= 2) #t
   (infix: 0 < 1 + 2 < 3) #f
   (infix: 0 < 1 + 2 <= 3) #t))
