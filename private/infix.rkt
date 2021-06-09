#lang racket/base

(provide infix:)

(require syntax/parse/define
         (only-in racket/list rest)
         (for-syntax racket/base
                     syntax/parse/class/paren-shape
                     "util/define-operator-classes.rkt")
         "aliases/division.rkt"
         "aliases/expt.rkt"
         "aliases/not-equal.rkt")

(begin-for-syntax
  (define-operator-classes
    op-or         (or)
    op-and        (and)
    op-not        (not)
    op-equal      (eq? eqv? equal? = not-eq? not-eqv? not-equal? !=)
    op-inequality (< <= > >=)
    op-add-sub    (+ -)
    op-mul-div    (* / modulo quotient remainder // % quotient/remainder)
    op-expt       (^ expt)))
    
(define-syntax-parser infix:
  #:track-literals
    
  [(_ (~parens e e0 ...+))
   #'(infix: e e0 ...)]
    
  [(_ lhs ...+ binary-op:op-or rhs ...+)
   #'(binary-op (infix: lhs ...)
                (infix: rhs ...))]
    
  [(_ lhs ...+ binary-op:op-and rhs ...+)
   #'(binary-op (infix: lhs ...)
                (infix: rhs ...))]
    
  [(_ unary-op:op-not rhs ...+)
   #'(unary-op (infix: rhs ...))]
  
  [(_ lhs:not-op-equal ...+
      (~seq op:op-equal rhs:not-op-equal ...+) ...+)
   #'(let* ([lhs-list (list (infix: lhs ...) (infix: rhs ...) ...)]
            [rhs-list (rest lhs-list)]
            [ops-list (list op ...)])
       (for/and ([lhs* (in-list lhs-list)]
                 [rhs* (in-list rhs-list)]
                 [op*  (in-list ops-list)])
         (op* lhs* rhs*)))]
    
  [(_ lhs:not-op-inequality ...+
      (~seq op:op-inequality rhs:not-op-inequality ...+) ...+)
   #'(let* ([lhs-list (list (infix: lhs ...) (infix: rhs ...) ...)]
            [rhs-list (rest lhs-list)]
            [ops-list (list op ...)])
       (for/and ([lhs* (in-list lhs-list)]
                 [rhs* (in-list rhs-list)]
                 [op*  (in-list ops-list)])
         (op* lhs* rhs*)))]
    
  [(_ lhs ...+ binary-op:op-add-sub rhs ...+)
   #'(binary-op (infix: lhs ...)
                (infix: rhs ...))]
    
  [(_ lhs ...+ binary-op:op-mul-div rhs ...+)
   #'(binary-op (infix: lhs ...)
                (infix: rhs ...))]
    
  [(_ lhs ...+ binary-op:op-expt rhs ...+)
   #'(binary-op (infix: lhs ...)
                (infix: rhs ...))]
    
  [(_ f xs ...+)
   #'(f xs ...)]
    
  [(_ x)
   #'x])

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
