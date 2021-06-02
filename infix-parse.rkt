#lang racket/base

(provide infix-parse)

(require syntax/parse/define
         (only-in racket/list rest)
         (for-syntax racket/base
                     syntax/parse/class/paren-shape
                     "define-operator-classes.rkt"))

(begin-for-syntax
  (define-operator-classes
    op-not        (not)
    op-and        (and)
    op-or         (or)
    op-add-sub    (+ -)
    op-mul-div    (* / modulo quotient remainder quotient/remainder)
    op-equal      (eq? eqv? equal? = not-eq? not-eqv? not-equal? !=)
    op-inequality (< <= > >=)))

(define-syntax-parser infix-parse
  #:track-literals
    
  [(_ (~parens e e0 ...+))
   #'(infix-parse e e0 ...)]
    
  [(_ lhs ...+ binary-op:op-or rhs ...+)
   #'(binary-op (infix-parse lhs ...)
                (infix-parse rhs ...))]
    
  [(_ lhs ...+ binary-op:op-and rhs ...+)
   #'(binary-op (infix-parse lhs ...)
                (infix-parse rhs ...))]
    
  [(_ unary-op:op-not rhs ...+)
   #'(unary-op (infix-parse rhs ...))]
    
  [(_ lhs ...+ binary-op:op-equal rhs ...+)
   #'(binary-op (infix-parse lhs ...)
                (infix-parse rhs ...))]
    
  [(_ lhs:not-op-inequality ...+
      (~seq op:op-inequality
            rhs:not-op-inequality ...+) ...+)
   #:with (ops ...) #'(op ...)
   #:with (tmp ...) (generate-temporaries
                     #'((lhs ...) (rhs ...) ...))
   #'(let*-values
         ([(tmp ...) (values (infix-parse lhs ...)
                             (infix-parse rhs ...) ...)]
          [(tmps)    (list tmp ...)])
       (for/and ([lhs* (in-list tmps)]
                 [rhs* (in-list (rest tmps))]
                 [op*  (in-list (list ops ...))])
         (op* lhs* rhs*)))]
    
  [(_ lhs ...+ binary-op:op-add-sub rhs ...+)
   #'(binary-op (infix-parse lhs ...)
                (infix-parse rhs ...))]
    
  [(_ lhs ...+ binary-op:op-mul-div rhs ...+)
   #'(binary-op (infix-parse lhs ...)
                (infix-parse rhs ...))]
    
  [(_ f xs ...+)
   #'(f xs ...)]
    
  [(_ x)
   #'x])

(module+ test
  (require rackunit/chk)
  (chk
   (infix-parse 1) 1
   (infix-parse 1 + 2) 3
   (infix-parse 1 + 2 * 3) 7
   (infix-parse (1 + 2) * 3) 9
   (infix-parse 3 - 2 - 1) 0
   (infix-parse 3 - (2 - 1)) 2
   (infix-parse 2 / 3) 2/3
   (infix-parse 1 + 2 / 3) 5/3
   (infix-parse (1 + 2) / 3) 1
   (infix-parse 5 quotient 3) 1
   (infix-parse 5 remainder 3) 2
   (infix-parse 5 quotient/remainder 3) (values 1 2)
   (infix-parse 'a) 'a
   (infix-parse 'a equal? 'b) #f
   (infix-parse 'a eqv? 'a) #t
   (infix-parse 3 - 2 = 1) #t
   (infix-parse 1 = 3 - 2) #t
   (infix-parse #f) #f
   (infix-parse not #f) #t
   (infix-parse not not #f) #f
   (infix-parse #t or #f) #t
   (infix-parse #f or #f) #f
   (infix-parse #t and #f) #f
   (infix-parse #t and #t) #t
   (infix-parse #t or #f and #f) #t
   (infix-parse #t and #t or #f) #t
   (infix-parse #t and #t or #f) #t
   (infix-parse not #f and #t) #t
   (infix-parse not not #f and #t) #f
   (infix-parse not #f or #f) #t
   (infix-parse not #f or not #f) #t
   (infix-parse #f and 1 / 0 > 2) #f
   (infix-parse 1 < 2) #t
   (infix-parse 1 > 2) #f
   (infix-parse 1 + 1 > 2) #f
   (infix-parse 1 < 1 + 2) #t
   (infix-parse 1 < 2 < 3) #t
   (infix-parse 1 > 2 > 3) #f
   (infix-parse 0 < 1 < 2) #t
   (infix-parse 0 < 2 < 2) #f
   (infix-parse 0 < 2 <= 2) #t
   (infix-parse 0 < 1 + 2 < 3) #f
   (infix-parse 0 < 1 + 2 <= 3) #t))

