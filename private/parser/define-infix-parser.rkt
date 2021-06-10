#lang racket/base

(provide define-infix-parser
         define-simple-infix-parser)

(require syntax/parse/define)

(require
  (for-syntax racket/base
              syntax/parse/class/paren-shape
              "operator-class-name.rkt"
              "define-operator-classes.rkt"
              "associativity-class.rkt"))

(define-syntax-parser define-infix-parser
  #:track-literals
  #:datum-literals (unary binary chained
                    left right)  
  [(_ parser-name:id
      #:operator-classes 
        (~seq arity
              operator-class-name:operator-class-name
              (literal-id:id ...+)
              (~optional (~seq #:associate assoc-kw:associativity))) ...+
      #:precedence
        op-class:operator-class-name ...+)
   #:with ooo (quote-syntax ...)
   #'(begin
       (begin-for-syntax 
         (define-operator-classes parser-name 
           (arity
            operator-class-name
            (literal-id ...)
            (~? (~@ #:associate assoc-kw))) ...))
       (define-syntax-parser parser-name
         [(_ (~parens e e0 ...+))     #'(parser-name e e0 ooo)]
         [(_ (~var op op-class.expr)) #'op.result] ...
         [(_ f xs ...+)               #'(f xs ooo)]
         [(_ x)                       #'x]))])

(define-syntax-parser define-simple-infix-parser
  #:track-literals
  #:datum-literals (unary binary chained
                    left right)
  [(_ parser-name:id
      (~seq arity
            (literal-id:id ...+)
            (~optional (~seq #:associate assoc-kw:associativity))) ...+)
   #:with (class-name ...) (generate-temporaries #'(arity ...))
   #'(define-infix-parser parser-name
       #:operator-classes
         (~@ arity
             class-name
             (literal-id ...)
             (~? (~@ #:associate assoc-kw))) ...
       #:precedence
         class-name ...)])
