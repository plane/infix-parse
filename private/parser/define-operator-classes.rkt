#lang racket/base

(provide define-operator-classes)

(require syntax/parse
         syntax/parse/define)

(require (for-syntax "operator-class-name.rkt")
  (for-syntax racket/base                
              syntax/parse)
  (for-template racket/base
                (only-in racket/list rest)))

(define-syntax-parser define-unary-operator-class
  #:track-literals
  [(_ parser:id name:operator-class-name literal-id:id ...+)
   #'(begin
       (define-syntax-class name
         (pattern (~literal literal-id)) ...)
       (define-splicing-syntax-class name.expr
         (pattern (~seq (~var op name) rhs ...+)
           #:attr result
             #'(... (op (parser rhs ...))))))])

(define-syntax-parser define-binary-operator-class
  #:track-literals
  [(_ parser:id name:operator-class-name literal-id:id ...+)
   #'(begin
       (define-syntax-class name
         (pattern (~literal literal-id)) ...)
       (define-splicing-syntax-class name.expr
         (pattern (~seq lhs ...+ (~var op name) rhs ...+)
           #:attr result
             #'(... (op (parser lhs ...)
                        (parser rhs ...))))))])

(define-syntax-parser define-chained-operator-class
  #:track-literals
  [(_ parser:id name:operator-class-name literal-id:id ...+)
   #'(begin
       (define-syntax-class name
         (pattern (~literal literal-id)) ...)
       (define-syntax-class name.is-not
         (pattern (~not (~or (~literal literal-id) ...))))
       (define-splicing-syntax-class name.expr
         (pattern (~seq (~var lhs name.is-not) ...+
                        (~seq (~var op name)
                              (~var rhs name.is-not) ...+) ...+)
           #:attr result
             #'(... (let* ([lhs-list (list (parser lhs ...)
                                           (parser rhs ...) ...)]
                           [rhs-list (rest lhs-list)]
                           [ops-list (list op ...)])
                      (for/and ([lhs* (in-list lhs-list)]
                                [rhs* (in-list rhs-list)]
                                [op*  (in-list ops-list)])
                        (op* lhs* rhs*)))))))])

(define-syntax-parser define-operator-class
  #:track-literals
  #:datum-literals (unary binary chained)
  [(_ parser:id unary name:operator-class-name literal-id:id ...+)
   #'(define-unary-operator-class parser name literal-id ...)]
  
  [(_ parser:id binary name:operator-class-name literal-id:id ...+)
   #'(define-binary-operator-class parser name literal-id ...)]
  
  [(_ parser:id chained name:operator-class-name literal-id:id ...+)
   #'(define-chained-operator-class parser name literal-id ...)])

(define-syntax-parser define-operator-classes
  #:track-literals
  #:datum-literals (unary binary chained)
  [(_ parser:id ((~seq arity name:operator-class-name (literal-id:id ...+))) ...+)
   #'(begin
       (define-operator-class parser arity name literal-id ...) ...)])
