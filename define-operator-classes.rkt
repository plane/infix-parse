#lang racket/base

(provide define-operator-classes)

(require syntax/parse
         syntax/parse/define)

(require 
  (for-syntax racket/base                
              racket/syntax
              syntax/parse
              syntax/parse/define))

(begin-for-syntax 
  (define-syntax-class op-class-name
    (pattern name:id
      #:attr negation (format-id #'name "not-~a" (syntax-e #'name))
      #:attr pred     (format-id #'name "~a?"    (syntax-e #'name)))))

(define-syntax-parser define-operator-classes
  [(_ (~seq name:op-class-name (literal-id:id ...+)) ...+)
   #'(begin
       (define (name.pred stx)
         (syntax-parse stx [(~var _ name) #t] [else #f])) ...
       (define-syntax-class name
         (pattern (~literal literal-id)) ...) ...
       (define-syntax-class name.negation
         (pattern (~not (~or (~literal literal-id) ...)))) ...)])

