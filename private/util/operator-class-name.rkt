#lang racket/base

(provide operator-class-name)

(require racket/syntax
         syntax/parse)

(define-syntax-class operator-class-name
  (pattern name:id
    #:attr expr   (format-id #'name "~a-expr" (syntax-e #'name))
    #:attr is-not (format-id #'name "not-~a"  (syntax-e #'name))))
