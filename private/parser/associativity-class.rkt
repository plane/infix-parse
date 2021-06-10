#lang racket/base

(provide associativity)

(require syntax/parse)

(define-syntax-class associativity
  (pattern (~literal left))
  (pattern (~literal right)))

