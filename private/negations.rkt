#lang racket/base

(provide not-eq?
         not-eqv?
         not-equal?
         !=)

(define not-eq?    (compose not eq?))
(define not-eqv?   (compose not eqv?))
(define not-equal? (compose not equal?))
(define !=         (compose not =))

