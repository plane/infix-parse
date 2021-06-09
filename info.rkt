#lang info

(define collection "infix-parse")
(define pkg-authors '("crystal@panix.com"))
(define version "0.2")
(define scribblings
 '(("infix-parse.scrbl" () (library) "infix-parse")))
(define pkg-desc "Infix expression parsing")
(define deps '("base"
               "reprovide-lang-lib"
               "syntax-classes-lib"))
(define build-deps '("rackunit-lib"
                     "rackunit-chk"
                     "racket-doc"
                     "scribble-lib"))
(define compile-omit-paths '("examples"))

