#lang racket/base

(provide infix-parse)

(require
  (for-syntax racket/base
              (only-in racket/list rest)
              syntax/parse
              syntax/parse/class/paren-shape))

(begin-for-syntax
  (define-syntax-class op-not        (pattern (~literal not)))
  (define-syntax-class op-and        (pattern (~literal and)))
  (define-syntax-class op-or         (pattern (~literal or)))
  (define-syntax-class op-add-sub    (pattern (~literal +))
                                     (pattern (~literal -)))
  (define-syntax-class op-mul-div    (pattern (~literal *))
                                     (pattern (~literal /))
                                     (pattern (~literal modulo))
                                     (pattern (~literal quotient))
                                     (pattern (~literal remainder))
                                     (pattern (~literal quotient/remainder)))
  (define-syntax-class op-equal      (pattern (~literal eq?))
                                     (pattern (~literal eqv?))
                                     (pattern (~literal equal?))
                                     (pattern (~literal =))
                                     (pattern (~literal not-eq?))
                                     (pattern (~literal not-eqv?))
                                     (pattern (~literal not-equal?))
                                     (pattern (~literal !=)))
  (define-syntax-class op-inequality (pattern (~literal <))
                                     (pattern (~literal <=))
                                     (pattern (~literal >))
                                     (pattern (~literal >=)))

  (define (split-partition delim? lst)
    (for/foldr ([finished '()]
                [current '()]
                [delims '()]
                #:result (values (cons current finished) delims))
               ([input (in-list lst)])
      (if (delim? input)
          (values (cons current finished)
                  '()
                  (cons input delims))
          (values finished
                  (cons input current)
                  delims))))

  (define (inequality? stx)
    (syntax-parse stx
      [_:op-inequality #t]
      [else #f]))
  
  (define (add-prefix stx)
    (cons #'infix-parse stx))

  (define (make-tmp num)
    (generate-temporaries (build-list num values)))
  
  (define (parse-inequality stx)
    (define tokens (rest (syntax->list stx)))
    (define-values (operands operators)
      (split-partition inequality? tokens))
    (define tmp (make-tmp (length operands)))
    (define prefixed (map add-prefix operands))
    (define ops
      (for/list ([lhs (in-list tmp)]
                 [rhs (in-list (rest tmp))]
                 [op  (in-list operators)])
        #`(#,op #,lhs #,rhs)))
    
    #`(let-values ([(#,@tmp) (values #,@prefixed)])
        (and #,@ops))))

(define-syntax (infix-parse stx)
  (syntax-parse stx
    #:track-literals
    #:datum-literals (or
                      and
                      not
                      + -
                      * / quotient remainder quotient/remainder
                      eq? eqv? equal? = not-eq? not-eqv? not-equal? !=
                      < <= > >=)
    [(_ (~parens e e0 ...+))                  #'(infix-parse e e0 ...)]
    [(_ l ...+ binary-op:op-or        r ...+) #'(binary-op (infix-parse l ...) (infix-parse r ...))]
    [(_ l ...+ binary-op:op-and       r ...+) #'(binary-op (infix-parse l ...) (infix-parse r ...))]
    [(_         unary-op:op-not       r ...+) #'( unary-op                     (infix-parse r ...))]
    [(_ l ...+ binary-op:op-equal     r ...+) #'(binary-op (infix-parse l ...) (infix-parse r ...))]
    [(_ l ...+ op1:op-inequality m ...+
              (op2:op-inequality r ...+) ...)   (parse-inequality stx)]
    [(_ l ...+ binary-op:op-add-sub   r ...+) #'(binary-op (infix-parse l ...) (infix-parse r ...))]
    [(_ l ...+ binary-op:op-mul-div   r ...+) #'(binary-op (infix-parse l ...) (infix-parse r ...))]
    [(_ f xs ...+)                            #'(f xs ...)]
    [(_ x)                                    #'x]))

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
   (infix-parse 1 < 2 < 3) #t
   (infix-parse 1 > 2 > 3) #f
   (infix-parse 0 < 1 < 2) #t
   (infix-parse 0 < 2 < 2) #f
   (infix-parse 0 < 2 <= 2) #t
   (infix-parse 0 < 1 + 2 < 3) #f
   (infix-parse 0 < 1 + 2 <= 3) #t))
