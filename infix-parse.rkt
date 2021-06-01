#lang racket/base

(provide infix-parse)

(require
  (for-syntax racket/base
              (only-in racket/list rest)
              syntax/parse
              syntax/parse/class/paren-shape))

(begin-for-syntax
  (define-syntax-class op-not     (pattern (~literal not)))
  (define-syntax-class op-and     (pattern (~literal and)))
  (define-syntax-class op-or      (pattern (~literal or)))
  (define-syntax-class op-add-sub (pattern (~literal +))
                                  (pattern (~literal -)))
  (define-syntax-class op-mul-div (pattern (~literal *))
                                  (pattern (~literal /))
                                  (pattern (~literal modulo))
                                  (pattern (~literal quotient))
                                  (pattern (~literal remainder))
                                  (pattern (~literal quotient/remainder)))
  (define-syntax-class op-equal   (pattern (~literal eq?))
                                  (pattern (~literal eqv?))
                                  (pattern (~literal equal?))
                                  (pattern (~literal =))
                                  (pattern (~literal not-eq?))
                                  (pattern (~literal not-eqv?))
                                  (pattern (~literal not-equal?))
                                  (pattern (~literal !=)))
  (define-syntax-class op-inequal (pattern (~literal <))
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
      [_:op-inequal #t]
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
    [(_ l ...+ op1:op-inequal m ...+
              (op2:op-inequal r ...+) ...)      (parse-inequality stx)]
    [(_ l ...+ binary-op:op-add-sub   r ...+) #'(binary-op (infix-parse l ...) (infix-parse r ...))]
    [(_ l ...+ binary-op:op-mul-div   r ...+) #'(binary-op (infix-parse l ...) (infix-parse r ...))]
    [(_ f xs ...+)                            #'(f xs ...)]
    [(_ x)                                    #'x]))
