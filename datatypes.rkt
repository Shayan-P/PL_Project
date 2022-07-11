#lang racket

(require (lib "eopl.ss" "eopl"))

(provide stmt stmt? assign-stmt side-effect-stmt return-value-stmt return-novalue-stmt global-stmt pass-stmt continue-stmt break-stmt def-stmt if-stmt for-stmt
    expr expr? num-expr app-expr ident-expr list-expr
    param param? param-with-default is-num?
    val val? num-val proc-val none-val non-return list-val
    force-num force-proc force-list programmer-forbided-val is-programmer-forbided-val?)

(define-datatype param param?
    (param-with-default (name symbol?) (default expr?)))

(define-datatype stmt stmt?
    (assign-stmt (name symbol?) (value expr?))
    (side-effect-stmt (e expr?))
    (return-value-stmt (e expr?))
    (return-novalue-stmt)
    (global-stmt (e symbol?))
    (pass-stmt)
    (continue-stmt)
    (break-stmt)
    (def-stmt (name symbol?) (params (listof param?)) (statements (listof stmt?)))
    (if-stmt (condition expr?) (then-block (listof stmt?)) (else-block (listof stmt?)))
    (for-stmt (counter expr?) (count-set expr?) (statements (listof stmt?))))

(define-datatype expr expr?
    (num-expr (v number?))
    (ident-expr (v symbol?))
    (list-expr (v (listof expr?)))
    (app-expr (rator expr?) (rand expr?)))

(define-datatype val val?
    (num-val (v number?))
    (proc-val (f procedure?))
    (list-val (v (listof val?)))
    (none-val)
    (programmer-forbided-val))

(define non-return (lambda (s)  (programmer-forbided-val) ))
(define (is-programmer-forbided-val? v)
    (cases val v
        (programmer-forbided-val () #t )
        (else #f )))

(define (force-num v)
    (cases val v
        (num-val (r) r)
        (else (error 'value-is-not-number))))

(define (is-num? v)
    (cases val v
        (num-val (r) #t)
        (else #f )))

(define (force-proc v)
    (cases val v
        (proc-val (r) r)
        (else (error 'value-is-not-function))))

(define (force-list v)
    (cases val v
        (list-val (r) r)
        (else (error 'value-is-not-number))))
