#lang racket

(require (lib "eopl.ss" "eopl"))

(provide stmt stmt? assign-stmt side-effect-stmt return-value-stmt return-novalue-stmt global-stmt pass-stmt continue-stmt break-stmt def-stmt if-stmt for-stmt
    expr expr? num-expr app-expr ident-expr
    param param? param-with-default
    val val? num-val proc-val none-val
    force-num force-proc)

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
    (def-stmt (name symbol?) (params list?) (statements list?))
    (if-stmt (condition expr?) (statements list?) (else-block list?))
    (for-stmt (counter expr?) (count-set list?) (statements list?)))

(define-datatype expr expr?
    (num-expr (v number?))
    (ident-expr (v symbol?))
    (app-expr (rator expr?) (rand expr?)))

(define-datatype val val?
    (num-val (v number?))
    (proc-val (f procedure?))
    (none-val))

(define (force-num v)
    (cases val v
        (num-val (r) r)
        (else (error 'value-is-not-number))))

(define (force-proc v)
    (cases val v
        (proc-val (r) r)
        (else (error 'value-is-not-function))))
