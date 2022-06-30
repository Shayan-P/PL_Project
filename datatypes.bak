#lang racket

(require (lib "eopl.ss" "eopl"))

(provide stmt stmt? assign-stmt side-effect-stmt
    expr expr? num-expr app-expr ident-expr
    val val? num-val proc-val none-val
    force-num force-proc)

(define-datatype stmt stmt?
    (assign-stmt (name symbol?) (value expr?))
    (side-effect-stmt (e expr?)))

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
