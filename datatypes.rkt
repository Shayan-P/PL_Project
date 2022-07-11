#lang racket

(require (lib "eopl.ss" "eopl"))

(provide stmt stmt? assign-stmt side-effect-stmt return-value-stmt return-novalue-stmt global-stmt pass-stmt continue-stmt break-stmt def-stmt if-stmt for-stmt
    expr expr? num-expr app-expr ident-expr list-expr end-of-args-expr end-of-args-val is-end-of-args-val?
    param param? param-with-default is-num? is-break? break-val is-continue? continue-val is-bool? not-found-val is-not-found-val?
    val val? num-val proc-val none-val non-return list-val get-name-arg get-defualt-arg bool-val force-bool 
    force-num force-proc force-list programmer-forbided-val is-programmer-forbided-val?)

(define-datatype param param?
    (param-with-default (name symbol?) (default expr?)))

(define (get-name-arg e)
    (cases param e
        (param-with-default (name defualt) name)))

(define (get-defualt-arg e)
    (cases param e
        (param-with-default (name defualt) defualt)))

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
    (for-stmt (counter symbol?) (count-set expr?) (statements (listof stmt?))))

(define-datatype expr expr?
    (end-of-args-expr)
    (num-expr (v number?))
    (ident-expr (v symbol?))
    (list-expr (v (listof expr?)))
    (app-expr (rator expr?) (rand expr?)))


(define-datatype val val?
    (num-val (v number?))
    (proc-val (f procedure?))
    (list-val (v (listof val?)))
    (bool-val (v boolean?))
    (none-val)
    (programmer-forbided-val)
    (break-val)
    (continue-val)
    (end-of-args-val)
    (not-found-val (name symbol?)))

(define non-return (lambda (s)  (programmer-forbided-val) ))
(define (is-programmer-forbided-val? v)
    (cases val v
        (programmer-forbided-val () #t )
        (else #f )))


(define debug (lambda (s) (let [(t1 (pretty-print `************))(t2 (pretty-print s))(t3 (pretty-print `*********))] s)))

(define (is-break? v)
    (cases val v
        (break-val () #t )
        (else #f )))

(define (is-not-found-val? v)
    (cases val v
        (not-found-val (name) #t )
        (else #f )))

(define (is-end-of-args-val? v)
    (cases val v
        (end-of-args-val () #t )
        (else #f )))

(define (is-continue? v)
    (cases val v
        (continue-val () #t )
        (else #f )))

(define (force-num v)
    (cases val v
        (num-val (r) r)
        (else (error 'value-is-not-number))))

(define (is-num? v)
    (cases val v
        (num-val (r) #t)
        (else #f )))


(define (force-bool v)
    (cases val v
        (bool-val (r) r)
        (else (error 'value-is-not-number))))

(define (is-bool? v)
    (cases val v
        (bool-val (r) #t)
        (else #f )))

(define (force-proc v)
    (cases val v
        (proc-val (r) r)
        (else (error 'value-is-not-function))))

(define (force-list v)
    (cases val v
        (list-val (r) r)
        (else (error 'value-is-not-number))))
