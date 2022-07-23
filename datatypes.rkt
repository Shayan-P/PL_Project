#lang racket

(require (lib "eopl.ss" "eopl"))

(provide stmt stmt? assign-stmt side-effect-stmt return-value-stmt return-novalue-stmt global-stmt pass-stmt continue-stmt break-stmt def-stmt if-stmt for-stmt
    expr expr? num-expr app-expr ident-expr list-expr end-of-args-expr end-of-args-val is-end-of-args-val?
    param param? param-with-default is-num? is-break? break-val is-continue? continue-val is-bool? not-found-val is-not-found-val?
    val val? num-val proc-val none-val non-return list-val get-name-arg get-defualt-arg bool-val force-bool tank is-tank? 
    force-num force-proc force-list programmer-forbided-val is-programmer-forbided-val? program program? prog
    type type? int-type bool-type float-type list-type none-type complex-type complex-type? primitive-type unknown-type dummy-type function-type or-type 
    or-type? unknown-type? function-type? end-of-args-type? end-of-args-type statement-return-type)

(define-datatype param param?
    (param-with-default (name symbol?) (a-type complex-type?) (default expr?)))

(define (get-name-arg e)
    (cases param e
        (param-with-default (name otype defualt) name)))

(define (get-defualt-arg e)
    (cases param e
        (param-with-default (name otype defualt) defualt)))

(define-datatype program program?
    (prog (lines (listof stmt?)) (type-check-enable boolean?)))

(define-datatype stmt stmt?
    (assign-stmt (name symbol?) (a-type complex-type?) (value expr?))
    (side-effect-stmt (e expr?))
    (return-value-stmt (e expr?))
    (return-novalue-stmt)
    (global-stmt (e symbol?))
    (pass-stmt)
    (continue-stmt)
    (break-stmt)
    (def-stmt (name symbol?) (params (listof param?)) (a-type complex-type?) (statements (listof stmt?)))
    (if-stmt (condition expr?) (then-block (listof stmt?)) (else-block (listof stmt?)))
    (for-stmt (counter symbol?) (count-set expr?) (statements (listof stmt?))))

(define-datatype expr expr?
    (end-of-args-expr)
    (num-expr (v number?))
    (ident-expr (v symbol?))
    (list-expr (v (listof expr?)))
    (app-expr (rator expr?) (rand expr?)))


(define-datatype val val?
    (tank (exp expr?) (local-env box?) (global-env box?))
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

(define-datatype type type?
    (int-type)
    (float-type)
    (bool-type)
    (list-type)
    (none-type))

(define-datatype complex-type complex-type?
    (primitive-type (pr-type type?))
    (unknown-type)
    (end-of-args-type)
    (dummy-type) ; matches the input of functions with no parameter
    (statement-return-type)
    (or-type (case1 complex-type?) (case2 complex-type?))
    (function-type (from complex-type?) (to complex-type?)))

(define (function-type? a-type)
    (cases complex-type a-type
        (function-type (a b) #t)
        (else #f)))

(define (unknown-type? a-type)
    (cases complex-type a-type
        (unknown-type () #t)
        (else #f)))

(define (end-of-args-type? a-type)
    (cases complex-type a-type
        (end-of-args-type () #t)
        (else #f)))


(define (or-type? a-type)
    (cases complex-type a-type
        (or-type (a b) #t)
        (else #f)))

(define non-return (lambda (s)  (programmer-forbided-val) ))
(define (is-programmer-forbided-val? v)
    (cases val v
        (programmer-forbided-val () #t )
        (else #f )))

(define (is-tank? v)
    (cases val v
        (tank (e local-env global-env) #t )
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
        (else (error 'value-is-not-bool))))

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
        (else (error 'value-is-not-list))))
