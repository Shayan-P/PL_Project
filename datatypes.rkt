#lang racket

(require (lib "eopl.ss" "eopl"))

(provide stmt stmt? assign-stmt side-effect-stmt return-value-stmt return-novalue-stmt global-stmt pass-stmt continue-stmt break-stmt def-stmt if-stmt for-stmt
    expr expr? num-expr app-expr ident-expr list-expr end-of-args-expr end-of-args-val is-end-of-args-val?
    param param? param-with-default is-num? is-break? break-val is-continue? continue-val is-bool? not-found-val is-not-found-val?
    val val? num-val proc-val none-val non-return list-val get-name-arg get-defualt-arg bool-val force-bool tank is-tank? 
    force-num force-proc force-list programmer-forbided-val is-programmer-forbided-val? program program? prog
    type type? int-type bool-type float-type list-type none-type optional-type optional-type? empty-type some-type)

(define-datatype param param?
    (param-with-default (name symbol?) (otype optional-type?) (default expr?)))

(define (get-name-arg e)
    (cases param e
        (param-with-default (name otype defualt) name)))

(define (get-defualt-arg e)
    (cases param e
        (param-with-default (name otype defualt) defualt)))

(define-datatype program program?
    (prog (lines (listof stmt?)) (type-check-enable boolean?)))

(define-datatype stmt stmt?
    (assign-stmt (name symbol?) (otype optional-type?) (value expr?))
    (side-effect-stmt (e expr?))
    (return-value-stmt (e expr?))
    (return-novalue-stmt)
    (global-stmt (e symbol?))
    (pass-stmt)
    (continue-stmt)
    (break-stmt)
    (def-stmt (name symbol?) (params (listof param?)) (otype optional-type?) (statements (listof stmt?)))
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

(define-datatype optional-type optional-type?
    (empty-type)
    (some-type (a-type type?)))

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
