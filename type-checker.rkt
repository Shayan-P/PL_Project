#lang racket

(provide type-check-stms)

(require (lib "eopl.ss" "eopl"))
(require "./datatypes.rkt")

(define-datatype type-environment type-environment?
    (empty-tenv)
    (extended-tenv (prev-tenv type-environment?) (key symbol?) (key-type complex-type?)))

(define (lookup-tenv tenv search-key)
    (cases type-environment tenv
        (empty-tenv () (error 'free-variable))
        (extended-tenv (prev-tenv key key-type) 
            (if (equal? prev-tenv key search-key) key-type (lookup-tenv prev-tenv search-key)))))

(define (extend-tenv-list tenv pairs)
    (if (null? pairs) tenv 
        (let [(first (car pairs)) (rest (cdr pairs))] 
            (extend-tenv-list (extended-tenv tenv (car first) (cadr first)) rest))))

(define (numeric-type) (or-type (primitive-type (int-type)) (primitive-type (float-type))))
(define (f2t t1 t2) (function-type t1 t2))
(define (f3t t1 t2 t3) (f2t t1 (f2t t2 t3)))

(define prelude-tenv (extend-tenv-list (empty-tenv)
    (list
        (list 'print  (f2t (unknown-type) (primitive-type (none-type))))
        (list '$not   (f2t (primitive-type (bool-type)) (primitive-type (bool-type))))
        (list '$mul   (f3t (numeric-type) (numeric-type) (numeric-type)))
        (list '$plus  (or-type (f3t (numeric-type) (numeric-type) (numeric-type)) (f3t (primitive-type (list-type)) (primitive-type (list-type)) (primitive-type (list-type)))))
        (list `$pow   (f3t (numeric-type) (numeric-type) (numeric-type)))
        (list `$minus (f3t (numeric-type) (numeric-type) (numeric-type)))
        (list `$div   (f3t (numeric-type) (numeric-type) (numeric-type)))
        (list `$dummy (f3t (numeric-type) (numeric-type) (numeric-type)))
        (list `$eq?   (f3t (numeric-type) (numeric-type) (primitive-type (bool-type))))
        (list `$lt?   (f3t (numeric-type) (numeric-type) (primitive-type (bool-type))))
        (list `$gt?   (f3t (numeric-type) (numeric-type) (primitive-type (bool-type))))
        (list `$or    (f3t (primitive-type (bool-type)) (primitive-type (bool-type)) (primitive-type (bool-type))))
        (list `$and   (f3t (primitive-type (bool-type)) (primitive-type (bool-type)) (primitive-type (bool-type))))
        (list `$t     (primitive-type (bool-type)))
        (list `$f     (primitive-type (bool-type)))
        (list `$none  (primitive-type (none-type)))
        (list `$index (f3t (primitive-type (list-type)) (primitive-type (int-type)) (unknown-type))))))


(define (type-check-stms lines)
    (display "ok"))


; (define (type-check-stms lines)
;     (type-check-stats-global prelude-tenv lines))

; (define (unknown-type? a-type)
;     (cases complex-type a-type
;         (unknown-type () #t)
;         (else #f)))

; (define (param->type a-param)
;     (cases param a-param
;         (param-with-default (name a-type default) 
;             (let [(d-type (expr->type default))]
;                 (if (match-type d-type a-type) (error 'function-type-mismatch)
;                     (if (unknown-type? a-type) d-type a-type))))))


; (define (function-type-builder params ret-type)
;     (if (null? params) (function-type (dummy-type) (ret-type))
;         (let [(first (car params)) (rest (cdr params))]
;             (if (null? rest)
;                 (function-type (param->type first) (ret-type))
;                 (function-type (param->type first) (function-type-builder rest ret-type))))))


; (define (type-check-stmt global-tenv tenv s)
;     ; TODO
; )

; (define (type-check-assign-get-env global-tenv tenv update-tenv name a-type value expr)
;     ; CHECK IF name previously exists...
;                     (let [(new-env (extended-tenv global-tenv name a-type))]
; ))

; (define (type-check-def-get-env name a-type value expr)
;     ; CHECK IF name previously exists...
;                     (extended-tenv global-tenv name (function-type-builder params ret-type))

; )

; (define (type-of-expression tenv exp)

; )


; (define (type-check-stats-global global-tenv lines)
;     (if (null? lines) (void)
;         (let [(first (car lines)) (rest (cdr lines))]
;             (cases stmt first
;                 (assign-stmt (name a-type value expr)
;                     (let [(new-env (type-check-assign-get-env global-tenv prelude-tenv global-tenv name a-type value expr))]
;                         (type-check-stats-global new-env rest)))
;                 (def-stmt (name params ret-type body)
;                     (let [(new-env (type-check-def-get-env global-tenv prelude-tenv global-tenv name params ret-type body))]
;                         (type-check-stats-global new-env rest)))
;                 (else )   
;             )
;             (let [(val (type-check-stms global-tenv env (car l)))] (if (is-programmer-forbided-val? val) (eval-stmts-with-return global-env env (cdr l)) val))))



; (define (eval-stmt global-env env s)
;     (let [(r (cases stmt  s
;         (side-effect-stmt (e) (non-return(eval-expr global-env env e)))
;         (assign-stmt (name otype val) (non-return(benv-extend-replace! env name  (lazy-eval-expr global-env env val))))
;         (def-stmt (name arg otype body) (non-return(benv-extend-replace! env name (first-function-builder name body arg env))))
;         (return-value-stmt (e) (eval-expr global-env env e))
;         (if-stmt (cond true false) (if (force-bool (force-not-tank (eval-expr global-env env cond))) (eval-stmts-with-return global-env env true) (eval-stmts-with-return global-env env false)))
;         (for-stmt (counter-name list body) (for global-env env counter-name (force-list (force-not-tank (eval-expr global-env env list))) body ))
;         (break-stmt () (break-val))
;         (continue-stmt () (continue-val))
;         (pass-stmt () (programmer-forbided-val))
;         (global-stmt (name) (non-return(benv-extend-direct! env  name (benv-lookup-direct  global-env name))))
;         (else (error 'TODO))))] (let [
;          ;  (x1 (pretty-print r)) (x2 (pretty-print s)) (x4  (pretty-print env)) (x3 (pretty-print `^*^*^*^))
;             ] r)))
