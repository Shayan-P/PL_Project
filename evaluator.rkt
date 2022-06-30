#lang racket

(require (lib "eopl.ss" "eopl"))
(require "./datatypes.rkt")

(provide eval-stmts prelude-env benv-lookup)

(define (eval-stmts env l)
    (if (null? l) 'end (let ([ignore (eval-stmt env (car l))]) (eval-stmts env (cdr l)))))
(define (eval-stmt env s)
    (cases stmt s
        (side-effect-stmt (e) (eval-expr env e))
        (assign-stmt (name val) (benv-extend! env name (eval-expr env val)))
        (else (error 'TODO))))

(define (env-lookup env v) (env v))
(define (env-extend env name val) (lambda (n) (if (eq? name n) val (env n))))
(define (env-exlist env l) (foldl (lambda (p e) (env-extend e (car p) (cadr p))) env l))

(define (benv-lookup benv v) (env-lookup (unbox benv) v))
(define (benv-extend! benv name val) (set-box! benv (env-extend (unbox benv) name val)))

(define (pnf2 f)
    (proc-val (lambda (e arg1)
        (proc-val (lambda (e arg2) (num-val (f (force-num arg1) (force-num arg2))))))))

(define (empty-env name) (error 'name-not-found (symbol->string name)))
(define prelude-env (env-exlist empty-env
    (list
        (list 'print (proc-val (lambda (e arg) (let ([ignore (pretty-print arg)]) none-val))))
        (list '$plus (pnf2 +)))))

(define (eval-expr env e)
    (cases expr e
        (num-expr (v) (num-val v))
        (ident-expr (v) (benv-lookup env v))
        (app-expr (rator rand) ((force-proc (eval-expr env rator)) env (eval-expr env rand)))))
