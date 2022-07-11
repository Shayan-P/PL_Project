#lang racket

(require (lib "eopl.ss" "eopl"))
(require "./datatypes.rkt")

(provide eval-stmts prelude-env benv-lookup )

(define debug (lambda (s) (let [(t1 (pretty-print `************))(t2 (our-pretty-print s))(t3 (pretty-print `*********))] s)))
(define pipe-none-val (lambda (s)  (none-val ) ))
(define (is-none? v)
    (cases val v
        (none-val () #t )
        (else #f )))


(define our-pretty-print (lambda (s)
 (if (procedure? s) (display s) (display s ))))

(define (eval-stmts env l)
    (if (null?  l) 'end (let [(ignore (eval-stmt env (car l)))] (eval-stmts env (cdr l)))))

(define (eval-stmts-with-return env l)
    (if (null?  l) (none-val) (let [(val (eval-stmt env (car l)))] (if (is-none? val) (eval-stmts-with-return env (cdr l)) val))))

(define (eval-stmt env s)
    (cases stmt s
        (side-effect-stmt (e) (pipe-none-val(eval-expr env e)))
        (assign-stmt (name val) (pipe-none-val(benv-extend! env name (eval-expr env val))))
        (def-stmt (name arg body) (pipe-none-val(benv-extend! env name (function-builder body))))
        (return-value-stmt (e) (eval-expr env e))
        (else (error 'TODO))))

(define (function-builder body) 
    (proc-val (lambda (env dummy) (let [] (eval-stmts-with-return env body)))))

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
        (list '$mul (pnf2 *))
        (list '$plus (pnf2 +))
        (list `$pow (pnf2 expt))
        (list `$minus (pnf2 -))
        (list `$div (pnf2 /))
        (list `$dummy (pnf2 *)))))

(define (eval-expr env e)
    (cases expr e
        (num-expr (v) (num-val v))
        (ident-expr (v) (benv-lookup env v))
        (app-expr (rator rand) ((force-proc (eval-expr env rator)) env (eval-expr env rand)))))
