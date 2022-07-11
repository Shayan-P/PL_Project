#lang racket

(require (lib "eopl.ss" "eopl"))
(require "./datatypes.rkt")

(provide eval-stmts prelude-env benv-lookup )

(define debug (lambda (s) (let [(t1 (pretty-print `************))(t2 (our-pretty-print s))(t3 (pretty-print `*********))] s)))


(define our-pretty-print (lambda (s)
 (if (procedure? s) (display s) (display s ))))

(define (eval-stmts env l)
    (eval-stmts-with-return env l))

(define (eval-stmts-with-return env l)
    (if (null?  l) (programmer-forbided-val) (let [(val (eval-stmt env (car l)))] (if (is-programmer-forbided-val? val) (eval-stmts-with-return env (cdr l)) val))))

(define (eval-stmt env s)
    (let [(r (cases stmt s
        (side-effect-stmt (e) (non-return(eval-expr env e)))
        (assign-stmt (name val) (non-return(benv-extend! env name (eval-expr env val))))
        (def-stmt (name arg body) (non-return(benv-extend! env name (function-builder body arg))))
        (return-value-stmt (e) (eval-expr env e))
        (if-stmt (cond true false) (if (= (force-num (eval-expr env cond)) 1) (eval-stmts-with-return env true) (eval-stmts-with-return env false)))
        (for-stmt (counter-name list body) (for env counter-name (force-list (eval-expr env list)) body ))
        (break-stmt () (break-val))
        (continue-stmt () (continue-val))
        (pass-stmt () (programmer-forbided-val))
        (else (error 'TODO))))] (let [
           ; (x1 (pretty-print r)) (x2 (pretty-print s)) (x3 (pretty-print `^*^*^*^))
            ] r)))

(define (for env counter-name list body)
    (if (null? list) (programmer-forbided-val) 
    (let [(ignore (benv-extend! env counter-name (car list)))] (let [(val (eval-stmts-with-return env body))] (if (is-break? val) (programmer-forbided-val) (if (or (is-programmer-forbided-val? val) (is-continue? val))  (for env counter-name (cdr list) body) val))))))
(define (function-builder body arg) 
    (proc-val (lambda (env dummy) (let [] (eval-stmts-with-return env body)))))

(define (env-lookup env v) (env v))
(define (env-extend env name val) (lambda (n) (if (eq? name n) val (env n))))
(define (env-exlist env l) (foldl (lambda (p e) (env-extend e (car p) (cadr p))) env l))

(define (benv-lookup benv v) (env-lookup (unbox benv) v))
(define (benv-extend! benv name val) (set-box! benv (env-extend (unbox benv) name val)))

(define (pnf2 f)
    (proc-val (lambda (e arg1)
        (proc-val (lambda (e arg2) (if (is-num? arg1) 
        (num-val (f (force-num arg1) (force-num arg2)))
        (list-val (f (force-list arg1) (force-list arg2)))
        ))))))

(define (empty-env name) (error 'name-not-found (symbol->string name)))
(define prelude-env (env-exlist empty-env
    (list
        (list 'print (proc-val (lambda (e arg) (let ([ignore (pretty-print arg)]) none-val))))
        (list '$mul (pnf2 *))
        (list '$plus (pnf2 (lambda (a b) (if (list? a) (append a b) (+ a b)))))
        (list `$pow (pnf2 expt))
        (list `$minus (pnf2 -))
        (list `$div (pnf2 /))
        (list `$dummy (pnf2 *))
        (list `$eq? (pnf2 (lambda (a b) (if (= a b) 1 0))))
        (list `$lt? (pnf2 (lambda (a b) (if (< a b) 1 0))))
        (list `$gt? (pnf2 (lambda (a b) (if (> a b) 1 0))))

        )))

(define (eval-expr env e)
    (cases expr e
        (num-expr (v) (num-val v))
        (ident-expr (v) (benv-lookup env v))
        (app-expr (rator rand) ((force-proc (eval-expr env rator)) env (eval-expr env rand)))
        (end-of-args-expr () (error 'broken))
        (list-expr (v) (list-val (map (lambda (e) (eval-expr env e)) v)))
        ))
