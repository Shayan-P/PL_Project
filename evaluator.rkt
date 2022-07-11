#lang racket

(require (lib "eopl.ss" "eopl"))
(require "./datatypes.rkt")

(provide eval-stmts prelude-env benv-lookup )

(define debug (lambda (s) (let [(t1 (pretty-print `************))(t2 (our-pretty-print s))(t3 (pretty-print `*********))] s)))


(define our-pretty-print (lambda (s)
 (if (procedure? s) (display s) (display s ))))

(define (eval-stmts global-env env l)
    (eval-stmts-with-return global-env env l))

(define (eval-stmts-with-return global-env env l)
    (if (null?  l) (programmer-forbided-val) (let [(val (eval-stmt global-env env (car l)))] (if (is-programmer-forbided-val? val) (eval-stmts-with-return global-env env (cdr l)) val))))

(define (eval-stmt global-env env s)
    (let [(r (cases stmt s
        (side-effect-stmt (e) (non-return(eval-expr env e)))
        (assign-stmt (name val) (non-return(benv-extend! env name (eval-expr env val))))
        (def-stmt (name arg body) (non-return(benv-extend! env name (first-function-builder name body arg env))))
        (return-value-stmt (e) (eval-expr env e))
        (if-stmt (cond true false) (if (force-bool (eval-expr env cond)) (eval-stmts-with-return global-env env true) (eval-stmts-with-return global-env env false)))
        (for-stmt (counter-name list body) (for global-env env counter-name (force-list (eval-expr env list)) body ))
        (break-stmt () (break-val))
        (continue-stmt () (continue-val))
        (pass-stmt () (programmer-forbided-val))
        (else (error 'TODO))))] (let [
        ;    (x1 (pretty-print r)) (x2 (pretty-print s)) (x3 (pretty-print `^*^*^*^))
            ] r)))

(define (for global-env env counter-name list body)
    (if (null? list) (programmer-forbided-val) 
    (let [(ignore (benv-extend! env counter-name (car list)))] (let [(val (eval-stmts-with-return global-env env body))] (if (is-break? val) (programmer-forbided-val) (if (or (is-programmer-forbided-val? val) (is-continue? val))  (for global-env env counter-name (cdr list) body) val))))))



(define (add-rest-of-arg-and-eval body args local-env global-env build-env)
    (if (null? args) (eval-stmts-with-return global-env local-env body)
    (add-rest-of-arg-and-eval body (cdr args) (env-extend-inplace local-env (get-name-arg (car args)) (eval-expr build-env (get-defualt-arg (car args)))) global-env build-env)))


(define (function-builder body args local-env build-env) 
    (if (null?  args) (proc-val (lambda (global-env dummy) (eval-stmts-with-return global-env local-env body)))
    (proc-val (lambda (global-env potential-arg) (if (is-end-of-args-val?  potential-arg) (add-rest-of-arg-and-eval body args local-env global-env build-env)
    (function-builder body (cdr args) (env-extend-inplace local-env (get-name-arg (car args))  potential-arg) build-env))))))

(define (first-function-builder name body args build-env)
    (if (null?  args) (proc-val (lambda (global-env dummy) (eval-stmts-with-return global-env (box prelude-env) body)))
    (proc-val (lambda (global-env potential-arg) (if (is-end-of-args-val?  potential-arg) (add-rest-of-arg-and-eval body args (box prelude-env) global-env build-env)
    (function-builder body (cdr args) (env-extend-inplace (box prelude-env) (get-name-arg (car args))  potential-arg) build-env))))))



(define (env-extend-inplace env name val) (let [(ignore (benv-extend! env name val))] env))
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

(define (pnf2bool f)
    (proc-val (lambda (e arg1)
        (proc-val (lambda (e arg2)  
        (bool-val (f (force-bool arg1) (force-bool arg2))))))))

(define (pnf2-num-to-bool f)
    (proc-val (lambda (e arg1)
        (proc-val (lambda (e arg2)  
        (bool-val (f (force-num arg1) (force-num arg2))))))))

(define end-of-args-expr-eater (proc-val (lambda (e arg) none-val)))
(define (empty-env name) (error 'name-not-found (symbol->string name)))
(define prelude-env (env-exlist empty-env
    (list
        (list 'print (proc-val (lambda (e arg) (let ([ignore (pretty-print arg)]) end-of-args-expr-eater))))
        (list '$not (proc-val (lambda (e arg) (bool-val (not (force-bool arg))))))
        (list '$mul (pnf2 *))
        (list '$plus (pnf2 (lambda (a b) (if (list? a) (append a b) (+ a b)))))
        (list `$pow (pnf2 expt))
        (list `$minus (pnf2 -))
        (list `$div (pnf2 /))
        (list `$dummy (pnf2 *))
        (list `$eq? (pnf2-num-to-bool (lambda (a b) (if (= a b) #t #f))))
        (list `$lt? (pnf2-num-to-bool (lambda (a b) (if (< a b) #t #f))))
        (list `$gt? (pnf2-num-to-bool (lambda (a b) (if (> a b) #t #f))))
        (list `$or (pnf2bool (lambda (a b) (or a b))))
        (list `$and (pnf2bool (lambda (a b) (and a b))))
        (list `$t (bool-val #t))
        (list `$f (bool-val #f))
        (list `$none (none-val))
        )))

(define (eval-expr env e)
    (cases expr e
        (num-expr (v) (num-val v))
        (ident-expr (v) (benv-lookup env v))
        (app-expr (rator rand) ((force-proc (eval-expr env rator)) env (eval-expr env rand)))
        (end-of-args-expr () (end-of-args-val))
        (list-expr (v) (list-val (map (lambda (e) (eval-expr env e)) v)))
        ))
