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
    (let [(r (cases stmt  s
        (side-effect-stmt (e) (non-return(eval-expr global-env env e)))
        (assign-stmt (name val) (non-return(benv-extend-rep! env name (eval-expr global-env env val))))
        (def-stmt (name arg body) (non-return(benv-extend-rep! env name (first-function-builder name body arg env))))
        (return-value-stmt (e) (eval-expr global-env env e))
        (if-stmt (cond true false) (if (force-bool (eval-expr global-env env cond)) (eval-stmts-with-return global-env env true) (eval-stmts-with-return global-env env false)))
        (for-stmt (counter-name list body) (for global-env env counter-name (force-list (eval-expr global-env env list)) body ))
        (break-stmt () (break-val))
        (continue-stmt () (continue-val))
        (pass-stmt () (programmer-forbided-val))
        (global-stmt (name) (non-return(benv-extend-direct! env  name (env-lookup-direct  global-env name))))
        (else (error 'TODO))))] (let [
          ; (x1 (pretty-print r)) (x2 (pretty-print s)) (x3 (pretty-print `^*^*^*^))
            ] r)))

(define (for global-env env counter-name list body)
    (if (null? list) (programmer-forbided-val) 
    (let [(ignore (benv-extend! env counter-name (car list)))] (let [(val (eval-stmts-with-return global-env env body))] (if (is-break? val) (programmer-forbided-val) (if (or (is-programmer-forbided-val? val) (is-continue? val))  (for global-env env counter-name (cdr list) body) val))))))


(define (add-rest-of-arg-and-eval body args local-env global-env build-env)
    (if (null? args) (eval-stmts-with-return global-env local-env body)
    (add-rest-of-arg-and-eval body (cdr args) (env-extend-inplace local-env (get-name-arg (car args)) (eval-expr build-env build-env (get-defualt-arg (car args)))) global-env build-env)))


(define (function-builder name body args local-env build-env) 
    (if (null?  args) (proc-val (lambda (global-env dummy) (eval-stmts-with-return global-env local-env body)) )
    (proc-val (lambda (global-env potential-arg) (if (is-end-of-args-val?  potential-arg) (add-rest-of-arg-and-eval body args local-env global-env build-env)
    (function-builder name body (cdr args) (env-extend-inplace local-env (get-name-arg (car args))  potential-arg) build-env))) )))

(define (first-function-builder name body args build-env)
    (if (null?  args) (proc-val (lambda (global-env dummy) (eval-stmts-with-return global-env (box (add-self global-env name)) body)) )
    (proc-val (lambda (global-env potential-arg) (if (is-end-of-args-val?  potential-arg) (add-rest-of-arg-and-eval body args (box (add-self global-env name)) global-env build-env)
    (function-builder name body (cdr args) (env-extend-inplace (box (add-self global-env name)) (get-name-arg (car args))  potential-arg) build-env))) )))



(define (env-extend-inplace env name val) (let [(ignore (benv-extend! env name val))] env))
(define (env-lookup env v) (unbox (env v)))
(define (env-lookup-direct env v) ((unbox env) v))

(define (env-extend env name val) (lambda (n) (if (eq? name n) val (env n))))
(define (env-exlist env l) (foldl (lambda (p e) (env-extend e (car p) (box (cadr p)))) env l))

(define (benv-lookup benv v) (env-lookup (unbox benv) v))
(define (benv-extend! benv name val) (set-box! benv (env-extend (unbox benv) name (box val))))
(define (benv-extend-rep! benv name val) (if (is-not-found-val? (benv-lookup benv name)) (set-box! benv (env-extend (unbox benv) name (box val))) (set-box! ((unbox benv) name) val)))
(define (benv-extend-direct! benv name box) (set-box! benv (env-extend (unbox benv) name box)))
(define (merge global-env env) (box (lambda (n) (let [(val ((unbox env) n) )] (if (is-not-found-val? (unbox val)) ((unbox global-env) n) val)))))
(define (add-self global-env name) (env-extend prelude-env name (box (benv-lookup global-env name))))

(define (pnf2 f)
    (proc-val (lambda (e arg1)
        (proc-val (lambda (e arg2) (if (is-num? arg1) 
        (num-val (f (force-num arg1) (force-num arg2)))
        (list-val (f (force-list arg1) (force-list arg2)))
        )) ))))

(define (pnf2bool f)
    (proc-val (lambda (e arg1)
        (proc-val (lambda (e arg2)  
        (bool-val (f (force-bool arg1) (force-bool arg2)))) )) ))

(define (pnf2-num-to-bool f)
    (proc-val (lambda (e arg1)
        (proc-val (lambda (e arg2)  
        (bool-val (f (force-num arg1) (force-num arg2)))) )) ))

(define end-of-args-expr-eater (proc-val (lambda (e arg) none-val)))
(define (empty-env name) (box(not-found-val name)))
(define prelude-env (env-exlist empty-env
    (list
        (list 'print (proc-val (lambda (e arg) (let ([ignore (pretty-print arg)]) end-of-args-expr-eater)) ))
        (list '$not (proc-val (lambda (e arg) (bool-val (not (force-bool arg)))) ))
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

(define (eval-expr global-env env e)
    (cases expr e
        (num-expr (v) (num-val v))
        (ident-expr (v) (benv-lookup env v))
        (app-expr (rator rand) (let [(solved-rator  (eval-expr global-env env rator))
                                        (solved-rand (eval-expr global-env env rand))
                                        (merged-env (merge global-env env))]
                                        ((force-proc solved-rator) merged-env solved-rand)
                                        ))
        (end-of-args-expr () (end-of-args-val))
        (list-expr (v) (list-val (map (lambda (e) (eval-expr global-env env e)) v)))
        ))
