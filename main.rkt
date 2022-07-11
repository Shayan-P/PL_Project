#lang racket

(require "./parser.rkt")
(require "./evaluator.rkt")

(define (main) '())
(define debug (lambda (s) (let [(t1 (pretty-print `************))(t2 (pretty-print s))(t3 (pretty-print `*********))] s)))


(let [(parser-res (str-to-sexp "
    def fib(a=3):
        a=a+1;
        if  a<7 and not a>8:
            print(a);
        else:
            print(3);
            ;
        ;
    fib();
    fib();
    fib();
    fib();
"))
(env (box prelude-env))] (let
[ (ignore (eval-stmts env env (debug parser-res)))] 'finished))

;(let ((parser-res (str-to-sexp "
;    def f():
;        global a;
;        a = a + 1;
;        ;
;    a = 2;
;    print(a);
;    b = f();
;    print(a);
;"))) parser-res)
