#lang racket

(require "./parser.rkt")
(require "./evaluator.rkt")

(define (main) '())
(define debug (lambda (s) (let [(t1 (pretty-print `************))(t2 (pretty-print s))(t3 (pretty-print `*********))] s)))


(let [(parser-res (str-to-sexp "
    def fib(a=10):
        if a==0:
            return 1;
        else:
            if a==1:
                return 1;
            else:
                return fib(a-1)+fib(a-2);
                ;
            ;
        ;
    print(fib());
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
