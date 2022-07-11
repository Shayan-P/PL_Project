#lang racket

(require "./parser.rkt")
(require "./evaluator.rkt")

(define (main) '())
(define debug (lambda (s) (let [(t1 (pretty-print `************))(t2 (pretty-print s))(t3 (pretty-print `*********))] s)))


(let [(parser-res (str-to-sexp "
    def t():
        print(5);
        ;
    print(t());
"))
(env (box prelude-env))] (let
[ (ignore (eval-stmts env (debug parser-res)))] 'finished))

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
