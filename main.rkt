#lang racket

(require "./parser.rkt")
(require "./evaluator.rkt")

(define (main) '())


(let [(parser-res (str-to-sexp "
    a = 2;
    b = 3;
    c = a + b;
    print(c);
"))
(env (box prelude-env))] (let
[(ignore (eval-stmts env parser-res))] 'finished))

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
