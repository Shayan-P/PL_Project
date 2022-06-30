#lang racket

(require "./parser.rkt")
(require "./evaluator.rkt")

(define (main) '())


(let [(parser-res (str-to-sexp "
    print(1);
    a = 2;
    print(a * 3 + 2);
    print(3);
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
