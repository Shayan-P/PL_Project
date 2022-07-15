#lang racket

(require "./parser.rkt")
(require "./evaluator.rkt")

;;; (define (main) '())
; switch the comment for debugging
;(define debug (lambda (s) (let [(t1 (pretty-print `************))(t2 (pretty-print s))(t3 (pretty-print `*********))] s)))
(define debug (lambda (s) s))

(define (evaluate f) (let [(parser-res (str-to-sexp (file->string f)))
(env (box prelude-env))] (let
[ (ignore (eval-stmts env env (debug parser-res)))] 'finished)))

(evaluate (car (vector->list (current-command-line-arguments))))

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

;; (let [(parser-res (str-to-sexp "
;;     def sum(l=[], f=None):
;;         a = 0;
;;         for x in l:
;;             a = x + a;
;;             ;
;;         return a;
;;         ;

;;     print(sum([1, 2, 3]));
;;     print(sum([1, 10, 100]));
;;     print(sum([]));
;; "))
;; (env (box prelude-env))] (let
;; [ (ignore (eval-stmts env env (debug parser-res)))] 'finished))
