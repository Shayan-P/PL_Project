#lang racket

(provide type-check-stmts)

(require (lib "eopl.ss" "eopl"))
(require "./datatypes.rkt")


(define debug (lambda (s) (let [
   (t1 (pretty-print `************))(t2 (pretty-print s))(t3 (pretty-print `*********))
    ] s)))

(define trace-path (box '())) ; stores the context for debugging

(define (panic message)
    (begin
        (display (~a "Type Error: " message))
        (newline)
        (display "inner trace: ")
        (newline)
        (map (lambda (t) 
                (begin (newline) (pretty-print t) (newline))) 
            (unbox trace-path))
        (error "------------------------")))

(define (pop-trace)
    (set-box! trace-path (cdr (unbox trace-path))))

(define (push-trace obj)
    (set-box! trace-path (cons obj (unbox trace-path))))


(define-datatype type-environment type-environment?
    (empty-tenv)
    (extended-tenv (prev-tenv type-environment?) (key symbol?) (key-type complex-type?)))

(define (lookup-tenv tenv search-key)
    (cases type-environment tenv
        (empty-tenv () (panic (~a "free-variable: " search-key)))
        (extended-tenv (prev-tenv key key-type) 
            (if (equal? key search-key) key-type (lookup-tenv prev-tenv search-key)))))

(define (is-in-tenv? tenv search-key)
    (cases type-environment tenv
        (empty-tenv () #f)
        (extended-tenv (prev-tenv key key-type) 
            (if (equal? key search-key) #t (is-in-tenv? prev-tenv search-key)))))


(define (extend-tenv-list tenv pairs)
    (if (null? pairs) tenv 
        (let [(first (car pairs)) (rest (cdr pairs))] 
            (extend-tenv-list (extended-tenv tenv (car first) (cadr first)) rest))))

(define (numeric-type) (or-type (primitive-type (int-type)) (primitive-type (float-type))))

(define (fnt in-types ret-type) ; function type with dummy at the end
    (foldr
        (lambda (cur-type cur-fun) (function-type cur-type cur-fun))
                                    (function-type (dummy-type) ret-type) in-types))

(define (n2t t1 rett) (nnt (list t1) rett))
(define (n3t t1 t2 rett) (nnt (list t1 t2) rett))
(define (nnt in-types ret-type) ; function type without dummy at the end
    (foldr
        (lambda (cur-type cur-fun) (function-type cur-type cur-fun))
                                    ret-type in-types))


(define (merge-tenv low-prior high-prior)
    (cases type-environment high-prior
        (empty-tenv () low-prior)
        (extended-tenv (tenv name tp) (extended-tenv (merge-tenv low-prior tenv) name tp))))

(define prelude-tenv (extend-tenv-list (empty-tenv)
    (list
        (list 'print  (fnt (list (unknown-type)) (primitive-type (none-type)))) ; special case print? has end-of-args
        (list '$not   (n2t (primitive-type (bool-type)) (primitive-type (bool-type))))
        (list '$mul   (n3t (numeric-type) (numeric-type) (numeric-type)))
        (list '$plus  (or-type (n3t (numeric-type) (numeric-type) (numeric-type)) (n3t (primitive-type (list-type)) (primitive-type (list-type)) (primitive-type (list-type)))))
        (list `$pow   (n3t (numeric-type) (numeric-type) (numeric-type)))
        (list `$minus (n3t (numeric-type) (numeric-type) (numeric-type)))
        (list `$div   (n3t (numeric-type) (numeric-type) (numeric-type)))
        (list `$dummy (n3t (numeric-type) (numeric-type) (numeric-type)))
        (list `$eq?   (n3t (numeric-type) (numeric-type) (primitive-type (bool-type))))
        (list `$lt?   (n3t (numeric-type) (numeric-type) (primitive-type (bool-type))))
        (list `$gt?   (n3t (numeric-type) (numeric-type) (primitive-type (bool-type))))
        (list `$or    (n3t (primitive-type (bool-type)) (primitive-type (bool-type)) (primitive-type (bool-type))))
        (list `$and   (n3t (primitive-type (bool-type)) (primitive-type (bool-type)) (primitive-type (bool-type))))
        (list `$t     (primitive-type (bool-type)))
        (list `$f     (primitive-type (bool-type)))
        (list `$none  (primitive-type (none-type)))
        (list `$index (n3t (primitive-type (list-type)) (primitive-type (int-type)) (unknown-type))))))


(define (type-check-stmts lines)
    (let [(prog-return-type  (type-check-stmts-global (box prelude-tenv) lines))]
        (if (equal? prog-return-type (statement-return-type))
            (display "type-checked!\n")
            (panic 'return-in-global-scope))))

; (define (type-check-stmts lines) (display "ok"))

(define (merge-return-types A B)
    (if (equal? A (statement-return-type)) B
    (if (equal? B (statement-return-type)) A
        (error-if-not-type "wrong return type" (merge-types A B)))))

(define (type-check-stmts-global global-tenv-box lines)
    (foldl 
        (lambda (line ret-type) (merge-return-types ret-type (type-check-stmt-update-tenv global-tenv-box global-tenv-box line)))
                                    (statement-return-type) lines))

(define (type-check-stmts-non-global global-tenv-box tenv-box lines)
    (foldl 
        (lambda (line ret-type) (merge-return-types ret-type (type-check-stmt-update-tenv global-tenv-box tenv-box line)))
                                    (statement-return-type) lines))


; view statements as (begin () () ()) expression! Then return would jump. you should return the merged of all return types...
(define (type-check-stmt-update-tenv global-tenv-box tenv-box s)
        (let [
            (dummy1 (push-trace s))
            (my-ans
                (let [(tenv (unbox tenv-box)) (global-tenv (unbox global-tenv-box))]
                    (cases stmt s
                        (assign-stmt (name a-type exp)
                            (begin
                                (set-box! tenv-box (updated-tenv-if-consistent tenv name (error-if-not-type "wrong assignment type" (merge-types a-type (type-of-expression tenv exp)))))
                                (statement-return-type)))
                        (side-effect-stmt (exp)
                            (let [(my-expr-type (type-of-expression tenv exp))]
                                (statement-return-type)))
                        (return-value-stmt (exp)
                            (let [(my-expr-type (type-of-expression tenv exp))]
                                my-expr-type))
                        (return-novalue-stmt () (primitive-type (none-type)))
                        (global-stmt (name)
                            (begin
                                (set-box! tenv-box (extended-tenv tenv name 
                                    (if (is-in-tenv? global-tenv name) (lookup-tenv global-tenv name) (unknown-type))))
                                (statement-return-type)))
                        (pass-stmt () (statement-return-type))
                        (continue-stmt () (statement-return-type))
                        (break-stmt () (statement-return-type))
                        (def-stmt (name params ret-type body)
                            (let [(inside-tenv-box (box tenv))]
                                (let [(type-array
                                        (map (lambda (param) 
                                                (begin
                                                    (set-box! inside-tenv-box (extended-tenv (unbox inside-tenv-box) (get-name-arg param) (param->type tenv param))) ; change tenv to prelude-tenv to make functions have no access to outside
                                                    (param->type tenv param)))
                                            params))]
                                    (let [(my-def-type (fnt type-array ret-type)) (inside-tenv (unbox inside-tenv-box))]
                                        (begin
                                                (error-if-not-type "actual returned type different from specified" (merge-types ret-type
                                                    (let [(ret-type
                                                        (type-check-stmts-non-global (box global-tenv) (box (extended-tenv inside-tenv name my-def-type)) body))]
                                                        (if (equal? ret-type (statement-return-type)) (primitive-type (none-type)) ret-type))))
                                                (set-box! tenv-box (updated-tenv-if-consistent tenv name my-def-type))
                                                (statement-return-type))))))
                        (if-stmt (condition then-body else-body)
                            (let [(condition-expr-type (type-of-expression tenv condition))]
                                (begin
                                    (error-if-not-type "if condition not boolean" (merge-types (primitive-type (bool-type)) condition-expr-type))
                                    (merge-return-types 
                                        (type-check-stmts-non-global (box global-tenv) (box tenv) then-body)
                                        (type-check-stmts-non-global (box global-tenv) (box tenv) else-body)))))
                        (for-stmt (counter-name count-set body)
                            (let [(count-set-type (type-of-expression tenv count-set))]
                                (begin
                                    (error-if-not-type "for counter-set not list" (merge-types (primitive-type (list-type)) count-set-type))
                                    (type-check-stmts-non-global (box global-tenv) (box tenv) body)))))))
            (dummy2 (pop-trace))]
        my-ans))


(define (unknown-type? a-type)
    (cases complex-type a-type
        (unknown-type () #t)
        (else #f)))


(define (param->type tenv a-param)
    (cases param a-param
        (param-with-default (name a-type default)
            (error-if-not-type "parameter default type and specified type inconsistant" (merge-types a-type (type-of-expression tenv default))))))


(define (merge-types A B)
    (let [(merge-by-or (lambda (X Y) (if (equal? X 'NONE) Y (if (equal? Y 'NONE) X (or-type X Y)))))]
        (if (equal? A B) A
        (if (equal? A (unknown-type)) B
        (if (equal? B (unknown-type)) A
        (cases complex-type A
            (or-type (O1 O2) (merge-by-or (merge-types O1 B) (merge-types O2 B)))
            (else 
                (cases complex-type B
                    (or-type (O1 O2) (merge-by-or (merge-types A O1) (merge-types A O2)))
                    (function-type (B1 B2)
                        (cases complex-type A
                            (function-type (A1 A2) (function-type (merge-types A1 B1) (merge-types A2 B2)))
                            (else 'NONE)))
                    (else 'NONE)))))))))


(define (merge-type-func-end-of-args A)
    (let [(merge-by-or (lambda (X Y) (if (equal? X 'NONE) Y (if (equal? Y 'NONE) X (or-type X Y)))))]
        (cases complex-type A
            (or-type (O1 O2) (merge-by-or (merge-type-func-end-of-args O1) (merge-type-func-end-of-args O2)))
            (function-type (F1 F2) F2)
            (unknown-type () (unknown-type))
            (else 'NONE))))


(define (merge-type-app-exp F A)
    (let [(merge-by-or (lambda (X Y) (if (equal? X 'NONE) Y (if (equal? Y 'NONE) X (or-type X Y)))))]
        (cases complex-type F
            (or-type (O1 O2) (merge-by-or (merge-type-app-exp O1 A) (merge-type-app-exp O2 A)))
            (function-type (F1 F2) (if (equal? 'NONE (merge-types A F1)) 'NONE F2))
            (unknown-type () (unknown-type))
            (else 'NONE))))


(define (error-if-not-type message a-type)
    (if (complex-type? a-type) a-type (panic message)))


(define (type-of-expression tenv exp)
    (let [
        (dummy1 (push-trace exp))
        (my-ans
            (cases expr exp
                (end-of-args-expr () (end-of-args-type))
                (num-expr (num) (primitive-type (if (integer? num) (int-type) (float-type))))
                (ident-expr (name) (lookup-tenv tenv name))
                (list-expr (exprs) (begin (map (lambda (exp) (type-of-expression tenv exp)) exprs) (primitive-type (list-type))))
                (app-expr (rator rand)
                    (let [(trator (type-of-expression tenv rator))
                            (trand (type-of-expression tenv rand))]
                            (if (end-of-args-type? trand) (error-if-not-type "type error in function call" (merge-type-func-end-of-args trator)) (error-if-not-type "type error in function call" (merge-type-app-exp trator trand)))))))
        (dummy2 (pop-trace))]
    my-ans))


(define (updated-tenv-if-consistent tenv name a-type)
    (extended-tenv tenv name
        (if (is-in-tenv? tenv name) 
            (error-if-not-type "multiple definition of variables with inconsistant types" (merge-types a-type (lookup-tenv tenv name)))
            a-type)))
