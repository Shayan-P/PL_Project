#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(require "./datatypes.rkt")

(provide str-to-sexp)

(define simple-math-lexer
  (lexer
   ((:or (:+ (char-range #\0 #\9))
         (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
    (token-NUM (string->number lexeme)))
   ("+" (token-sg-plus))
   ("-" (token-sg-minus))
   ("*" (token-sg-mul))
   ("/" (token-sg-div))
   ("**" (token-sg-pow))
   ("=" (token-sg-eq))
   ("==" (token-sg-eqeq))
   ("<" (token-sg-lt))
   (">" (token-sg-gt))
   (";" (token-sg-semi))
   (":" (token-sg-colon))
   ("," (token-sg-comma))
   ("(" (token-sg-popen))
   (")" (token-sg-pclose))
   ("[" (token-sg-sbopen))
   ("]" (token-sg-sbclose))
   ("def" (token-kw-def))
   ("pass" (token-kw-pass))
   ("continue" (token-kw-cont))
   ("break" (token-kw-break))
   ("return" (token-kw-ret))
   ("global" (token-kw-global))
   ("if" (token-kw-if))
   ("else" (token-kw-else))
   ("in" (token-kw-in))
   ("for" (token-kw-for))
   ("and" (token-kw-and))
   ("or" (token-kw-or))
   ("not" (token-kw-not))
   ("true" (token-kw-true))
   ("false" (token-kw-false))
   ("none" (token-kw-none))
   ((:or (:+ (char-range #\a #\z))
         (:: (:+ (char-range #\a #\z))))
    (token-IDENT (string->symbol lexeme)))
   (whitespace (simple-math-lexer input-port))
   ((eof) (token-EOF))))

(define-tokens a (NUM IDENT))
(define-empty-tokens b (EOF sg-plus sg-minus sg-mul sg-div sg-pow sg-eq sg-eqeq sg-lt sg-gt sg-semi sg-colon sg-comma sg-popen sg-pclose sg-sbopen sg-sbclose
                            kw-def kw-pass kw-cont kw-break kw-ret kw-global kw-if kw-else kw-in kw-for kw-and kw-or kw-not kw-true kw-false kw-none))

(define (pf0 name) (ident-expr name))
(define (pf1 name a1) (app-expr (pf0 name) a1))
(define (pf2 name a1 a2) (app-expr (pf1 name a1) a2))
(define (pfn name L)
  (if (null? L)
      name
      (app-expr (pfn name (cdr L)) (car L))))

(define simple-math-parser
  (parser
   (start statements)
   (end EOF)
   (error void)
   (tokens a b)
   (grammar
    (statements ((statement sg-semi) (list $1))
                ((statements statement sg-semi) (append $1 (list $2))))
    (statement ((compound_stmt) $1)
               ((simple_stmt) $1))
    (simple_stmt ((assignment) $1)
                 ((global_stmt) $1)
                 ((return_stmt) $1)
                 ((expression) (side-effect-stmt $1))
                 ((kw-pass) (pass-stmt))
                 ((kw-break) (continue-stmt))
                 ((kw-cont) (break-stmt)))
    (compound_stmt ((function_def) $1)
                   ((if_stmt) $1)
                   ((for_stmt) $1))
    (assignment ((IDENT sg-eq expression) (assign-stmt $1 $3)))
    (return_stmt ((kw-ret) (return-novalue-stmt))
                 ((kw-ret expression) (return-value-stmt $2)))
    (global_stmt ((kw-global IDENT) (global-stmt $2)))
    (function_def ((kw-def IDENT sg-popen params sg-pclose sg-colon statements) (def-stmt $2 $4 $7))
                  ((kw-def IDENT sg-popen sg-pclose sg-colon statements) (def-stmt $2 (list) $6)))
    (params ((param_with_default) (list $1))
            ((params sg-comma param_with_default) (append $1 (list $3))))
    (param_with_default ((IDENT sg-eq expression) (param-with-default $1 $3)))
    (if_stmt ((kw-if expression sg-colon statements else_block) (if-stmt $2 $4 $5)))
    (else_block ((kw-else sg-colon statements) $3))
    (for_stmt ((kw-for IDENT kw-in expression sg-colon statements) (for-stmt $2 $4 $6)))
    (expression ((disjunction) $1))
    (disjunction ((conjunction) $1)
                 ((disjunction kw-or conjunction) (pf2 '$or $1 $3)))
    (conjunction ((inversion) $1)
                 ((conjunction kw-and inversion) (pf2 '$and $1 $3)))
    (inversion ((kw-not inversion) (pf1 '$not $2))
               ((comparison) $1))
    (comparison ((eq_sum) $1)
                ((lt_sum) $1)
                ((gt_sum) $1)
                ((sum) $1))
    (eq_sum ((sum sg-eqeq sum) (pf2 '$eq? $1 $3)))
    (lt_sum ((sum sg-lt sum) (pf2 '$lt? $1 $3)))
    (gt_sum ((sum sg-gt sum) (pf2 '$gt? $1 $3)))
    (sum ((sum sg-plus term) (pf2 '$plus $1 $3))
         ((sum sg-minus term) (pf2 '$minus $1 $3))
         ((term) $1))
    (term ((term sg-mul factor) (pf2 '$mul $1 $3))
         ((term sg-div factor) (pf2 '$div $1 $3))
         ((factor) $1))
    (factor ((sg-plus power) $2)
         ((sg-minus power) (pf2 '$mul $2 (num-expr -1)))
         ((power) $1))
    (power ((patom sg-pow factor) (pf2 '$pow $1 $3))
           ((primary) $1))
    (primary ((patom) $1)
             ((primary sg-sbopen expression sg-sbclose) (pf2 '$index $1 $3))
             ((primary sg-popen sg-pclose) (pfn $1 (list (ident-expr '$dummy))))
             ((primary sg-popen arguments sg-pclose) (pfn $1 (reverse $3))))
    (arguments ((expression) (list $1))
               ((arguments sg-comma expression) (append $1 (list $3))))
    (patom ((IDENT) (ident-expr $1))
          ((kw-true) (ident-expr '$t))
          ((kw-false) (ident-expr '$f))
          ((kw-none) (ident-expr '$none))
          ((NUM) (num-expr $1))
          ((plist) $1))
    (plist ((sg-sbopen expressions sg-sbclose) $2)
           ((sg-sbopen sg-sbclose) (list)))
    (expressions ((expressions sg-comma expression) (append $1 (list $3)))
                 ((expression) (list $1))))))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "1+2+ 3 +   4")))

(define (str-to-sexp x) (simple-math-parser (lex-this simple-math-lexer (open-input-string x))))
;; (str-to-sexp "
;;     def f(x=0, y=1):
;;     return x
;;      ;;
;;    a = f(2, 3);
;;    a = f(a);
;;    b = 3 or a * 3 ** -3 and a;
;; ")

;; (str-to-sexp "
;;     a = 2;
;;     b = 3;
;;     c = a + b;
;;     print(c);
;; ")
