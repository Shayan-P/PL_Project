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
   ("=" (token-sg-eq))
   (";" (token-sg-semi))
   ("(" (token-sg-popen))
   (")" (token-sg-pclose))
   ("def" (token-kw-def))
   ((:or (:+ (char-range #\a #\z))
         (:: (:+ (char-range #\a #\z)) #\. (:+ (char-range #\a #\z))))
    (token-IDENT (string->symbol lexeme)))
   (whitespace (simple-math-lexer input-port))
   ((eof) (token-EOF))))

(define-tokens a (NUM IDENT))
(define-empty-tokens b (EOF sg-plus sg-semi sg-eq sg-popen sg-pclose kw-def))

(define (pf2 name a1 a2) (app-expr (app-expr (ident-expr name) a1) a2))

(define simple-math-parser
  (parser
   (start statements)
   (end EOF)
   (error void)
   (tokens a b)
   (grammar
    (statements ((statement sg-semi) (list $1))
                ((statements statement sg-semi) (append $1 (list $2))))
    (statement ((assignment) $1)
               ((expression) (side-effect-stmt $1)))
    (assignment ((IDENT sg-eq expression) (assign-stmt $1 $3)))
    (expression ((expression sg-plus atom) (pf2 '$plus $1 $3))
                ((expression sg-popen expression sg-pclose) (app-expr $1 $3))
                ((atom) $1))
    (atom ((NUM) (num-expr $1))
          ((IDENT) (ident-expr $1)))
    )))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "1+2+ 3 +   4")))

(define (str-to-sexp x) (simple-math-parser (lex-this simple-math-lexer (open-input-string x))))
