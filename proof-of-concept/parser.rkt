#lang racket

(require brag/support)
(require "language.rkt")
(require "ast.rkt")
(require "ast-builder.rkt")
(require syntax/parse)

(define (tokenize ip)
  (port-count-lines! ip)
  (define typed-octave-lexer
    (lexer-src-pos
     ["function"
      (token 'FUNCTION lexeme)]
     ["endfunction"
      (token 'ENDFUNCTION lexeme)]
     [".*"
      (token 'ARRAYMUL lexeme)]
     ["./"
      (token 'ARRAYDIV lexeme)]
     [".\\"
      (token 'ARRAYRDIV lexeme)]
     [".^"
      (token 'ARRAYPOW lexeme)]
     ["global"
      (token 'GLOBAL lexeme)]
     ["clear"
      (token 'CLEAR lexeme)]
     ["if"
      (token 'IF lexeme)]
     ["endif"
      (token 'END lexeme)]
     ["else"
      (token 'ELSE lexeme)]
     ["elseif"
      (token 'ELSEIF lexeme)]
     ["while"
      (token 'WHILE lexeme)]
     ["for"
      (token 'FOR lexeme)]
     ["break"
      (token 'BREAK lexeme)]
     ["return"
      (token 'RETURN lexeme)]
     [(char-set "\n\r")
      (token 'CR lexeme)]
     ["!="
      (token 'NE_OP lexeme)]
     ["=="
      (token 'EQ_OP lexeme)]
     [">="
      (token 'GE_OP lexeme)]
     ["<="
      (token 'LE_OP lexeme)]
     [".'"
      (token 'NCTRANSPOSE lexeme)]
     ["false"
      (token 'BOOLEAN lexeme)]
     ["true"
      (token 'BOOLEAN lexeme)]
     [(:: ":" (repetition 0 +inf.0 whitespace) (repetition 1 +inf.0 (intersection (complement whitespace) (complement (char-set"=()[]:,+-~*/\\^<>&|;")) any-char)))
      (token 'TYPE lexeme)]
     [(char-set "=()[]:,+-~*/\\^<>&|;")
      (token lexeme lexeme)]
     ["'"
      (token 'TRANSPOSE lexeme)]
     [numeric
      (token 'CONSTANT lexeme)]
     [(:: "\"" (repetition 0 +inf.0 (intersection (complement "\"") any-char)) "\"")
      (token 'STRING_LITERAL lexeme)]
     [whitespace
      (token 'WHITESPACE lexeme #:skip? #t)]
     [(:: "##" (repetition 0 +inf.0 (intersection (complement "\n") any-char)))
      (token 'COMMENT lexeme #:skip? #t)]
     [(repetition 0 +inf.0 (intersection (complement whitespace) (complement (char-set"=()[]:,+-~*/\\^<>&|;")) any-char))
      (token 'IDENTIFIER lexeme)]
     [(eof)
      (void)]))
  (define (next-token) (typed-octave-lexer ip))
  next-token)

(define (token-list tokenize-generator)
  (local [(define (helper tokenize-generator rsf)
            (local [(define f (position-token-token (tokenize-generator)))]
              (cond [(void? f) rsf]
                    [else (helper tokenize-generator (append rsf (list f)))])))]
(helper tokenize-generator empty)))

(define (show-syntax token-list)
  (syntax->datum (parse token-list)))

(define (test1) (open-input-string "a = 3;"))
(define test1-tokens (token-list (tokenize (test1))))
(define test1-parsed (parse (tokenize (test1))))
(define test1-ast (build-ast test1-parsed))

(define (test2) (open-input-string (file->string "examples/hello_world.m")))
(define test2-tokens (token-list (tokenize (test2))))
(define test2-parsed (parse (tokenize (test2))))
(define test2-ast (build-ast test2-parsed))

(define (test3) (open-input-string (file->string "examples/circle3d-test.m")))
(define test3-tokens (token-list (tokenize (test3))))
(define test3-parsed (parse (tokenize (test3))))
(define test3-ast (build-ast test3-parsed))

;(define (test4) (open-input-string "function x = test(y, z)
;y = 1;
;z = 2;
;endfunction
;"))
;(define test4-tokens (token-list (tokenize (test4))))
;(define test4-parsed (parse (tokenize (test4))))

;; Interpretation ;;

; from here our interp takes the syntax objects (symbol) outputted by our parser

#| e.g.
'(drawing

  (rows (repeat 3) (chunk 9 "X") ";")

  (rows (repeat 6) (chunk 3 " ") (chunk 3 "X") (chunk 3 " ") ";")

  (rows (repeat 3) (chunk 9 "X") ";"))
|#

; use syntax-parse, in the syntax/parse library, where we provide it a set of patterns to parse
; and actions to perform when those patterns match
