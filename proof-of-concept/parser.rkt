#lang racket

(require brag/support)
(require "language.rkt")

(define (tokenize ip)
  (port-count-lines! ip)
  (define typed-octave-lexer
    (lexer-src-pos
     ["function"
      (token 'FUNCTION lexeme)]
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
     ["end"
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
     [(char-set "=()[]:,+-~*/\\^<>&|;")
      (token lexeme)]
     ["'"
      (token 'TRANSPOSE lexeme)]
     [numeric
      (token 'CONSTANT lexeme)]
     [(:: "\"" (repetition 0 +inf.0 (intersection (complement "\"") any-char)) "\"")
      (token 'STRING_LITERAL lexeme)]
     [whitespace
      (token 'WHITESPACE lexeme #:skip? #t)]
     [(repetition 0 +inf.0 (intersection (complement whitespace) (complement numeric) any-char))
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
(define test1-parsed (show-syntax (tokenize (test1))))

(define (test2) (open-input-string (file->string "examples/hello_world.m")))
(define test2-tokens (token-list (tokenize (test2))))
(define test2-parsed (show-syntax (tokenize (test2))))
