#lang racket

(require brag/support)
(require "language.rkt")

(provide parse tokenize token-list)

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
      (token 'ARRAYRDIV lexeme)]
     [".\\"
      (token 'ARRAYLDIV lexeme)]
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
     [(:: (union (string "int") (string "bool") (string "string")) whitespace) ; must have space after type
      (token 'BASIC_TYPE lexeme)]
     [(:: (string "list") whitespace)
      (token 'LIST_TYPE lexeme)]
     [(:: (string "matrix") whitespace)
      (token 'MATRIX_TYPE lexeme)]
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
