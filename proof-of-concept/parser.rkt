#lang racket

(require brag/support)
(require "language.rkt")
(require syntax/parse)

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

(define (interp octave-stx)
  (syntax-parse octave-stx
    [({~literal translation_unit} rows-stxs ...)

     ; we use ~literal to let syntax-parse know that yes should show up literally in the syntax object
     (for ([rows-stx (syntax->list #'(rows-stxs ...))])
            (interpret-rows rows-stx))]))

; interpret-rows
(define (interpret-rows rows-stx)
    (syntax-parse rows-stx
      [({~literal statement_list}
        ({~literal statement} repeat-number)
        chunks ... ";")

       ; we extract out the repeat-number out of the syntax object and use it as the range of the for
       ; loop. The inner loop walks across each chunk-stx and calls interpret-chunk on it.
       (for ([i (syntax-e #'repeat-number)])
            (for ([chunk-stx (syntax->list #'(chunks ...))])
                 (interpret-chunk chunk-stx))
            (newline))]))

; interpret-chunk
(define (interpret-chunk chunk-stx)
    (syntax-parse chunk-stx
      ; extract out the chunk-size and chunk-string portions, and print to standard output
      [({~literal chunk} chunk-size chunk-string)
  
       (for ([k (syntax-e #'chunk-size)])
            (display (syntax-e #'chunk-string)))]))

; interp tests
(define test-interp-chunk (interpret-chunk #'(chunk 3 "X")))
(define test-interp (interp #'(translation_unit (statement_list (statement 5) (chunk 3 "X") ";"))))
; TODO: fix these tests
(define test-interp2 (interp test1-parsed))
(define test-interp3 (interp test2-parsed))