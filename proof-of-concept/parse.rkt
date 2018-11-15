#lang plai
(require "./ast.rkt")

;; Source: https://stackoverflow.com/a/12444091/3985212
;;
;;
;; index_expression
;;         : ':'
;;         | expression
;;         ;
;;
;; index_expression_list
;;         : index_expression
;;         | index_expression_list ',' index_expression
;;         ;
;;
;; array_expression
;;         : IDENTIFIER '(' index_expression_list ')'
;;         ;
;;
;; identifier_list
;;         : IDENTIFIER
;;         | identifier_list IDENTIFIER
;;         ;
;;
;; global_statement
;;         : GLOBAL identifier_list eostmt
;;         ;
;;
;; clear_statement
;;         : CLEAR identifier_list eostmt
;;         ;
;;
;; assignment_statement
;;         : assignment_expression eostmt
;;         ;
;;
;; array_element
;;         : expression
;;         | expression_statement
;;         ;
;;
;; array_list
;;         : array_element
;;         | array_list array_element
;;         ;
;;
;; selection_statement
;;         : IF expression statement_list END eostmt
;;         | IF expression statement_list ELSE statement_list END eostmt
;;         | IF expression statement_list elseif_clause END eostmt
;;         | IF expression statement_list elseif_clause
;;           ELSE statement_list END eostmt
;;         ;
;;
;; elseif_clause
;;         : ELSEIF expression statement_list
;;         | elseif_clause ELSEIF expression statement_list
;;         ;
;;
;; iteration_statement
;;         : WHILE expression statement_list END eostmt
;;         | FOR IDENTIFIER '=' expression statement_list END eostmt
;;         | FOR '(' IDENTIFIER '=' expression ')' statement_list END eostmt 
;;         ;
;;
;; jump_statement
;;         : BREAK eostmt
;;         | RETURN eostmt
;;         ;
;;
;; translation_unit
;;         : statement_list
;;         | FUNCTION function_declare eostmt statement_list
;;         ;
;;
;; func_ident_list
;;         : IDENTIFIER
;;         | func_ident_list ',' IDENTIFIER
;;         ;
;;
;; func_return_list
;;         : IDENTIFIER
;;         | '[' func_ident_list ']'
;;         ;
;;
;; function_declare_lhs
;;         : IDENTIFIER
;;         | IDENTIFIER '(' ')'
;;         | IDENTIFIER '(' func_ident_list ')'
;;         ;
;;
;; function_declare
;;         : function_declare_lhs
;;         | func_return_list '=' function_declare_lhs
;;         ;

(define (string-literal sexp)
  (local [(define (string-literal? s)
            (and (string? s)
                 (> (string-length s) 1)
                 (char=? (string-ref s 0) #\")
                 (char=? (string-ref s (sub1 (string-length s))) #\")
                 s))]
    (match sexp
      [(list (? string-literal? sl) rest-expr ...) (v*r (s-string-literal sl) rest-expr)]
      [else false])))

(define (constant sexp)
   (local [(define (constant? s)
             (and (string? s)
                  (string->number s)))]
     (match sexp
       [(list (? constant? c) rest-expr ...) (v*r (s-constant (string->number c)) rest-expr)]
       [else false])))

;; TODO
(define (reserved? s) false)

;; TODO
(define (identifier sexp)
  (local [(define (identifier? s)
            (and (string? s)
                 (> (string-length s) 0)
                 (not (reserved? s))
                 (string->symbol s)))]
    (match sexp
      [(list (? identifier? i) rest-expr ...) (v*r (s-identifier (string->symbol i)) rest-expr)]
      [else false])))

(define-type ValueXRest
  [v*r (value s-type?) (rest (listof string?))])

(define (v*r->values v)
  (type-case ValueXRest v
    [v*r (value rest) (values value rest)]))

(define (parsetest sexp)
    (match sexp
      [(list (? string? n) rest-expr ...) (v*r (s-string-literal n) rest-expr)]
      [else false]))

(define (parsetest2 sexp)
  (or (chain-parse-and-process sexp (list parsetest constant parsetest) (lambda (rest-expr a ignore b) (list ignore (list a b rest-expr))))
      (chain-parse-and-process sexp (list parsetest parsetest) (lambda (rest-expr a b) (list a b rest-expr)))))

(define (parse-string str)
  (lambda (sexp) (local [(define (string-match? s)
                           (string=? s str))]
                   (match sexp
                     [(list (? string-match? n) rest-expr ...) (v*r (s-string-literal n) rest-expr)]
                     [else false]))))

(define (chain-parse parse-fns sexp)
  (local [(define (chain-parse-helper parse-fns rsf sexp)
            (cond [(empty? parse-fns) rsf]
                  [else (local [(define parsed-v*r ((first parse-fns) sexp))]
                          (cond [(false? parsed-v*r) false]
                                [else (chain-parse-helper (rest parse-fns)
                                                          (append rsf (list parsed-v*r))
                                                          (v*r-rest parsed-v*r))]))]))]
    (chain-parse-helper parse-fns empty sexp)))

(define (process-parsing v*r-list result-fn)
  (with-handlers ([exn:fail? (lambda (exn)
                               (begin (displayln v*r-list)
                                      (displayln result-fn))
                               #f)])
    (apply result-fn (cons (v*r-rest (last v*r-list)) (map v*r-value v*r-list)))))

(define (chain-parse-and-process sexp parse-fns result-fn)
  (local [(define parsed-v*r-list (chain-parse parse-fns sexp))]
    (cond [(false? parsed-v*r-list) false]
          [else (process-parsing (chain-parse parse-fns sexp) result-fn)])))

;; statement
;;         : global_statement     ;; TODO
;;         | clear_statement      ;; TODO
;;         | assignment_statement
;;         | expression_statement
;;         | selection_statement  ;; TODO
;;         | iteration_statement  ;; TODO
;;         | jump_statement       ;; TODO
;;         ;
(define-type s-statement
  [ss-assignment-statement (as s-assignment-statement?)]
  [ss-expression-statement (es s-expression-statement?)])

(define (statement sexp)
  (or (chain-parse-and-process sexp (list assignment-statement) (lambda (rest-expr as) (v*r (ss-assignment-statement as) rest-expr)))
      (chain-parse-and-process sexp (list expression-statement) (lambda (rest-expr es) (v*r (ss-expression-statement es) rest-expr)))))

;; expression_statement
;;         : eostmt
;;         | expression eostmt
;;         ;
(define-type s-expression-statement
  [ses-eostmt]
  [ses-expression (e s-expression?)])

(define (expression-statement sexp)
  (or (chain-parse-and-process sexp (list eostmt) (lambda (rest-expr eos) (v*r (ses-eostmt) rest-expr)))
      (chain-parse-and-process sexp (list expression eostmt) (lambda (rest-expr e eos) (v*r (ses-expression (expression e)) rest-expr)))))

;; expression
;;         : or_expression
;;         | expression ':' or_expression
;;         ;
(define-type s-expression
  [se-or-expression (oe s-or-expression?)]
  [se-expression (e s-expression?) (oe s-or-expression?)])

(define (expression sexp)
  (or (chain-parse-and-process sexp (list or-expression) (lambda (rest-expr oe) (v*r (se-or-expression oe) rest-expr)))
      (chain-parse-and-process sexp (list expression (parse-string "|") or-expression) (lambda (rest-expr e ignore oe) (v*r (se-expression e oe) rest-expr)))))

;; postfix_expression
;;         : primary_expression
;;         | array_expression               ;; TODO
;;         | postfix_expression TRANSPOSE   ;; TODO
;;         | postfix_expression NCTRANSPOSE ;; TODO
;;         ;
(define-type s-postfix-expression
  [spe-primary-expression (pe s-primary-expression?)])

(define (postfix-expression sexp)
  (or (chain-parse-and-process sexp (list primary-expression) (lambda (rest-expr pe) (v*r (spe-primary-expression pe) rest-expr)))))

;; primary_expression
;;         : IDENTIFIER                  
;;         | CONSTANT    
;;         | STRING_LITERAL        
;;         | '(' expression ')'
;;         | '[' ']'
;;         | '[' array_list ']' ;; TODO
;;         ;
(define-type s-primary-expression
  [spe-identifier (i s-identifier?)]
  [spe-constant (c s-constant?)]
  [spe-string-literal (sl s-string-literal?)]
  [spe-expression (e s-expression?)]
  [spe-empty])

(define (primary-expression sexp)
  (or (chain-parse-and-process sexp (list string-literal) (lambda (rest-expr sl) (v*r (spe-string-literal sl) rest-expr)))
      (chain-parse-and-process sexp (list constant) (lambda (rest-expr c) (v*r (spe-constant c) rest-expr)))
      (chain-parse-and-process sexp (list identifier) (lambda (rest-expr i) (v*r (spe-identifier i) rest-expr)))
      (chain-parse-and-process sexp (list (parse-string "(") expression (parse-string ")")) (lambda (rest-expr ignore1 e ignore2) (v*r (spe-expression e) rest-expr)))
      (chain-parse-and-process sexp (list (parse-string "[") (parse-string "]")) (lambda (rest-expr ignore1 ignore2) (v*r (spe-empty) rest-expr))))) ; minor, consider fixing []

;; or_expression
;;         : and_expression
;;         | or_expression '|' and_expression
;;         ;
(define-type s-or-expression
  [soe-and-expression (ae s-and-expression?)]
  [soe-or-expression (oe s-or-expression?) (ae s-and-expression?)])

(define (or-expression sexp)
  (or (chain-parse-and-process sexp (list and-expression) (lambda (rest-expr ae) (v*r (soe-and-expression ae) rest-expr)))
      (chain-parse-and-process sexp (list or-expression (parse-string "|") and-expression) (lambda (rest-expr oe ignore ae) (v*r (soe-or-expression oe ae) rest-expr)))))

;; and_expression
;;         : equality_expression
;;         | and_expression '&' equality_expression
;;         ;
(define-type s-and-expression
  [sae-equality-expression (ee s-equality-expression?)]
  [sae-and-expression (ae s-and-expression?) (ee s-equality-expression?)])

(define (and-expression sexp)
  (or (chain-parse-and-process sexp (list equality-expression) (lambda (rest-expr ee) (v*r (sae-equality-expression ee) rest-expr)))
      (chain-parse-and-process sexp (list and-expression (parse-string "&") equality-expression) (lambda (rest-expr ae ignore ee) (v*r (sae-and-expression ae ee) rest-expr)))))

;; equality_expression
;;         : relational_expression
;;         | equality_expression EQ_OP relational_expression
;;         | equality_expression NE_OP relational_expression
;;         ;
(define-type s-equality-expression
  [see-relational-expression (re s-relational-expression?)]
  [s-eq-op (ee s-equality-expression?) (re s-relational-expression?)]
  [s-ne-op (ee s-equality-expression?) (re s-relational-expression?)])

(define (equality-expression sexp)
  (or (chain-parse-and-process sexp (list relational-expression) (lambda (rest-expr re) (v*r (see-relational-expression re) rest-expr)))
      (chain-parse-and-process sexp (list equality-expression (parse-string "=") relational-expression) (lambda (rest-expr ee ignore re) (v*r (s-eq-op ee re) rest-expr)))
      (chain-parse-and-process sexp (list equality-expression (parse-string "!=") relational-expression) (lambda (rest-expr ee ignore re) (v*r (s-ne-op ee re) rest-expr)))))

;; relational_expression
;;         : additive_expression
;;         | relational_expression '<' additive_expression
;;         | relational_expression '>' additive_expression
;;         | relational_expression LE_OP additive_expression
;;         | relational_expression GE_OP additive_expression
;;         ;
(define-type s-relational-expression
  [sre-additive-expression (ae s-additive-expression?)]
  [sre-lt (re s-relational-expression?) (ae s-additive-expression?)]
  [sre-gt (re s-relational-expression?) (ae s-additive-expression?)]
  [sre-lte (re s-relational-expression?) (ae s-additive-expression?)]
  [sre-gte (re s-relational-expression?) (ae s-additive-expression?)])

(define (relational-expression sexp)
  (or (chain-parse-and-process sexp (list additive-expression) (lambda (rest-expr ae) (v*r (sre-additive-expression ae) rest-expr)))
      (chain-parse-and-process sexp (list relational-expression (parse-string "<") additive-expression) (lambda (rest-expr re ignore ae) (v*r (sre-lt re ae) rest-expr)))
      (chain-parse-and-process sexp (list relational-expression (parse-string ">") additive-expression) (lambda (rest-expr re ignore ae) (v*r (sre-gt re ae) rest-expr)))
      (chain-parse-and-process sexp (list relational-expression (parse-string "<=") additive-expression) (lambda (rest-expr re ignore ae) (v*r (sre-lte re ae) rest-expr)))
      (chain-parse-and-process sexp (list relational-expression (parse-string ">=") additive-expression) (lambda (rest-expr re ignore ae) (v*r (sre-gte re ae) rest-expr)))))

;; additive_expression
;;         : multiplicative_expression
;;         | additive_expression '+' multiplicative_expression
;;         | additive_expression '-' multiplicative_expression
;;         ;
(define-type s-additive-expression
  [sae-multiplicative-expression (me s-multiplicative-expression?)]
  [sae-add (ae s-additive-expression?) (me s-multiplicative-expression?)]
  [sae-subtract (ae s-additive-expression?) (me s-multiplicative-expression?)])

(define (additive-expression sexp)
  (or (chain-parse-and-process sexp (list multiplicative-expression) (lambda (rest-expr me) (v*r (sae-multiplicative-expression me) rest-expr)))
      (chain-parse-and-process sexp (list additive-expression (parse-string "+") multiplicative-expression) (lambda (rest-expr ae ignore me) (v*r (sae-add ae me) rest-expr)))
      (chain-parse-and-process sexp (list additive-expression (parse-string "-") multiplicative-expression) (lambda (rest-expr ae ignore me) (v*r (sae-subtract ae me) rest-expr)))))

;; multiplicative_expression
;;         : unary_expression
;;         | multiplicative_expression '*' unary_expression
;;         | multiplicative_expression '/' unary_expression
;;         | multiplicative_expression '\\' unary_expression
;;         | multiplicative_expression '^' unary_expression
;;         | multiplicative_expression ARRAYMUL unary_expression  ; TODO
;;         | multiplicative_expression ARRAYDIV unary_expression  ; TODO
;;         | multiplicative_expression ARRAYRDIV unary_expression ; TODO
;;         | multiplicative_expression ARRAYPOW unary_expression  ; TODO
;;         ;
(define-type s-multiplicative-expression
  [sme-unary-expression (ue s-unary-expression?)]
  [sme-multiply (me s-multiplicative-expression?) (ue s-unary-expression?)]
  [sme-right-divide (me s-multiplicative-expression?) (ue s-unary-expression?)]
  [sme-left-divide (me s-multiplicative-expression?) (ue s-unary-expression?)]
  [sme-power (me s-multiplicative-expression?) (ue s-unary-expression?)])

(define (multiplicative-expression sexp)
  (or (chain-parse-and-process sexp (list unary-expression) (lambda (rest-expr ue) (v*r (sme-unary-expression ue) rest-expr)))
      (chain-parse-and-process sexp (list multiplicative-expression (parse-string "*") unary-expression) (lambda (rest-expr me ignore ue) (v*r (sme-multiply me ue) rest-expr)))
      (chain-parse-and-process sexp (list multiplicative-expression (parse-string "/") unary-expression) (lambda (rest-expr me ignore ue) (v*r (sme-right-divide me ue) rest-expr)))
      (chain-parse-and-process sexp (list multiplicative-expression (parse-string "\\") unary-expression) (lambda (rest-expr me ignore ue) (v*r (sme-left-divide me ue) rest-expr)))
      (chain-parse-and-process sexp (list multiplicative-expression (parse-string "^") unary-expression) (lambda (rest-expr me ignore ue) (v*r (sme-power me ue) rest-expr)))))

;; unary_expression
;;         : postfix_expression
;;         | unary_operator postfix_expression
;;         ;
;;
;; unary_operator
;;         : '+'
;;         | '-'
;;         | '~'
;;         ;
(define-type s-unary-expression
  [sue-postfix-expression (pe s-postfix-expression?)]
  [sue-unary-add (pe s-postfix-expression?)]
  [sue-unary-subtract (pe s-postfix-expression?)]
  [sue-unary-tilda (pe s-postfix-expression?)])

(define (unary-expression sexp)
  (or (chain-parse-and-process sexp (list postfix-expression) (lambda (rest-expr pe) (v*r (sue-postfix-expression pe) rest-expr)))
      (chain-parse-and-process sexp (list (parse-string "+") postfix-expression) (lambda (rest-expr ignore pe) (v*r (sue-unary-add pe) rest-expr)))
      (chain-parse-and-process sexp (list (parse-string "-") postfix-expression) (lambda (rest-expr ignore pe) (v*r (sue-unary-subtract pe) rest-expr)))
      (chain-parse-and-process sexp (list (parse-string "~") postfix-expression) (lambda (rest-expr ignore pe) (v*r (sue-unary-tilda pe) rest-expr)))))

;; statement_list
;;         : statement
;;         | statement_list statement
;;         ;
;;
;; assignment_statement
;;         : assignment_expression eostmt
;;         ;
;;
;; assignment_expression
;;         : postfix_expression '=' expression
;;
;; eostmt
;;         :  ','
;;         |  ';'
;;         |  CR
;;         ;
;;
(define-type s-octave-tl
  [s-identifier (name symbol?)]
  [s-constant (n number?)]
  [s-string-literal (str string?)]
  [s-assignment-statement (ae s-assignment-expression?)]
  [s-assignment-expression (pe s-postfix-expression?) (e s-expression?)]
  [s-empty])

(define (assignment-statement sexp)
  (or (chain-parse-and-process sexp (list assignment-expression eostmt) (lambda (rest-expr ae ignore) (v*r (s-assignment-statement ae) rest-expr)))))

(define (assignment-expression sexp)
  (or (chain-parse-and-process sexp (list postfix-expression (parse-string "=") expression) (lambda (rest-expr pe ignore e) (v*r (s-assignment-expression pe e) rest-expr)))))

(define (s-type? v)
  ((or/c s-octave-tl?
        s-expression?
        s-unary-expression?
        s-multiplicative-expression?
        s-additive-expression?
        s-relational-expression?
        s-equality-expression?
        s-and-expression?
        s-or-expression?
        s-primary-expression?
        s-postfix-expression?
        s-expression-statement?
        s-statement?) v))

(define (eostmt sexp)
        (match (if (list? sexp)
                sexp
                (list sexp))
     [(list "," rest-expr ...) (v*r (s-empty) rest-expr)]
     [(list ";" rest-expr ...) (v*r (s-empty) rest-expr)]
     [(list "\r" rest-expr ...) (v*r (s-empty) rest-expr)]
     [else false]))

(define (parse sexp)
  (local [(define (parse-helper sexp rsf)
            (cond [(empty? sexp) rsf]
                  [else
                   (or (chain-parse-and-process sexp (list statement) (lambda (rest-expr stmt) (parse-helper rest-expr (cons stmt rsf))))
                       (chain-parse-and-process sexp (list expression) (lambda (rest-expr e) (parse-helper rest-expr (cons e rsf)))))]))]
    (parse-helper sexp empty)))

;; TODO
(define (desugar sexp) sexp)

(define (run-parser file)
  (desugar (parse (string-split (file->string file)))))

(define (test1)
  (parse (string-split "a = 3 ;")))

(define (test2)
  (run-parser "examples/hello_world.m"))
