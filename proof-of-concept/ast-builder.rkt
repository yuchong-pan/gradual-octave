#lang plai

(require "ast.rkt")
(require "parser.rkt")
(require "typecheck.rkt")
(require racket/pretty)

(define (stx-atom? sexp)
  (lambda (stx)
    (cond [(symbol? sexp) (and (symbol? (syntax->datum stx))
                               (symbol=? (syntax->datum stx) sexp))]
          [(string? sexp) (and (string? (syntax->datum stx))
                                 (string=? (syntax->datum stx) sexp))]
          [(boolean? sexp) (and (boolean? (syntax->datum stx))
                                  (boolean=? (syntax->datum stx) sexp))]
          [else false])))

(define (stx-many? sexp)
  (lambda (stx)
    (local [(define sl (syntax->list stx))]
      (and (not (false? sl))
           ((stx-atom? sexp) (first sl))))))

(define (stx->id stx)
  (string->symbol (syntax->datum stx)))

(define (stx->t stx)
  (string->symbol (syntax->datum stx))) ; may want more validation later

(define (parse-literal stx)
  (or (and (string->number (syntax->datum stx)) (int (string->number (syntax->datum stx))))
      (and (string=? (syntax->datum stx) "true") (bool #t))
      (and (string=? (syntax->datum stx) "false") (bool #f))
      (string (syntax->datum stx))))

; we can use this to skip empty or not-implemented expressions
(define (valid? e)
  (not (i-null? e)))

(define (assert-type pred value)
  (if (pred value)
      value
      (error 'build-ast "received a value of the wrong type: ~a; expected ~a" value pred)))

;; Intermediate Typed Octave Construct
(define-type I-TOC
  [i-null]
  [i-id-type (name symbol?)
             (type Type?)]
  [i-assn-decl (vars (listof i-id-type?))
               (expr (or/c Expr? I-TOC?))]
  [i-app (fun (or/c Expr? I-TOC?))
         (args (listof (or/c Expr? I-TOC?)))]
  [i-int-binop (op procedure?)
               (lhs (or/c Expr? I-TOC?))
               (rhs (or/c Expr? I-TOC?))]
  [i-bool-binop (op procedure?)
                (lhs (or/c Expr? I-TOC?))
                (rhs (or/c Expr? I-TOC?))]
  [i-func (name i-id-type?)
          (args (listof i-id-type?))
          (rets (listof i-id-type?))
          (body (listof (or/c Stmt? I-TOC?)))]
  [i-if-stmt (cond (or/c Expr? I-TOC?))
             (then (listof (or/c Stmt? I-TOC?)))
             (else (listof (or/c Stmt? I-TOC?)))])

(define-type Env 
  [mtEnv]
  [anEnv (id symbol?) 
         (type Type?) 
         (more-subs Env?)])

;; lookup : symbol Env -> Type? or false
;; Finds the value of name in env or errors if name is undefined
(define (lookup name env)
  (local ([define (lookup-helper name env)
            (type-case Env env
              [mtEnv () false]
              [anEnv (bound-name bound-value rest-env)
                     (if (symbol=? bound-name name)
                         bound-value
                         (lookup-helper name rest-env))])])
    (lookup-helper name env)))


(define (desugar iast-list)
  (local [(define (add-all-env env-list env)
            (cond [(empty? env-list) env]
                  [else
                   (anEnv (anEnv-id (first env-list))
                          (anEnv-type (first env-list))
                          (add-all-env (rest env-list) env))]))
          
          ;; Assumes that something is produced
          (define (desugar-single iast env)
            (first (helper (list iast) env)))
          
          (define (helper iast-list env)
            (cond [(empty? iast-list) empty]
                  [((or/c Constant? Expr? Pgrm? Stmt?) (first iast-list)) (cons (first iast-list)                ; worst part of intermediate struct
                                                                                        (helper (rest iast-list) env))]  ; is that we also need to handle original cases
                  [else
                   (type-case I-TOC (first iast-list)
                     [i-null () (helper (rest iast-list) env)]
                     [i-id-type (n t) (cons (id n)
                                            (helper (rest iast-list) env))]
                     [i-assn-decl (v e) (local [(define new-vars (filter (lambda (x) (false? (lookup (i-id-type-name x) env))) v))
                                                (define new-env-list (map (lambda (x) (anEnv (i-id-type-name x)
                                                                                             (i-id-type-type x)
                                                                                             (mtEnv))) new-vars))
                                                (define new-decl (map (lambda (x) (decl (i-id-type-name x)
                                                                                        (i-id-type-type x))) new-vars))]
                                          (append
                                           (append new-decl (list (assn (map i-id-type-name v) (desugar-single e env))))
                                           (helper (rest iast-list) (add-all-env new-env-list env))))]
                     [i-app (f a) (cons (app (desugar-single f env) (helper a env))
                                        (helper (rest iast-list) env))]
                     [i-int-binop (o l r) (cons (int-binop o
                                                           (desugar-single l env)
                                                           (desugar-single r env))
                                                (helper (rest iast-list) env))]
                     [i-bool-binop (o l r) (cons (bool-binop o
                                                             (desugar-single l env)
                                                             (desugar-single r env))
                                                (helper (rest iast-list) env))]
                     [i-func (n a r b) (cons (func (i-id-type-name n)
                                                   (map (lambda (x) (cons (i-id-type-name x)
                                                                          (i-id-type-type x))) a)
                                                   (map (lambda (x) (cons (i-id-type-name x)
                                                                          (i-id-type-type x))) r)
                                                   (helper b env))
                                             (helper (rest iast-list) env))]
                     [i-if-stmt (c t e) (cons (if-stmt (desugar-single c env)
                                                       (helper t env)
                                                       (helper e env))
                                              (helper (rest iast-list) env))])]))]
  (helper iast-list (mtEnv))))
                  
(define (parse-intermediate stx)
  (local [(define (helper stx)
      (match (syntax->list stx)
        
      ; typed_identifier
      [(list (? (stx-atom? 'typed_identifier))
             (? syntax? id-stx))
       (i-id-type (stx->id id-stx) 'dynamic)] ; dynamic type if no type given
      [(list (? (stx-atom? 'typed_identifier))
             (? syntax? id-stx)
             (? (stx-atom? ":"))
             (? syntax? t-stx))
       (i-id-type (stx->id id-stx) (stx->t t-stx))]

      ; octave
      [(list (? (stx-atom? 'octave))
             (? (stx-many? 'translation_unit) tu-stx))
       (local [(define tu-val (helper tu-stx))
               (define list-val (if (list? tu-val)
                                         tu-val
                                         (list tu-val)))]
                list-val)]
      [(list (? (stx-atom? 'octave))
             (? (stx-many? 'octave) o-stx)
             (? (stx-many? 'translation_unit) tu-stx))
       (local [(define tu-val (helper tu-stx))
               (define list-val (if (list? tu-val)
                                         tu-val
                                         (list tu-val)))]
        (append (helper o-stx) list-val))]

      ; translation_unit
      [(list (? (stx-atom? 'translation_unit))
             (? (stx-many? 'statement_list) sl-stx))
       (helper sl-stx)]
      [(list (? (stx-atom? 'translation_unit))
             (? (stx-atom? "function"))
             (? (stx-many? 'function_declare) fd-stx)
             (? (stx-many? 'eostmt))
             (? (stx-many? 'statement_list) sl-stx)
             (? (stx-many? 'eostmt))
             (? (stx-atom? "endfunction"))
             (? (stx-many? 'eostmt)))
       (local [(define fn-val (helper fd-stx))
               (define body-val (helper sl-stx))]
         (i-func (first fn-val) (second fn-val) (third fn-val) body-val))]

      ; statement_list
      [(list (? (stx-atom? 'statement_list))
             (? (stx-many? 'statement) s-stx))
       (filter valid? (list (helper s-stx)))]
      [(list (? (stx-atom? 'statement_list))
             (? (stx-many? 'statement_list) sl-stx)
             (? (stx-many? 'statement) s-stx))
       (append (helper sl-stx) (filter valid? (list (helper s-stx))))]

      ; statement
      [(list (? (stx-atom? 'statement))
             (? (stx-many? 'assignment_statement) as-stx))
       (helper as-stx)]
      [(list (? (stx-atom? 'statement))
             (? (stx-many? 'expression_statement) es-stx))
       (helper es-stx)]
      [(list (? (stx-atom? 'statement))
             (? (stx-many? 'selection_statement) ss-stx))
       (helper ss-stx)]

      #|
      ; eostmt
      [(list (? (stx-atom? 'eostmt))
             (? (stx-atom? ",")))
       ...] ;TODO
      [(list (? (stx-atom? 'eostmt))
             (? (stx-atom? ";")))
       ...] ;TODO
      [(list (? (stx-atom? 'eostmt))
             (? (stx-atom? 'CR)))
       ...] ;TODO
      |#

      ; expression_statement
      [(list (? (stx-atom? 'expression_statement))
             (? (stx-many? 'eostmt)))
       (i-null)]
      [(list (? (stx-atom? 'expression_statement))
             (? (stx-many? 'expression) e-stx)
             (? (stx-many? 'eostmt)))
       (helper e-stx)]

      ; assignment_statement
      [(list (? (stx-atom? 'assignment_statement))
             (? (stx-many? 'assignment_expression) ae-stx)
             (? (stx-many? 'eostmt)))
       (helper ae-stx)]

      ; assignment_expression
      [(list (? (stx-atom? 'assignment_expression))
             (? (stx-many? 'postfix_expression) pe-stx)
             (? (stx-atom? "="))
             (? (stx-many? 'expression) e-stx))
       (local [(define ids (helper pe-stx))
               (define id-list (or (and (list? ids) ids)
                                   (list ids)))] ; can get single assignments too
         (i-assn-decl id-list (helper e-stx)))]

      ; postfix_expression
      [(list (? (stx-atom? 'postfix_expression))
             (? (stx-many? 'primary_expression) pe-stx))
       (helper pe-stx)]
      [(list (? (stx-atom? 'postfix_expression))
             (? (stx-many? 'array_expression) ae-stx))
       (helper ae-stx)]

      ; array_expression
      [(list (? (stx-atom? 'array_expression))
             (? (stx-many? 'typed_identifier) ti-stx)
             (? (stx-atom? "("))
             (? (stx-many? 'index_expression_list) iel-stx)
             (? (stx-atom? ")")))
       (i-app (helper ti-stx) (helper iel-stx))]

      ; index_expression_list
      [(list (? (stx-atom? 'index_expression_list))
             (? (stx-many? 'index_expression) ie-stx))
       (list (helper ie-stx))]
      [(list (? (stx-atom? 'index_expression_list))
             (? (stx-many? 'index_expression_list) iel-stx)
             (? (stx-atom? ","))
             (? (stx-many? 'index_expression) ie-stx))
       (append (helper iel-stx) (list (helper ie-stx)))]

      ; index_expression
      [(list (? (stx-atom? 'index_expression))
             (? (stx-atom? ":")))
       (i-null)]
      [(list (? (stx-atom? 'index_expression))
             (? (stx-many? 'expression) e-stx))
       (helper e-stx)]

      ; primary_expression
      [(list (? (stx-atom? 'primary_expression))
             (? (stx-many? 'typed_identifier) ti-stx))
       (helper ti-stx)]
      [(list (? (stx-atom? 'primary_expression))
             (? syntax? literal)) ; string literal
       (parse-literal literal)]
      [(list (? (stx-atom? 'primary_expression))
             (? (stx-atom? "("))
             (? (stx-many? 'expression) e-stx)
             (? (stx-atom? ")")))
       (helper e-stx)]
      [(list (? (stx-atom? 'primary_expression))
             (? (stx-atom? "["))
             (? (stx-atom? "]")))
       (list)] ; TODO, I think this initializes a matrix
      [(list (? (stx-atom? 'primary_expression))
             (? (stx-atom? "["))
             (? (stx-many? 'array_list) al-stx)
             (? (stx-atom? "]")))
       (helper al-stx)]

      ; array_list
      [(list (? (stx-atom? 'array_list))
             (? (stx-many? 'array_element) ae-stx))
       (list (helper ae-stx))]
      [(list (? (stx-atom? 'array_list))
             (? (stx-many? 'array_list) al-stx)
             (? (stx-many? 'array_element) ae-stx))
       (append (helper al-stx) (list (helper ae-stx)))]

      ; array_element
      [(list (? (stx-atom? 'array_element))
             (? (stx-many? 'expression) e-stx))
       (helper e-stx)]
      [(list (? (stx-atom? 'array_element))
             (? (stx-many? 'expression_statement) es-stx))
       (helper es-stx)]

      ; expression
      [(list (? (stx-atom? 'expression))
             (? (stx-many? 'or_expression) oe-stx))
       (helper oe-stx)]
      [(list (? (stx-atom? 'expression))
             (? (stx-many? 'expression) e-stx)
             (? (stx-atom? ":"))
             (? (stx-many? 'or_expression) oe-stx))
       (in-range (helper e-stx) (helper oe-stx) 1)] ; DONE: Create a vector, e.g. x = 1:10

      ; or_expression
      [(list (? (stx-atom? 'or_expression))
             (? (stx-many? 'and_expression) ae-stx))
       (helper ae-stx)]
      [(list (? (stx-atom? 'or_expression))
             (? (stx-many? 'or_expression) oe-stx)
             (? (stx-atom? "|"))
             (? (stx-many? 'and_expression) ae-stx))
         (i-bool-binop (lambda (x y) (not (and (false? x) (false? y)))) (helper oe-stx) (helper ae-stx))]

      ; and_expression
      [(list (? (stx-atom? 'and_expression))
             (? (stx-many? 'equality_expression) ee-stx))
       (helper ee-stx)]
      [(list (? (stx-atom? 'and_expression))
             (? (stx-many? 'and_expression) ae-stx)
             (? (stx-atom? "&"))
             (? (stx-many? 'equality_expression) ee-stx))
       (i-bool-binop (lambda (x y) (not (or (false? x) (false? y)))) (helper ae-stx) (helper ee-stx))]

      ; equality_expression
      [(list (? (stx-atom? 'equality_expression))
             (? (stx-many? 'relational_expression) re-stx))
       (helper re-stx)]
      [(list (? (stx-atom? 'equality_expression))
             (? (stx-many? 'equality_expression) ee-stx)
             (? (stx-atom? "=="))
             (? (stx-many? 'relational_expression) re-stx))
       (i-bool-binop (lambda (x y) (equal? x y)) (helper ee-stx) (helper re-stx))] ; helper for this too for now
      [(list (? (stx-atom? 'equality_expression))
             (? (stx-many? 'equality_expression) ee-stx)
             (? (stx-atom? "!="))
             (? (stx-many? 'relational_expression) re-stx))
       (i-bool-binop (lambda (x y) (not (equal? x y))) (helper ee-stx) (helper re-stx))]

      ; relational_expression
      [(list (? (stx-atom? 'relational_expression))
             (? (stx-many? 'additive_expression) ae-stx))
       (helper ae-stx)]
      [(list (? (stx-atom? 'relational_expression))
             (? (stx-many? 'relational_expression) re-stx)
             (? (stx-atom? "<"))
             (? (stx-many? 'additive_expression) ae-stx))
       (i-bool-binop (lambda (x y) (< x y)) (helper re-stx) (helper ae-stx))]
      [(list (? (stx-atom? 'relational_expression))
             (? (stx-many? 'relational_expression) re-stx)
             (? (stx-atom? ">"))
             (? (stx-many? 'additive_expression) ae-stx))
       (i-bool-binop (lambda (x y) (> x y)) (helper re-stx) (helper ae-stx))]
      [(list (? (stx-atom? 'relational_expression))
             (? (stx-many? 'relational_expression) re-stx)
             (? (stx-atom? "<="))
             (? (stx-many? 'additive_expression) ae-stx))
       (i-bool-binop (lambda (x y) (<= x y)) (helper re-stx) (helper ae-stx))]
      [(list (? (stx-atom? 'relational_expression))
             (? (stx-many? 'relational_expression) re-stx)
             (? (stx-atom? ">="))
             (? (stx-many? 'additive_expression) ae-stx))
       (i-bool-binop (lambda (x y) (>= x y)) (helper re-stx) (helper ae-stx))]

      ; additive_expression
      [(list (? (stx-atom? 'additive_expression))
             (? (stx-many? 'multiplicative_expression) me-stx))
       (helper me-stx)]
      [(list (? (stx-atom? 'additive_expression))
             (? (stx-many? 'additive_expression) ae-stx)
             (? (stx-atom? "+"))
             (? (stx-many? 'multiplicative_expression) me-stx))
       (i-int-binop (lambda (x y) (+ x y)) (helper ae-stx) (helper me-stx))] ; again, probably won't need lambda but hm
      [(list (? (stx-atom? 'additive_expression))
             (? (stx-many? 'additive_expression) ae-stx)
             (? (stx-atom? "-"))
             (? (stx-many? 'multiplicative_expression) me-stx))
       (i-int-binop (lambda (x y) (- x y)) (helper ae-stx) (helper me-stx))] ; these actually might need changing later
                                                                           ; for matrices, not only ints

      ; multiplicative_expression
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'unary_expression) ue-stx))
       (helper ue-stx)]
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? "*"))
             (? (stx-many? 'unary_expression) ue-stx))
       (i-int-binop (lambda (x y) (* x y)) (helper me-stx) (helper ue-stx))]
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? "/"))
             (? (stx-many? 'unary_expression) ue-stx))
       (i-int-binop (lambda (x y) (/ x y)) (helper me-stx) (helper ue-stx))]
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? "\\"))
             (? (stx-many? 'unary_expression) ue-stx))
       (i-int-binop (lambda (x y) (/ x y)) (helper me-stx) (helper ue-stx))] ; TODO: this doesn't work yet
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? "^"))
             (? (stx-many? 'unary_expression) ue-stx))
       (i-int-binop (lambda (x y) (expt x y)) (helper me-stx) (helper ue-stx))]
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? ".*"))
             (? (stx-many? 'unary_expression) ue-stx))
       (i-int-binop (lambda (x y) (* x y)) (helper me-stx) (helper ue-stx))] ; TODO: Element-wise multiplication
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? "./"))
             (? (stx-many? 'unary_expression) ue-stx))
       (i-int-binop (lambda (x y) (/ x y)) (helper me-stx) (helper ue-stx))] ; TODO: Element-wise right division
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? ".\\"))
             (? (stx-many? 'unary_expression) ue-stx))
       (i-int-binop (lambda (x y) (/ x y)) (helper me-stx) (helper ue-stx))] ; TODO: Element-wise left division
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? ".^"))
             (? (stx-many? 'unary_expression) ue-stx))
       (i-int-binop (lambda (x y) (expt x y)) (helper me-stx) (helper ue-stx))] ; TODO: Element-wise power

      ; unary_expression
      [(list (? (stx-atom? 'unary_expression))
             (? (stx-many? 'postfix_expression) pe-stx))
       (helper pe-stx)]
      [(list (? (stx-atom? 'unary_expression))
             (? (stx-atom? "+"))
             (? (stx-many? 'postfix_expression) pe-stx))
       (helper pe-stx)] ; DONE: just return this value?
      [(list (? (stx-atom? 'unary_expression))
             (? (stx-atom? "-"))
             (? (stx-many? 'postfix_expression) pe-stx))
       (- (helper pe-stx))] ; DONE: Normally subtracts the second (and following) number(s) from the first ; negates the number if there is only one argument.

      [(list (? (stx-atom? 'unary_expression))
             (? (stx-atom? "~"))
             (? (stx-many? 'postfix_expression) pe-stx))
       (- (helper pe-stx))] ; DONE: same as "-"
        
      ; function_declare
      [(list (? (stx-atom? 'function_declare))
             (? (stx-many? 'function_declare_lhs) fdl-stx))
       (append (helper fdl-stx) empty)] ; We return a 3-tuple of (type, args, rets)
      [(list (? (stx-atom? 'function_declare))               ; and in this case we aren't given rets
             (? (stx-many? 'func_return_list) frl-stx)
             (? (stx-atom? "="))
             (? (stx-many? 'function_declare_lhs) fdl-stx))
       (append (helper fdl-stx) (list (helper frl-stx)))]

      ; function_declare_lhs
      [(list (? (stx-atom? 'function_declare_lhs))
             (? (stx-many? 'typed_identifier) ti-stx))
       (list (helper ti-stx) empty)]
      [(list (? (stx-atom? 'function_declare_lhs))
             (? (stx-many? 'typed_identifier) ti-stx)
             (? (stx-atom? "("))
             (? (stx-atom? ")")))
       (list (car (helper ti-stx)) empty)] ; don't need type for function names
      [(list (? (stx-atom? 'function_declare_lhs))
             (? (stx-many? 'typed_identifier) ti-stx)
             (? (stx-atom? "("))
             (? (stx-many? 'func_ident_list) fil-stx)
             (? (stx-atom? ")")))
       (list (helper ti-stx) (helper fil-stx))]

      ; function_return_list
      [(list (? (stx-atom? 'function_return_list))
             (? (stx-atom? "["))
             (? (stx-many? 'func_ident_list) fil-stx)
             (? (stx-atom? "]")))
        (helper fil-stx)]

      ; function_ident_list
      [(list (? (stx-atom? 'func_ident_list))
             (? (stx-many? 'typed_identifier) ti-stx))
       (list (helper ti-stx))]
      [(list (? (stx-atom? 'func_ident_list))
             (? (stx-many? 'func_ident_list) fil-idx)
             (? (stx-atom? ","))
             (? (stx-many? 'typed_identifier) ti-stx))
       (append (list (helper ti-stx)) (helper fil-idx))]

      ; func_return_list
      [(list (? (stx-atom? 'func_return_list))
             (? (stx-many? 'typed_identifier) ti-stx))
       (list (helper ti-stx))]
      [(list (? (stx-atom? 'func_return_list))
             (? (stx-atom? "["))
             (? (stx-many? 'func_ident_list) fil-stx)
             (? (stx-atom? "]")))
       (helper fil-stx)]

      #|
      ; global_statement
      [(list (? (stx-atom? 'global_statement))
             (? (stx-many? 'identifier_list) il-stx))
       (list (helper il-stx))]
      |#
        
     ; elseif_clause
     [(list (? (stx-atom? 'elseif_clause))
            (? (stx-atom? "elseif"))
            (? (stx-many? 'expression) e-stx)
            (? (stx-many? 'statement_list) s-stx))
      (list (i-if-stmt (helper e-stx)
                       (helper s-stx)
                       empty))]
     [(list (? (stx-atom? 'elseif_clause))
            (? (stx-many? 'elseif_clause) eif-stx)
            (? (stx-atom? "elseif"))
            (? (stx-many? 'expression) e-stx)
            (? (stx-many? 'statement_list) s-stx))
      (append (helper eif-stx) (list (i-if-stmt (helper e-stx)
                                                (helper s-stx)
                                                empty)))]

      ; selection_statement
      [(list (? (stx-atom? 'selection_statement))
             (? (stx-atom? "if"))
             (? (stx-many? 'expression) e-stx)
             (? (stx-atom? "\n")) ; TODO: not sure if this is stx-many
             (? (stx-many? 'statement_list) s-stx)
             (? (stx-atom? "endif"))
             (? (stx-atom? "\n")))
       (i-if-stmt (helper e-stx)
                  (helper s-stx)
                  empty)]
      [(list (? (stx-atom? 'selection_statement))
             (? (stx-atom? "if"))
             (? (stx-many? 'expression) e-stx)
             (? (stx-atom? "\n"))
             (? (stx-many? 'statement_list) s-stx)
             (? (stx-atom? "else"))
             (? (stx-many? 'statement_list) s2-stx)
             (? (stx-atom? "endif"))
             (? (stx-atom? "\n")))
       (i-if-stmt (helper e-stx)
                  (helper s-stx)
                  (helper s2-stx))]
      [(list (? (stx-atom? 'selection_statement))
             (? (stx-atom? "if"))
             (? (stx-many? 'expression) e-stx)
             (? (stx-atom? "\n"))
             (? (stx-many? 'statement_list) s-stx)
             (? (stx-many? 'elseif_clause) eif-stx)
             (? (stx-atom? "endif"))
             (? (stx-atom? "\n")))
       (i-if-stmt (helper e-stx)
                  (helper s-stx)
                  (list (flatten-elseif (helper eif-stx))))]
      [(list (? (stx-atom? 'selection_statement))
             (? (stx-atom? "if"))
             (? (stx-many? 'expression) e-stx)
             (? (stx-atom? "\n"))
             (? (stx-many? 'statement_list) s-stx)
             (? (stx-many? 'elseif_clause) eif-stx)
             (? (stx-atom? "else"))
             (? (stx-many? 'statement_list) s2-stx)
             (? (stx-atom? "endif"))
             (? (stx-atom? "\n")))
       (local [(define child-elseif-v (flatten-elseif (helper eif-stx)))
               (define child-else-v (helper s2-stx))]
       (i-if-stmt (helper e-stx)
                  (helper s-stx)
                  (list (insert-else child-else-v child-elseif-v))))]
      [_ (error 'build-ast "Unable to recognize expr: ~a" (~a (pretty-format (syntax->datum stx))))]))]
    (helper stx)))

(define (flatten-elseif elseif-list)
  (cond [(empty? elseif-list) empty]
        [else
         (local [(define rec-val (flatten-elseif (rest elseif-list)))
                 (define rec-list-val (if (list? rec-val)
                                          rec-val
                                          (list rec-val)))]
         (i-if-stmt (i-if-stmt-cond (first elseif-list))
                    (i-if-stmt-then (first elseif-list))
                    rec-list-val))]))

(define (insert-else else-v elseif-v)
  (cond [(empty? (i-if-stmt-else elseif-v))
         (i-if-stmt (i-if-stmt-cond elseif-v)
                    (i-if-stmt-then elseif-v)
                    else-v)]
        [else
         (i-if-stmt (i-if-stmt-cond elseif-v)
                    (i-if-stmt-then elseif-v)
                    (list (insert-else else-v (first (i-if-stmt-else elseif-v)))))])) ; assumption that this is elseif only

(define (build-ast stx)
  (desugar (parse-intermediate stx)))

(define (test1) (open-input-string "a = 3;"))
(define test1-tokens (token-list (tokenize (test1))))
(define test1-parsed (parse (tokenize (test1))))
(define test1-intermediate (parse-intermediate test1-parsed))
(define test1-ast (desugar test1-intermediate))

(define (test2) (open-input-string (file->string "examples/hello_world.m")))
(define test2-tokens (token-list (tokenize (test2))))
(define test2-parsed (parse (tokenize (test2))))
(define test2-intermediate (parse-intermediate test2-parsed))
(define test2-ast (desugar test2-intermediate))

(define (test3) (open-input-string (file->string "examples/circle3d-test.m")))
(define test3-tokens (token-list (tokenize (test3))))
(define test3-parsed (parse (tokenize (test3))))
(define test3-intermediate (parse-intermediate test3-parsed))
(define test3-ast (desugar test3-intermediate))

(define (test4) (open-input-string (file->string "examples/test_function.m")))
(define test4-tokens (token-list (tokenize (test4))))
(define test4-parsed (parse (tokenize (test4))))
(define test4-intermediate (parse-intermediate test4-parsed))
(define test4-ast (desugar test4-intermediate))

(define (test5) (open-input-string (file->string "examples/test_elseif.m")))
(define test5-tokens (token-list (tokenize (test5))))
(define test5-parsed (parse (tokenize (test5))))
(define test5-intermediate (parse-intermediate test5-parsed))
(define test5-ast (desugar test5-intermediate))
