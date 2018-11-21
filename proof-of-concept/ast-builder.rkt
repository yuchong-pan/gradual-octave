#lang plai

(require "ast.rkt")
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

(define (build-ast stx)
  (local [(define (helper stx)
      (match (syntax->list stx)
      ; typed_identifier
      [(list (? (stx-atom? 'typed_identifier))
             (? syntax? id-stx))
       (iden (stx->id id-stx) 'dynamic)] ; dynamic type if no type given
      [(list (? (stx-atom? 'typed_identifier))
             (? syntax? id-stx)
             (? (stx-atom? ":"))
             (? syntax? t-stx))
       (iden (stx->id id-stx) (stx->t t-stx))]

      ; octave
      [(list (? (stx-atom? 'octave)) tu-stx) (helper tu-stx)]
      [(list (? (stx-atom? 'octave))
             (? (stx-many? 'octave) n-o-stx) tu-stx)
       (list (helper n-o-stx) (helper tu-stx))]

      ; translation_unit
      [(list (? (stx-atom? 'translation_unit))
             (? (stx-many? 'statement_list) sl-stx)) ; stub
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
         (func (first fn-val) (second fn-val) (third fn-val) body-val))]

      ; statement_list
      [(list (? (stx-atom? 'statement_list))
             (? (stx-many? 'statement) s-stx))
       (list (helper s-stx))]
      [(list (? (stx-atom? 'statement_list))
             (? (stx-many? 'statement_list) sl-stx)
             (? (stx-many? 'statement) s-stx))
       (append (list (helper s-stx)) (list (helper s-stx)))]

      ; statement
      [(list (? (stx-atom? 'statement))
             (? (stx-many? 'assignment_statement) as-stx))
       (helper as-stx)]
      [(list (? (stx-atom? 'statement))
             (? (stx-many? 'expression_statement) es-stx))
       (helper es-stx)]

      ; expression_statement
      [(list (? (stx-atom? 'expression_statement))
             (? (stx-many? 'eostmt)))
       empty] ; verify that this is working as intended
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
       (assn (list (helper pe-stx)) (helper e-stx))]

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
       (app (helper ti-stx) (helper iel-stx))]

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
       (bool #f)] ; TODO
      [(list (? (stx-atom? 'index_expression))
             (? (stx-many? 'expression) e-stx))
       (helper e-stx)]

      ; primary_expression
      [(list (? (stx-atom? 'primary_expression))
             (? (stx-many? 'typed_identifier) ti-stx))
       (helper ti-stx)]
      [(list (? (stx-atom? 'primary_expression))
             (? syntax? literal))
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

      ; expression
      [(list (? (stx-atom? 'expression))
             (? (stx-many? 'or_expression) oe-stx))
       (helper oe-stx)]
      [(list (? (stx-atom? 'expression))
             (? (stx-many? 'expression) e-stx)
             (? (stx-atom? ":"))
             (? (stx-many? 'or_expression) oe-stx))
       (list)] ; TODO: the hecc is this doing?

      ; or_expression
      [(list (? (stx-atom? 'or_expression))
             (? (stx-many? 'and_expression) ae-stx))
       (helper ae-stx)]
      [(list (? (stx-atom? 'or_expression))
             (? (stx-many? 'or_expression) oe-stx)
             (? (stx-atom? "|"))
             (? (stx-many? 'and_expression) ae-stx))
         (bool-binop (lambda (x y) (not (and (false? x) (false? y)))) (helper oe-stx) (helper ae-stx))]

      ; and_expression
      [(list (? (stx-atom? 'and_expression))
             (? (stx-many? 'equality_expression) ee-stx))
       (helper ee-stx)]
      [(list (? (stx-atom? 'and_expression))
             (? (stx-many? 'and_expression) ae-stx)
             (? (stx-atom? "&"))
             (? (stx-many? 'equality_expression) ee-stx))
       (bool-binop (lambda (x y) (not (or (false? x) (false? y)))) (helper ae-stx) (helper ee-stx))]

      ; equality_expression
      [(list (? (stx-atom? 'equality_expression))
             (? (stx-many? 'relational_expression) re-stx))
       (helper re-stx)]
      [(list (? (stx-atom? 'equality_expression))
             (? (stx-many? 'equality_expression) ee-stx)
             (? (stx-atom? "=="))
             (? (stx-many? 'relational_expression) re-stx))
       (bool-binop (lambda (x y) (equal? x y)) (helper ee-stx) (helper re-stx))] ; helper for this too for now
      [(list (? (stx-atom? 'equality_expression))
             (? (stx-many? 'equality_expression) ee-stx)
             (? (stx-atom? "!="))
             (? (stx-many? 'relational_expression) re-stx))
       (bool-binop (lambda (x y) (not (equal? x y))) (helper ee-stx) (helper re-stx))]

      ; relational_expression
      [(list (? (stx-atom? 'relational_expression))
             (? (stx-many? 'additive_expression) ae-stx))
       (helper ae-stx)]
      [(list (? (stx-atom? 'relational_expression))
             (? (stx-many? 'relational_expression) re-stx)
             (? (stx-atom? "<"))
             (? (stx-many? 'additive_expression) ae-stx))
       (bool-binop (lambda (x y) (< x y)) (helper re-stx) (helper ae-stx))]
      [(list (? (stx-atom? 'relational_expression))
             (? (stx-many? 'relational_expression) re-stx)
             (? (stx-atom? ">"))
             (? (stx-many? 'additive_expression) ae-stx))
       (bool-binop (lambda (x y) (> x y)) (helper re-stx) (helper ae-stx))]
      [(list (? (stx-atom? 'relational_expression))
             (? (stx-many? 'relational_expression) re-stx)
             (? (stx-atom? "<="))
             (? (stx-many? 'additive_expression) ae-stx))
       (bool-binop (lambda (x y) (<= x y)) (helper re-stx) (helper ae-stx))]
      [(list (? (stx-atom? 'relational_expression))
             (? (stx-many? 'relational_expression) re-stx)
             (? (stx-atom? ">="))
             (? (stx-many? 'additive_expression) ae-stx))
       (bool-binop (lambda (x y) (>= x y)) (helper re-stx) (helper ae-stx))]

      ; additive_expression
      [(list (? (stx-atom? 'additive_expression))
             (? (stx-many? 'multiplicative_expression) me-stx))
       (helper me-stx)]
      [(list (? (stx-atom? 'additive_expression))
             (? (stx-many? 'additive_expression) ae-stx)
             (? (stx-atom? "+"))
             (? (stx-many? 'multiplicative_expression) me-stx))
       (int-binop (lambda (x y) (+ x y)) (helper ae-stx) (helper me-stx))] ; again, probably won't need lambda but hm
      [(list (? (stx-atom? 'additive_expression))
             (? (stx-many? 'additive_expression) ae-stx)
             (? (stx-atom? "-"))
             (? (stx-many? 'multiplicative_expression) me-stx))
       (int-binop (lambda (x y) (- x y)) (helper ae-stx) (helper me-stx))] ; these actually might need changing later
                                                                           ; for matrices, not only ints

      ; multiplicative_expression
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'unary_expression) ue-stx))
       (helper ue-stx)]
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? "*"))
             (? (stx-many? 'unary_expression) ue-stx))
       (int-binop (lambda (x y) (* x y)) (helper me-stx) (helper ue-stx))]
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? "/"))
             (? (stx-many? 'unary_expression) ue-stx))
       (int-binop (lambda (x y) (/ x y)) (helper me-stx) (helper ue-stx))]
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? "\\"))
             (? (stx-many? 'unary_expression) ue-stx))
       (int-binop (lambda (x y) (/ x y)) (helper me-stx) (helper ue-stx))] ; TODO: this doesn't work yet
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? "^"))
             (? (stx-many? 'unary_expression) ue-stx))
       (int-binop (lambda (x y) (expt x y)) (helper me-stx) (helper ue-stx))]
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? ".*"))
             (? (stx-many? 'unary_expression) ue-stx))
       (int-binop (lambda (x y) (* x y)) (helper me-stx) (helper ue-stx))] ; TODO
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? "./"))
             (? (stx-many? 'unary_expression) ue-stx))
       (int-binop (lambda (x y) (/ x y)) (helper me-stx) (helper ue-stx))] ; TODO
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? ".\\"))
             (? (stx-many? 'unary_expression) ue-stx))
       (int-binop (lambda (x y) (/ x y)) (helper me-stx) (helper ue-stx))] ; TODO
      [(list (? (stx-atom? 'multiplicative_expression))
             (? (stx-many? 'multiplicative_expression) me-stx)
             (? (stx-atom? ".^"))
             (? (stx-many? 'unary_expression) ue-stx))
       (int-binop (lambda (x y) (expt x y)) (helper me-stx) (helper ue-stx))] ; TODO

      ; unary_expression
      [(list (? (stx-atom? 'unary_expression))
             (? (stx-many? 'postfix_expression) pe-stx))
       (helper pe-stx)]
      [(list (? (stx-atom? 'unary_expression))
             (? (stx-atom? "+"))
             (? (stx-many? 'postfix_expression) pe-stx))
       (helper pe-stx)] ; no clue how to handle
      [(list (? (stx-atom? 'unary_expression))
             (? (stx-atom? "-"))
             (? (stx-many? 'postfix_expression) pe-stx))
       (helper pe-stx)] ; no clue how to handle
      [(list (? (stx-atom? 'unary_expression))
             (? (stx-atom? "~"))
             (? (stx-many? 'postfix_expression) pe-stx))
       (helper pe-stx)] ; no clue how to handle (this one is presumably a negation)
        
      ; function_declare
      [(list (? (stx-atom? 'function_declare))
             (? (stx-many? 'function_declare_lhs) fdl-stx))
       (append (helper fdl-stx) (list empty))] ; We return a 3-tuple of (type, args, rets)
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
       (list (iden-name (helper ti-stx)) (helper fil-stx))]

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
      [_ (error 'build-ast "Unable to recognize expr: ~a" (~a (pretty-format (syntax->datum stx)) #:max-width 1000))]))]
    (helper stx)))