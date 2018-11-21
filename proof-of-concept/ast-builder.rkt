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

(define (build-ast stx)
  (local [(define (helper stx)
      (match (syntax->list stx)
      [(list (? (stx-atom? 'typed_identifier))
             (? syntax? id-stx))
       (list (stx->id id-stx) 'dynamic)] ; dynamic type if no type given
      [(list (? (stx-atom? 'typed_identifier))
             (? syntax? id-stx)
             (? (stx-atom? ":"))
             (? syntax? t-stx))
       (list (stx->id id-stx) (stx->t t-stx))]
      [(list (? (stx-atom? 'octave)) tu-stx) (helper tu-stx)]
      [(list (? (stx-atom? 'octave))
             (? (stx-many? 'octave) n-o-stx) tu-stx)
       (list (helper n-o-stx) (helper tu-stx))]
      [(list (? (stx-atom? 'translation_unit))
             (? (stx-many? 'statement_list) sl-stx)) ; stub
       (helper sl-stx)]
      [(list (? (stx-atom? 'statement_list))
             (? (stx-many? 'statement) s-stx)) ; stub
       empty]
      [(list (? (stx-atom? 'statement_list))
             (? (stx-many? 'statement_list) sl-stx)
             (? (stx-many? 'statement) s-stx)) ;stub
       empty]
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
      [(list (? (stx-atom? 'function_declare)) fdl-stx)
             (append (helper fdl-stx) (list empty))] ; We return a 3-tuple of (type, args, rets)
      [(list (? (stx-atom? 'function_declare))               ; and in this case we aren't given rets
             (? (stx-many? 'func_return_list) frl-stx)
             (? (stx-atom? "="))
             (? (stx-many? 'function_declare_lhs) fdl-stx))
       (append (helper fdl-stx) (list (helper frl-stx)))]
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
       (list (car (helper ti-stx)) (helper fil-stx))]
      [(list (? (stx-atom? 'function_return_list))
             (? (stx-atom? "["))
             (? (stx-many? 'func_ident_list) fil-stx)
             (? (stx-atom? "]")))
        (helper fil-stx)]
      [(list (? (stx-atom? 'func_ident_list))
             (? (stx-many? 'typed_identifier) ti-stx))
       (list (helper ti-stx))]
      [(list (? (stx-atom? 'func_ident_list))
             (? (stx-many? 'func_ident_list) fil-idx)
             (? (stx-atom? ","))
             (? (stx-many? 'typed_identifier) ti-stx))
       (append (list (helper ti-stx)) (helper fil-idx))]
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