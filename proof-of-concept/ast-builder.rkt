#lang plai

(require racket/pretty)

(define (stx? sexp)
  (lambda (stx) (symbol=? (syntax->datum stx) sexp)))

(define (stx-child? sexp)
  (lambda (stx) (symbol=? (syntax->datum (first (syntax->list stx))) sexp)))

(define (build-ast stx)
  (local [(define parse-results empty)]
    (match (syntax->list stx)
             [(list (? (stx? 'octave)) rest-stx) (build-ast rest-stx)]
             [(list (? (stx? 'octave)) (? (stx-child? 'octave) nested-stx) rest-stx)
              (begin (build-ast nested-stx)
                     (build-ast rest-stx))]
             [(list (? (stx? 'translation_unit)) rest-stx ...) (map syntax->datum rest-stx)]
             [_ (error 'parse "Unable to recognize expr: ~a" (~a (syntax->datum stx) #:max-width 500))])))