#lang typed/racket

(require "ast.rkt")

(: consistent? (-> Type Type Boolean))
(define (consistent? T1 T2)
  (match `(,T1 ,T2)
    [`(,T1 dynamic) #t]
    [`(dynamic ,T2) #t]
    [`(,(arrow T1-dom T1-cod) ,(arrow T2-dom T2-cod))
     (and (consistent-list? T1-dom T2-dom)
          (consistent-list? T1-cod T2-cod))]
    [else (equal? T1 T2)]))

(: consistent-list? (-> (Listof Type) (Listof Type) Boolean))
(define (consistent-list? lot1 lot2)
  (andmap consistent? lot1 lot2))

(: typeof (-> Constant Type))
(define (typeof c)
  (match c
    [(int n) 'int]
    [(bool b) 'bool]
    [(string s) 'string]
    [else (error 'typeof "not implemented")]))

(define-type Env (Listof (Pair Symbol Type)))

(: typecheck (-> Env Pgrm Env))
(define (typecheck env p)
  (match p
    ['() env]
    [(cons first rest)
     (local [(define type-env (typecheck-stmt env first))]
       (typecheck type-env rest))]))

(: typecheck-stmt (-> Env Stmt Env))
(define (typecheck-stmt env s)
  (match s
    [(func name args rets body)
     (and (typecheck (append rets (append args env)) body)
          (local [(define inst-map (inst map Type (Pair Symbol Type)))]
            (cons (cons name
                        (arrow (inst-map cdr args)
                               (inst-map cdr rets)))
                  env)))]
    [(decl name type)
     (cons (cons name type) env)]
    [(assn names expr)
     (local [(define expect-types
               ((inst map Type Symbol)
                (lambda (name)
                  (local [(define result (assoc name env))]
                    (if (false? result)
                        (error 'typecheck-stmt "Unbound identifier")
                        (cdr result))))
                names))
             (define expect-length (length expect-types))
             (define actual-types
               (local [(define original (typecheck-expr env expr))
                       (define starred (if (list? original)
                                           original
                                           (list original)))
                       ]
                 (if (> (length starred) expect-length)
                     (take starred expect-length)
                     starred)))
             (define actual-length (length actual-types))]
       (cond
         [(not (= expect-length actual-length))
          (error 'typecheck-stmt "Assignment arity mismatch")]
         [(consistent-list? expect-types actual-types) env]
         [else (error 'typecheck-stmt "Assignment type mismatch")]))]
    [else
     (begin (typecheck-expr env (cast s Expr))
            env)]))

(: typecheck-expr (-> Env Expr Type))
(define (typecheck-expr env e)
  (match e
    [(id name)
     (local [(define result (assoc name env))]
       (if (false? result)
           (error 'typecheck-expr "Unbound identifier")
           (cdr result)))]
    [(int i) 'int]
    [(bool b) 'bool]
    [(string s) 'string]
    [(int-binop op lhs rhs)
     (if (and (consistent? (typecheck-expr env lhs) 'int)
              (consistent? (typecheck-expr env rhs) 'int))
         'int
         (error 'typecheck-expr "Apply integer binary operation with non-int values"))]
    [(bool-binop op lhs rhs)
     (if (and (consistent? (typecheck-expr env lhs) 'bool)
              (consistent? (typecheck-expr env rhs) 'bool))
         'bool
         (error 'typecheck-expr "Apply Boolean binary operation with non-bool values"))]
    [(int-compop op lhs rhs)
     (if (and (consistent? (typecheck-expr env lhs) 'int)
              (consistent? (typecheck-expr env rhs) 'int))
         'bool
         (error 'typecheck-expr "Apply integer comparison operation with non-int values"))]
    [(string-compop op lhs rhs)
     (if (and (consistent? (typecheck-expr env lhs) 'string)
              (consistent? (typecheck-expr env rhs) 'string))
         'bool
         (error 'typecheck-expr "Apply string comparison operation with non-string values"))]
    [(app fun args)
     (local [(define fun-type (typecheck-expr env fun))]
       (if (arrow? fun-type)
           (typecheck-star env args)
           (error 'typecheck-expr "Try to apply a non-function")))]))

(: typecheck-star (-> Env (Listof Expr) (Listof Type)))
(define (typecheck-star env es)
  (match es
    ['() '()]
    [(cons first rest)
     (cons (typecheck-expr env first)
           (typecheck-star env rest))]))