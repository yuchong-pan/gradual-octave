#lang plai

(define-type Pgrm
  [pgrm (stmts (listof Stmt?))])

(define-type Stmt
  [s-expr (e Expr?)]
  [s-decl (name symbol?)
          (type Type?)]
  [s-assn (vars (listof symbol?))
          (e Expr?)]
  [s-func (fname symbol?)
          (args (listof (cons/c symbol? Type?)))
          (rets (listof (cons/c symbol? Type?)))
          (body (listof Stmt?))])

(define-type Expr
  [e-id (name symbol?)]
  [e-num (n number?)]
  [e-app (f Expr?) (args (listof Expr?))]
  [e-binop (op procedure?) (lhs Expr?) (rhs Expr?)])

(define-type Type
  [t-int]
  [t-bool]
  [t-str]
  [t-dyn]
  [t-none]
  [t-func (dom Type?) (cod Type?)])
