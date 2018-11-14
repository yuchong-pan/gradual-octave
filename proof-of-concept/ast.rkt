#lang typed/racket

(struct pgrm
  ([stmts : (Listof Stmt)]))

(define-type Stmt
  (U Expr decl assn func))

(define-type Expr
  (U id int bool string app binop))

(struct id
  ([name : Symbol]))

(struct int
  ([n : Integer]))

(struct bool
  ([b : Boolean]))

(struct string
  ([s : String]))

(struct app
  ([fun  : Expr]
   [args : (Listof Expr)]))

(struct binop
  ([op  : Procedure]
   [lhs : Expr]
   [rhs : Expr]))

(struct decl
  ([name : Symbol]
   [type : Type]))

(struct assn
  ([vars : (Listof Symbol)]
   [expr : Expr]))

(struct func
  ([name : Symbol]
   [args : (Listof (Pair Symbol Type))]
   [rets : (Listof (Pair Symbol Type))]
   [body : (Listof Stmt)]))

(define-type Type
  (U 'int 'boolean 'string 'dynamic 'none arrow star))

(struct arrow
  ([dom : Type]
   [cod : Type]))

(struct star
  ([list : (Listof Type)]))
