#lang typed/racket

(provide Pgrm Stmt Expr Constant id int bool string app int-binop bool-binop
         int-compop string-compop decl assn func Type arrow arrow?)

(define-type Pgrm (Listof Stmt))

(define-type Stmt
  (U Expr decl assn func))

(define-type Expr
  (U id Constant app int-binop bool-binop int-compop string-compop))

(define-type Constant
  (U int bool string))

(struct id
  ([name : Symbol]))

(struct int
  ([n : Integer]))

(struct bool
  [(b : Boolean)])

(struct string
  ([s : String]))

(struct app
  ([fun  : Expr]
   [args : (Listof Expr)]))

(struct int-binop
  ([op  : (-> Integer Integer Integer)]
   [lhs : Expr]
   [rhs : Expr]))

(struct bool-binop
  ([op  : (-> Boolean Boolean Boolean)]
   [lhs : Expr]
   [rhs : Expr]))

(struct int-compop
  ([op  : (-> Integer Integer Boolean)]
   [lhs : Expr]
   [rhs : Expr]))

(struct string-compop
  ([op  : (-> String String Boolean)]
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
  (U 'int 'bool 'string 'dynamic 'none arrow (Listof Type)))

(struct arrow
  ([dom : (Listof Type)]
   [cod : (Listof Type)]))
