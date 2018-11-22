#lang typed/racket

(provide Pgrm Stmt Expr Constant id int bool string app int-binop bool-binop
         int-compop string-compop decl assn func if-stmt if-stmt-cond if-stmt-then Type arrow arrow? arrow-dom
         arrow-cod)

(define-type Pgrm (Listof Stmt))

(define-type Stmt
  (U Expr decl assn func if-stmt))

(define-type Expr
  (U id Constant app int-binop bool-binop int-compop string-compop))

(define-type Constant
  (U int bool string))

(struct id
  ([name : Symbol])
  #:transparent)

(struct int
  ([n : Integer])
  #:transparent)

(struct bool
  [(b : Boolean)]
  #:transparent)

(struct string
  ([s : String])
  #:transparent)

(struct app
  ([fun  : Expr]
   [args : (Listof Expr)])
  #:transparent)

(struct int-binop
  ([op  : (-> Integer Integer Integer)]
   [lhs : Expr]
   [rhs : Expr])
  #:transparent)

(struct bool-binop
  ([op  : (-> Boolean Boolean Boolean)]
   [lhs : Expr]
   [rhs : Expr])
  #:transparent)

(struct int-compop
  ([op  : (-> Integer Integer Boolean)]
   [lhs : Expr]
   [rhs : Expr])
  #:transparent)

(struct string-compop
  ([op  : (-> String String Boolean)]
   [lhs : Expr]
   [rhs : Expr])
  #:transparent)

(struct decl
  ([name : Symbol]
   [type : Type])
  #:transparent)

(struct assn
  ([vars : (Listof Symbol)]
   [expr : Expr])
  #:transparent)

(struct func
  ([name : Symbol]
   [args : (Listof (Pair Symbol Type))]
   [rets : (Listof (Pair Symbol Type))]
   [body : (Listof Stmt)])
  #:transparent)

(struct if-stmt
  ([cond : Expr]
   [then : (Listof Stmt)]
   [else : (Listof Stmt)])
  #:transparent)

(define-type Type
  (U 'int 'bool 'string 'dynamic 'none arrow (Listof Type)))

(struct arrow
  ([dom : (Listof Type)]
   [cod : (Listof Type)])
  #:transparent)
