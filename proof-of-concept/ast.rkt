#lang typed/racket

(provide Pgrm Pgrm? Stmt Stmt? Expr Expr? Constant Constant? id id? id-name int int? int-n bool bool? bool-b str str? str-s
         app app? app-fun app-args int-binop int-binop? int-binop-op int-binop-lhs int-binop-rhs bool-binop bool-binop? bool-binop-op bool-binop-lhs bool-binop-rhs
         int-compop int-compop? int-compop-op int-compop-lhs int-compop-rhs string-compop string-compop? string-compop-op string-compop-lhs string-compop-rhs
         decl decl? decl-name decl-type assn assn? assn-vars assn-expr func func? func-name func-args func-rets func-body if-stmt if-stmt? if-stmt-cond if-stmt-then if-stmt-else
         Type Type? arrow arrow? arrow-dom arrow-cod)

(define-type Pgrm (Listof Stmt))

(define Pgrm? (make-predicate Pgrm))

(define-type Stmt
  (U Expr decl assn func if-stmt))

(define Stmt? (make-predicate Stmt))

(define-type Expr
  (U id Constant app int-binop bool-binop int-compop string-compop))

(define Expr? (make-predicate Expr))

(define-type Constant
  (U int bool str))

(define Constant? (make-predicate Constant))

(struct id
  ([name : Symbol])
  #:transparent)

(struct int
  ([n : Integer])
  #:transparent)

(struct bool
  [(b : Boolean)]
  #:transparent)

(struct str
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

(define Type? (make-predicate Type))

(struct arrow
  ([dom : (Listof Type)]
   [cod : (Listof Type)])
  #:transparent)
