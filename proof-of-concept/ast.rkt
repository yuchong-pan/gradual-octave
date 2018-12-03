#lang typed/racket

(provide Pgrm Pgrm? Stmt Stmt? Expr Expr? Constant Constant? id id? id-name int int-n bool bool? bool-b str str? str-s
         app app? app-fun app-args int-binop int-binop? int-binop-name int-binop-op int-binop-lhs int-binop-rhs bool-binop bool-binop? bool-binop-name bool-binop-op bool-binop-lhs bool-binop-rhs
         int-compop int-compop? int-compop-name int-compop-op int-compop-lhs int-compop-rhs string-compop string-compop? string-compop-name string-compop-op string-compop-lhs string-compop-rhs
         decl decl? decl-name decl-type assn assn? assn-vars assn-expr func func? func-name func-args func-rets func-body if-stmt if-stmt? if-stmt-cond if-stmt-then if-stmt-else
         Type Type? arrow arrow? arrow-dom arrow-cod matrix matrix? matrixT matrixT? MatrixRow MatrixRow? UNBOUNDED)

(define-type Pgrm (Listof Stmt))

(define Pgrm? (make-predicate Pgrm))

(define-type Stmt
  (U Expr decl assn func if-stmt))

(define Stmt? (make-predicate Stmt))

(define-type Expr
  (U id Constant app int-binop bool-binop int-compop string-compop))

(define Expr? (make-predicate Expr))

(define-type Constant
  (U int bool str matrix))

(define Constant? (make-predicate Constant))

(define-type MatrixRow
  (U Expr (Listof Constant)))

(define MatrixRow? (make-predicate Constant))

(struct matrix
  ([data : (Listof MatrixRow)])
  #:transparent)

(define UNBOUNDED -1)

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
  ([name : Symbol]
   [op  : (-> Integer Integer Integer)]
   [lhs : Expr]
   [rhs : Expr])
  #:transparent)

(struct bool-binop
  ([name : Symbol]
   [op  : (-> Boolean Boolean Boolean)]
   [lhs : Expr]
   [rhs : Expr])
  #:transparent)

(struct int-compop
  ([name : Symbol]
   [op  : (-> Integer Integer Boolean)]
   [lhs : Expr]
   [rhs : Expr])
  #:transparent)

(struct string-compop
  ([name : Symbol]
   [op  : (-> String String Boolean)]
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
  (U 'int 'bool 'string matrixT 'dynamic 'none arrow (Listof Type)))

(define Type? (make-predicate Type))

(struct matrixT
  ([typeConstr : Type]
   [rowConstr : Integer]
   [colConstr : Integer])
  #:transparent)

(struct arrow
  ([dom : (Listof Type)]
   [cod : (Listof Type)])
  #:transparent)
