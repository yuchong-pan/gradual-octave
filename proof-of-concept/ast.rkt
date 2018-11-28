#lang typed/racket

(provide Pgrm Pgrm? Stmt Stmt? Expr Expr? Constant Constant? id int int-n bool string app int-binop bool-binop
         int-compop string-compop decl assn func if-stmt if-stmt-cond if-stmt-then Type Type? arrow arrow? arrow-dom
         arrow-cod matrix matrix? matrixT matrixT? MatrixRow MatrixRow? UNBOUNDED)

(define-type Pgrm (Listof Stmt))

(define Pgrm? (make-predicate Pgrm))

(define-type Stmt
  (U Expr decl assn func if-stmt))

(define Stmt? (make-predicate Stmt))

(define-type Expr
  (U id Constant app int-binop bool-binop int-compop string-compop))

(define Expr? (make-predicate Expr))

(define-type Constant
  (U int bool string matrix))

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
