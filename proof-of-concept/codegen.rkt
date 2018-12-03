#lang racket

(require "ast.rkt")
(require "ast-builder.rkt")

(define (spacing depth)
  (local [(define (helper depth rsf)
            (cond [(zero? depth) rsf]
                  [else
                   (helper (sub1 depth)
                           (string-append "    " rsf))]))]
    (helper depth "")))

(define (matrix-helper data)
  (cond [(empty? data) ""]
        [else
         (string-join (map (lambda (row)
                             (string-join (map Constant->Octave row) " ")) data) ; assume list of constants for now
                      "; ")]))
                 
(define (Constant->Octave cnst)
  (cond [(int? cnst) (number->string (int-n cnst))]
        [(bool? cnst) (if (bool-b cnst) "true" "false")]
        [(str? cnst) (str-s cnst)] ; already have ""
        [(matrix? cnst) (string-append "[" (matrix-helper (matrix-data cnst)) "]")]
        [else (error 'Expr->Octave "Could not convert constant ~a" cnst)]))

(define (Expr->Octave expr)
  (cond [(id? expr) (symbol->string (id-name expr))]
        [(Constant? expr) (Constant->Octave expr)]
        [(app? expr) (string-append (Expr->Octave (app-fun expr)) "(" (string-join (map Expr->Octave (app-args expr)) ", ") ")")]
        [(int-binop? expr) (string-append (Expr->Octave (int-binop-lhs expr)) (symbol->string (int-binop-name expr)) (Expr->Octave (int-binop-rhs expr)))]
        [(bool-binop? expr) (string-append (Expr->Octave (bool-binop-lhs expr)) (symbol->string (bool-binop-name expr)) (Expr->Octave (bool-binop-rhs expr)))]
        [(int-compop? expr) (string-append (Expr->Octave (int-compop-lhs expr)) (symbol->string (int-compop-name expr)) (Expr->Octave (int-compop-rhs expr)))]
        [(string-compop? expr) (string-append (Expr->Octave (string-compop-lhs expr)) (symbol->string (string-compop-name expr)) (Expr->Octave (string-compop-rhs expr)))]
        [else (error 'Expr->Octave "Could not convert expression ~a" expr)]))

(define (not-mt-str s)
  (not (string=? "" s)))

(define (Stmt->Octave stmt depth)
  (cond [(Expr? stmt) (string-append
                       (spacing depth)
                       (Expr->Octave stmt))]
        [(decl? stmt) ""] ; ignore
        [(assn? stmt) (string-append
                       (spacing depth)
                       (if (> (length (assn-vars stmt)) 1)
                           (string-append
                            "["
                            (string-join (map symbol->string (assn-vars stmt)) ", ")
                            "]")
                           (symbol->string (first (assn-vars stmt))))
                       " = "
                       (Expr->Octave (assn-expr stmt)))]
        [(func? stmt) (string-append
                       (spacing depth)
                       "function "
                       (if (> (length (func-rets stmt)) 1)
                           (string-append
                            "["
                            (string-join (reverse (map (lambda (r) (symbol->string (car r))) (func-rets stmt))) ", ")
                            "]"
                            )
                           (symbol->string (first (func-rets stmt))))
                       " = "
                       (symbol->string (func-name stmt))
                       "("
                       (string-join (reverse (map (lambda (a) (symbol->string (car a))) (func-args stmt)))  ", ")
                       ")\n"
                       (string-join (filter not-mt-str (map (lambda (stmt) (Stmt->Octave stmt (add1 depth))) (func-body stmt))) "\n")
                       "\nendfunction")]
        [(if-stmt? stmt) (string-append
                          (spacing depth)
                          "if ("
                          (Expr->Octave (if-stmt-cond stmt))
                          ")\n"
                          (string-join (filter not-mt-str (map (lambda (stmt) (Stmt->Octave stmt (add1 depth))) (if-stmt-then stmt))) "\n")
                          "\n"
                          (if (not (empty? (if-stmt-else stmt)))
                              (string-append
                               (spacing depth)
                               "else\n"
                               (string-join (filter not-mt-str (map (lambda (stmt) (Stmt->Octave stmt (add1 depth))) (if-stmt-else stmt))) "\n"))
                               ""
                              ))]
        [else (error 'Stmt->Octave "Could not convert statement: ~a" stmt)]))

(define (Pgrm->Octave pgrm)
  (string-join (filter not-mt-str (map (lambda (stmt) (Stmt->Octave stmt 0)) pgrm)) "\n"))

(define (show-octave-code pgrm)
  (display (Pgrm->Octave pgrm)))
