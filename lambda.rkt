
#lang plai-typed

;;-----------------------------------------------------------------------------------------------------------------------------------------------------
;; λ-expression grammar
;; λ-exp -> v
;; λ-exp -> (λ-exp λ-exp)
;; λ-exp -> (λ v λ-exp)
;; v is a symbol.
;; λ-exp is a parse tree definition
(define-type λ-exp
(λ-sym (v : symbol))
(λ-app (l : λ-exp)(r : λ-exp))
(λ-def (v : symbol)(p : λ-exp))
)
;; Tests:
(λ-sym 'x)
(λ-app (λ-sym 'x)(λ-sym 'y))
(λ-def 'v (λ-app (λ-sym 'x)(λ-sym 'y)))
;; parse : s-exp -> λ-exp
;; Reason : Transform given s-expression
(define (λparser (sexp : s-expression)) : λ-exp
(cond
[(s-exp-symbol? sexp)(λ-sym (s-exp->symbol sexp))]
[(s-exp-list? sexp)
(let ([sexp-list (s-exp->list sexp)])
(cond
[(= 2 (length sexp-list))
(λ-app (λparser (first sexp-list))(λparser (second sexp-list)))]
[(= 3 (length sexp-list))
(if (and (symbol=? 'λ (s-exp->symbol (first sexp-list)))
(s-exp-symbol? (second sexp-list)))
(λ-def (s-exp->symbol(second sexp-list))
(λparser (third sexp-list)))
(error 'λparser "Not valid λ-definition")
)]
[else (error 'λparser "Not valid length λ-exp")]
))]
[else (error 'λparser "Not valid λ-exp")]
))
;; unionOfSets : (listof symbol) (listof symbol) -> (listof symbol)
;; Reason : Find the union of two sets.
(define (unionOfSets (s1 : (listof symbol)) (s2 : (listof symbol))) : (listof symbol)
(foldr (lambda (x y)
(if (member x y)
y
(cons x y)))
empty
(append s1 s2)))
;; Tests:
(test (unionOfSets empty empty) empty)
(test (unionOfSets empty (list 'x)) (list 'x))
(test (unionOfSets (list 'x)(list 'x 'y)) (list 'x 'y))
(test (unionOfSets (list 'x) (list 'x)) (list 'x))
;; differenceOfSets : (listof symbol) (listof symbol) -> (listof symbol)
;; Reason : To find the set difference of two sets.
(define (differenceOfSets (s1 : (listof symbol)) (s2 : (listof symbol))) : (listof symbol)
(filter (lambda (x)
(not (member x s2)))
s1))
;; Tests:
(test (differenceOfSets empty (list 'x)) empty)
(test (differenceOfSets (list 'x) empty) (list 'x))
(test (differenceOfSets (list 'x)(list 'x 'y)) empty)
(test (differenceOfSets (list 'x 'y)(list 'x))(list 'y))
;; freeIdentifier : λ-exp -> (listof symbol)
;; Reason : Find free identifiers in λ expression.
(define (freeIdentifier (le : λ-exp)) : (listof symbol)
(type-case λ-exp le
(λ-sym (v) (list v))
(λ-app (l r)(unionOfSets
(freeIdentifier l)
(freeIdentifier r)))
(λ-def (v p)(differenceOfSets (freeIdentifier p)
(list v)))
))
;; Tests:
(test (freeIdentifier (λparser (symbol->s-exp 'x))) (list 'x))
(test (freeIdentifier (λparser '(λ x x))) empty)
(test (freeIdentifier (λparser '(λ y y))) empty)
(test (freeIdentifier (λparser '(λ x y))) empty)
(test (freeIdentifier (λparser '(λ x y))) (list 'y))
(test (freeIdentifier (λparser '((λ x y)(λ y z)))) (list 'y 'z))
(test (freeIdentifier (λparser '((λ f y)(λ y z)))) (list 'y 'z))
(test (freeIdentifier (λparser '((λ f y)(λ z z)))) (list 'y))
(test (freeIdentifier (λparser '(λ x (λ y (y x))))) empty)
(test (freeIdentifier (λparser '(λ y (λ y (y x))))) empty)
(test (freeIdentifier (λparser '(λ x (λ y z)))) (list 'z))
(test (freeIdentifier (λparser '(λ y (λ y z)))) (list 'z))