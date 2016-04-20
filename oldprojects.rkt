#lang plai-typed

;; Grammar:
;; S -> ± number
;; S -> + S S
;; S -> - S S
;; S -> * S S
;; S -> ^ S S
;; S -> S
;; S -> - S
;; S -> Symbol
;; Data Definition of msl expression
;; Alphabet: [+, *, (), **, -, number]
;; Data Definition of msl expression
(define-type msl
[num (n : number)]
[add (l : msl) (r : msl)]
[mult (l : msl) (r : msl)]
[sub (l : msl) (r : msl)]
[div (l : msl) (r : msl)]
[exp (l : msl) (r : msl)]
; [id (s : symbol)]
; [app (fun : symbol) (arg : msl)]
; [if-greater-than-zero (pred : msl)(trueS : msl)(falseS : msl)]
; [uminus (e : msl)]
)
;; Data Definition of mslSugar
(define-type mslS
[numS (n : number)]
[plusS (l : mslS) (r : mslS)]
[mulS (l : mslS) (r : mslS)]
[subS (l : mslS) (r : mslS)]
[expS (l : mslS) (r : mslS)]
[uminusS (e : mslS)])
;; Definition of Exponentiation
;; number -> number
;; examples
;; (^ 1 1) -> 1
;; (^ 2 2) -> 4
;; (^ 2 3) -> 8
;; (^ 2 4) -> 16
;; (^ 2 5) -> 32
;; (^ 2 6) -> 64
;; (^ 3 2) -> 9
;; (^ 3 4) -> 81
(define (^ base n)
(cond ((or (= base 1) (= n 0)) 1)
(else (* base (^ base (- n 1))))))
;; exponentiation function
(define(** u t)
(cond
((= u 1) t)
(else
(* t(**(sub1 u) t)))))
;; factorial function
(define (factorialFun n)
(cond ((< n 0) 1)
(else (* n (factorialFun(- n 1)))))
)
;; Tests
(num 8)
(add (num 4) (num 4))
(mult (num 4) (num 2))
(add (add (num 2) (num 2)) (num 20))
(mult (add (num 3) (num 2)) (num 5))
;; Eval Function
;; evaluate an msl expression
;; eval msl -> number
(define (eval [expr : msl])
(type-case msl expr
[num (n) n]
[add (l r) (+ (eval l) (eval r))] ; add function
[mult (l r) (* (eval l) (eval r))] ; mult function
[sub (l r) (- (eval l) (eval r))] ; sub function
[div (l r) (/ (eval l) (eval r))] ; div function
[exp (l r) (^ (eval l) (eval r))]
)
)
;;Test cases
(test (eval (num 7)) 7)
(test (eval (num 5)) 5)
(test (eval (add (num 3) (num 4))) 7)
(test (eval (add (add (num 3) (num 4)) (num 35))) 42)
(test (eval (exp (num 3) (num 3))) 27)
(test (eval (add (add (num -3) (num 4)) (num 35))) 36)
(test (eval (sub (num 5) (num 2))) 3)
(test (eval (div (num 8) (num 1))) 8)
(test (eval (add (div (num 20) (num 5)) (num 35))) 39)
;; DeSugar
(define (desugar [as : mslS]) : msl
(type-case mslS as
[numS (n) (num n)]
[plusS (l r) (add (desugar l) (desugar r))]
[mulS (l r) (mult (desugar l) (desugar r))]
[subS (l r) (sub (desugar l) (desugar r))]
[expS (l r) (exp (desugar l) (desugar r))]
[uminusS (e) (mult (num -1) (desugar e))]))
;; DeSugar Tests
(desugar (plusS (numS 30) (numS 40)))
(test (eval (desugar (plusS (numS 30) (numS 40)))) 70)
(eval (desugar (plusS (numS 30) (numS 40))))
(desugar (mulS (numS 3) (numS 3)))
(eval (desugar (mulS (numS 3) (numS 3))))
(test (eval (desugar (mulS (numS 3) (numS 3)))) 9)
(desugar (plusS (uminusS (numS 3)) (numS 5)))
(eval (desugar (subS (uminusS (numS 3)) (numS 5))))
(test (eval (desugar (subS (uminusS (numS 3)) (numS 5)))) -8)
(desugar (expS (numS 2) (numS 3)))
(eval (desugar (expS (numS 2) (numS 3))))
(test (eval (desugar (expS (numS 2) (numS 3)))) 8)
(desugar (subS (numS 6) (numS 2)))
(eval (desugar (subS (numS 6) (numS 2))))
(test (eval (desugar (subS (numS 6) (numS 2)))) 4)
;; Parser
(define (parse [s : s-expression]) : mslS
(cond
[(s-exp-number? s) (numS (s-exp->number s))]
[(s-exp-list? s)
(let ([sl (s-exp->list s)])
(cond
[(= (length sl) 3)
(case (s-exp->symbol (first sl))
[(+) (plusS (parse (second sl)) (parse (third sl)))]
[(*) (mulS (parse (second sl)) (parse (third sl)))]
[(-) (subS (parse (second sl)) (parse (third sl)))]
[(^) (expS (parse (second sl)) (parse (third sl)))]
[else (error 'parse "invalid list input")])]
[(= (length sl) 2)
(case (s-exp->symbol (first sl))
[(-) (uminusS (parse (second sl)))]
)
]
)
)
]
)
)
;; Prefix Parser
(define (parsePrefix [s : s-expression]) : msl
(cond
[(s-exp-number? s) (num (s-exp->number s))]
[(s-exp-list? s)
(let ([sl (s-exp->list s)])
(case (s-exp->symbol (first sl))
[(+) (add (parsePrefix (second sl)) (parsePrefix (third sl)))]
[(-) (sub (parsePrefix (second sl)) (parsePrefix (third sl)))]
[(*) (mult (parsePrefix (second sl)) (parsePrefix (third sl)))]
[(**) (exp (parsePrefix (second sl)) (parsePrefix (third sl)))]
[else (error 'parsePrefix "invalid list input")]))]
[else (error 'parsePrefix "invalid input")]
)
)
;; Infix Parser
(define (parseInfix [s : s-expression]) : msl
(cond
[(s-exp-number? s) (num (s-exp->number s))]
[(s-exp-list? s)
(let ([s1 (s-exp->list s)])
(case (s-exp->symbol (second s1))
[(+) (add (parseInfix (first s1)) (parseInfix (third s1)))]
[(-) (sub (parseInfix (first s1)) (parseInfix (third s1)))]
[(*) (mult (parseInfix (first s1)) (parseInfix (third s1)))]
[(**) (exp (parseInfix (first s1)) (parseInfix (third s1)))]
[else (error 'parseInfix "invalid list input")]))]
[else (error 'parseInfix "invalid input")]))
;; Output Reverse Polish
(define (reversePolishOutput [expr : msl])
(type-case msl expr
[num (n) (list (number->s-exp n))]
[add (l r) (append (append (reversePolishOutput l) (reversePolishOutput r)) (list (symbol->s-exp '+)))]
[mult(l r) (append (append (reversePolishOutput l) (reversePolishOutput r)) (list (symbol->s-exp '*)))]
[sub (l r) (append (append (reversePolishOutput l) (reversePolishOutput r)) (list (symbol->s-exp '-)))]
[div (l r) (append (append (reversePolishOutput l) (reversePolishOutput r)) (list (symbol->s-exp '/)))]
[exp (l r) (append (append (reversePolishOutput l) (reversePolishOutput r)) (list (symbol->s-exp '**)))]
)
)
;; Unparser Infix
; Function takes msl type object then unparses into infix s-expression.
(define (unparserInfix [expr : msl])
(type-case msl expr
[num (n) (list (number->s-exp n))]
(add (l r) (append (append (unparserInfix l) (list (symbol->s-exp '+))) (unparserInfix r)))
(mult (l r) (append (append (unparserInfix l) (list (symbol->s-exp '*))) (unparserInfix r)))
(sub (l r) (append (append (unparserInfix l) (list (symbol->s-exp '-))) (unparserInfix r)))
(div (l r) (append (append (unparserInfix l) (list (symbol->s-exp '/))) (unparserInfix r)))
(exp (l r) (append (append (unparserInfix l) (list (symbol->s-exp '**))) (unparserInfix r)))
)
)
(define (unparserPrefix [expr : msl])
(type-case msl expr
[num (n) (list (number->s-exp n))]
(add (l r) (append (list(symbol->s-exp '+)) (append (unparserPrefix l) (unparserPrefix r))))
(sub (l r) (append (list(symbol->s-exp '-)) (append (unparserPrefix l) (unparserPrefix r))))
(mult (l r) (append (list(symbol->s-exp '*)) (append (unparserPrefix l) (unparserPrefix r))))
(div (l r) (append (list(symbol->s-exp '/)) (append (unparserPrefix l) (unparserPrefix r))))
(exp (l r) (append (list(symbol->s-exp '**)) (append (unparserPrefix l) (unparserPrefix r))))
)
)
;;****************************************************************************************************
(define-type mslX
[Xnum (n : number)]
[Xid (s : symbol)]
[ifGreaterThanZero (pred : mslX)(trueState : mslX)(falseState : mslX)]
[Xapp (fun : symbol) (arg : mslX)]
[Xplus (l : mslX) (r : mslX)]
[Xmult (l : mslX) (r : mslX)])
(define (subst [what : number] [for : symbol] [in : mslX]) : mslX
(type-case mslX in
[Xnum (n) in]
[Xid (s) (cond
[(symbol=? s for) (Xnum what)]
[else in])]
[Xplus (l r) (Xplus (subst what for l)
(subst what for r))]
[Xmult (l r) (Xmult (subst what for l)
(subst what for r))]
[Xapp (f a) (Xapp f (subst what for a))]
[ifGreaterThanZero (p t f) (error 'sust "error")]))
(define (parseX [s : s-expression]) : mslX
(cond
[(s-exp-number? s) (Xnum (s-exp->number s))]
[(s-exp-symbol? s) (Xid (s-exp->symbol s))]
[(s-exp-list? s)
(let ([sl (s-exp->list s)])
(case (s-exp->symbol (first sl))
[(ifzero) (ifGreaterThanZero
(parseX (second sl))
(parseX (third sl))
(parseX (fourth sl)))]
[(+) (Xplus (parseX (second sl)) (parseX (third sl)))]
[(*) (Xmult (parseX (second sl)) (parseX (third sl)))]
[else (error 'parseX "invalid keyword !")]))]
))
(define-type FunDefC
[fdC (name : symbol) (arg : symbol) (body : mslX)])
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
(cond
[(empty? fds) (error 'get-fundef "reference to undefined function")]
[(cons? fds) (cond
[(equal? n (fdC-name (first fds))) (first fds)]
[else (get-fundef n (rest fds))])]))
(define (interp [e : mslX][fds : (listof FunDefC)]) : number
(type-case mslX e
[Xnum (n) n]
[Xid (_) (error 'interp "shouldn't get here")]
[ifGreaterThanZero (pred t f)
(if (< 0 (interp pred fds))
(interp t fds)
(interp f fds))]
[Xapp (f a) (local ([define fd (get-fundef f fds)])
(interp (subst
(interp a fds) ;; Make it eager evaluation !!
(fdC-arg fd)
(fdC-body fd))
fds))]
[Xplus (l r) (+ (interp l fds) (interp r fds))]
[Xmult (l r) (* (interp l fds) (interp r fds))]
))
