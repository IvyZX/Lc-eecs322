#lang racket
; the library for 5-compiler functions
(provide (all-defined-out))

;; command checks
(define (biop? this) (member this '(+ - * < <= =)))
(define (pred? this) (member this '(number? a?)))
(define (prim? this) 
  (or (biop? this) (pred? this) (member this '(print new-array aref aset alen begin))))

(define (5e? this)
  (match this
    [(? prim?) 'prim]
    [(? var?) 'var]
    [(? num?) 'num]
    [`(lambda (,(? var? vars) ...) ,(? 5e? func-body)) 'lambda]
    [`(let ([,(? var? x) ,(? 5e? val)]) ,(? 5e? body)) 'let]
    [`(letrec ([,(? var? x) ,(? 5e? val)]) ,(? 5e? body)) 'letrec]
    [`(if ,(? 5e? cond) ,(? 5e? then) ,(? 5e? else)) 'if]
    [`(new-tuple ,(? 5e? cond) ...) 'new-tuple]
    [`(,(? prim? prim-func) ,(? 5e? args) ...) 'prim]
    [`(,(? 5e? func) ,(? 5e? args) ...) 'app]))


(define (4e? this)
  (match this
    [(? var? v) 'var]
    [(? label? l) 'label]
    [(? num? this) 'num]
    [`(let ([,(? var? var) ,(? 4e? val)]) ,(? 4e? exec)) 'let]
    [`(if ,(? 4e? cond) ,(? 4e? then) ,(? 4e? else)) 'if]
    [`(,(? biop? biop) ,(? 4e? a) ,(? 4e? b)) 'built-in]
    [`(,(? pred? pred) ,(? 4e? val)) 'built-in]
    [`(new-array ,(? 4e? size) ,(? 4e? init)) 'built-in]
    [`(new-tuple ,(? 4e? first) ...) 'built-in]
    [`(aref ,(? 4e? arr) ,(? 4e? index)) 'built-in]
    [`(aset ,(? 4e? arr) ,(? 4e? index) ,(? 4e? val)) 'built-in]
    [`(alen ,(? 4e? arr)) 'built-in]
    [`(begin ,(? 4e? first) ,(? 4e? second)) 'begin]
    [`(print ,(? 4e? val)) 'built-in]
    [`(make-closure ,(? label? l) ,(? 4e? val)) 'built-in]
    [`(closure-proc ,(? 4e? clo)) 'built-in]
    [`(closure-vars ,(? 4e? clo)) 'built-in]
    [`(,(? 4e? func) ...) 'app]
    [else #f]))


(define (num? this) (integer? this))
(define (label? this)
  (and (symbol? this) 
       (regexp-match #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$" (symbol->string this))))
(define (var? this) 
  (and (symbol? this) (not (member this all-regs)) (not (member this 4-built-ins))
       (regexp-match #rx"^[a-zA-Z_][a-zA-Z_0-9-]*$" (symbol->string this))))


; constants

(define (prim-lambda prim)
  (case prim
    [(+) `(lambda (x y) (+ x y))]
    [(-) `(lambda (x y) (- x y))]
    [(*) `(lambda (x y) (* x y))]
    [(<) `(lambda (x y) (< x y))]
    [(<=) `(lambda (x y) (<= x y))]
    [(=) `(lambda (x y) (= x y))]
    [(number?) `(lambda (x) (number? x))]
    [(a?) `(lambda (x) (a? x))]
    [(print) `(lambda (x) (print x))]
    [(new-array) `(lambda (size val) (new-array size val))]
    [(aref) `(lambda (arr i) (aref arr i))]
    [(aset) `(lambda (arr i val) (aset arr i val))]
    [(alen) `(lambda (arr) (alen arr))]
    [(begin) `(lambda (x y) (begin x y))]))



(define all-regs '(r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi rsp))
(define 4-built-ins '(print new-array new-tuple aref aset alen begin make-closure closure-proc closure-vars))