#lang racket
; the library for 3-compiler functions
(provide (all-defined-out))

;; command checks
(define (biop? this) (member this '(+ - * < <= =)))
(define (pred? this) (member this '(number? a?)))


;; type checks
(define (v? this) (or (var? this) (label? this) (integer? this)))

(define (d? this)
  (match this
    [(? v? val) #t]
    [`(,(? biop? biop) ,(? v? first) ,(? v? second)) #t]
    [`(,(? pred? x) ,(? v? val)) #t]
    [`(,(? v? x) ...) #t]
    [`(new-array ,(? v? size) ,(? v? val)) #t]
    [`(new-tuple ,(? v? first) ...) #t]
    [`(aref ,(? v? arr) ,(? v? index)) #t]
    [`(aset ,(? v? arr) ,(? v? index) ,(? v? val)) #t]
    [`(alen ,(? v? arr)) #t]
    [`(print ,(? v? val)) #t]
    [`(make-closure ,(? label? label) ,(? v? val)) #t]
    [`(closure-proc ,(? v? clo)) #t]
    [`(closure-vars ,(? v? clo)) #t]
    [else #f]))

(define (3e? this)
  (match this
    [(? d? d) #t]
    [`(if ,(? v? cond) ,(? 3e? then) ,(? 3e? else)) #t]
    [`(let ([,(? var? var) ,(? d? val)]) ,(? 3e? exec)) #t]
    [else #f]))

(define (4e? this)
  (or (var? this) (label? this) (num? this)
      (match this
        [(? var? v) 'var]
        [(? label? l) 'label]
        [(? num? this) 'num]
        [`(let ([,(? var? var) ,(? 4e? val)]) ,(? 4e? exec)) 'let]
        [`(if ,(? 4e? cond) ,(? 4e? then) ,(? 4e? else)) 'if]
        [`(,(? biop? biop) ,(? 4e? a) ,(? 4e? b)) 'built-in]
        [`(,(? pred? pred) ,(? 4e? val)) 'built-in]
        [`(,(? 4e? func) ...) 'app]
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
        [else #f])))


(define (l3-func? func)
  (match func
    [`(,(? label? func-name) (,(? var? var1) ...) ,(? 3e? body)) #t]
    [else #f]))




(define (num? this) (integer? this))
(define (label? this)
  (and (symbol? this) 
       (regexp-match #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$" (symbol->string this))))
(define (var? this) 
  (and (symbol? this) (not (member this all-regs)) (not (member this 3-built-ins))
       (regexp-match #rx"^[a-zA-Z_][a-zA-Z_0-9-]*$" (symbol->string this))))

(define 3-built-ins
  '(new-array new-tuple aref aset alen a? number? print + - * < <= =
              make-closure closure-proc closure-vars))
(define (3-built-in? n) (member n 3-built-ins))

(define all-regs '(r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdi rdx rsi rsp))