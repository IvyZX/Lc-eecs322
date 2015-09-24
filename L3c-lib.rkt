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

(define (e? this)
  (match this
    [(? d? d) #t]
    [`(if ,(? v? cond) ,(? e? then) ,(? e? else)) #t]
    [`(let ([,(? var? var) ,(? d? val)]) ,(? e? exec)) #t]
    [else #f]))

(define (num? this) (integer? this))
(define (label? this)
  (and (symbol? this) 
       (regexp-match #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$" (symbol->string this))))
(define (var? this) 
  (and (symbol? this) (not (member this all-regs)) (not (member this built-in-funcs))
       (regexp-match #rx"^[a-zA-Z_][a-zA-Z_0-9-]*$" (symbol->string this))))

(define (app? this)
  (and (list? this)
       (equal? (filter v? this) this)))


;;record special sets
(define arg-regs '(r9 r8 rcx rdx rsi rdi))
(define all-regs '(r10 r11 r12 r13 r14 r15 r8 r9 rax rbp rbx rcx rdx rdi rsi rbp))
(define built-in-funcs
  '(new-array new-tuple aref aset alen a? number? print + - * < <= =
              make-closure closure-proc closure-vars))
