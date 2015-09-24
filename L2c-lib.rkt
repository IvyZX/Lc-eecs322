#lang racket
; the library for 2-compiler functions
(provide (all-defined-out))

;; type checks
;; implement type check
(define (aop=? this) (member this '(+= -= *= &=)))
(define (sop=? this) (member this '(<<= >>=)))
(define (cmp? this) (member this '(< <= =)))
(define (u? this) (or (label? this) (x? this)))
(define (t? this) (or (integer? this) (x? this)))
(define (s? this) (or (integer? this) (label? this) (x? this)))
(define (x? this) (or (equal? this 'rsp) (w? this)))
(define (w? this) (or (member this '(rax rbx rbp r10 r11 r12 r13 r14 r15)) (a? this)))
(define (a? this) (or (member this '(rdi rsi rdx r8 r9))(sx? this)))
(define (sx? this) (or (equal? this 'rcx) (var? this)))
(define (label? this) 
  (and (symbol? this) (regexp-match #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$" (symbol->string this))))
(define (var? this) 
  (and (symbol? this) (not (all-regs? this)) (regexp-match #rx"^[a-zA-Z_][a-zA-Z_0-9-]*$" (symbol->string this))))
(define (built-in-func? this)
  (and (symbol? this) (member this '(print allocate array-error))))

;;record reg sets
(define arg-regs '(r9 r8 rcx rdx rsi rdi))
(define callee-save-regs '(r12 r13 r14 r15 rbp rbx))
(define caller-save-regs '(r10 r11 r8 r9 rax rcx rdi rdx rsi))
(define regs '(r10 r11 r12 r13 r14 r15 r8 r9 rax rbx rbp rcx rdi rdx rsi))
(define all-regs '(r10 r11 r12 r13 r14 r15 r8 r9 rax rbx rbp rcx rdi rdx rsi rsp))
(define (all-regs? x) (member x all-regs))
