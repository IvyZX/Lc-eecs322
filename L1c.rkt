#lang racket
;; by Ivy Zheng
;; compile a L1 file to a x86 assembly file
(require "L1c-lib.rkt")

(define prefix1
  "          .text\n          .globl go \ngo:\n          pushq   %rbx\n          pushq   %rbp
          pushq   %r12\n          pushq   %r13\n          pushq   %r14\n          pushq   %r15\n          call ")

(define prefix2
  "\n          popq   %r15\n          popq   %r14\n          popq   %r13\n          popq   %r12
          popq   %rbp\n          popq   %rbx\n          retq\n")

;; parenthsis-list -> string
(define (parse-line text func-space)
  (match text
    [(? label?) (string-append (label-name text) ":\n")]
    [`(,head <- ,tail) (string-append "\tmovq " (mem-line tail) ", " (mem-line head) "\n")]
    [`(,(? w? head) ,(? aop=? aop) ,(? t? tail)) 
     (string-append (aop-prefix aop) (reg-name tail) ", " (reg-name head) "\n")]
    [`(,(? w? head) ,(? sop=? sop) ,tail)
     (string-append (sop-prefix sop) (reg-name-8 tail) ", " (reg-name head) "\n")]
    [`(,(? w? dist) <- ,(? t? cmpa) ,(? cmp? cmp) ,(? t? cmpb)) (do-mov-cmp dist cmpa cmp cmpb)]
    [`(goto ,(? label? label)) (string-append "\tjmp " (label-name label) "\n")]
    [`(cjump ,(? t? cmpa) ,(? cmp? cmp) ,(? t? cmpb) ,(? label? yes) ,(? label? no))
     (do-jmp-cmp cmpa cmp cmpb yes no)]
    [`(call ,(? u? func) ,(? integer? args))
     (string-append "\tsubq " (reg-name (* 8 (+ 1 (if (> args 6) (- args 6) 0)))) ", %rsp\n"
                    "\tjmp " (if (label? func) (label-name func) (reg-name func)) "\n")]
    [`(call ,(? built-in-func? func) ,nat)
     (string-append "\tcall " (if (equal? func 'array-error) "array_error" (symbol->string func)) "\n")]
    [`(return) (string-append "\taddq " (const-name func-space) ", %rsp\n"
                              "\tret\n")]
    [`(tail-call ,func ,args) (string-append "\taddq " (const-name func-space) ", %rsp\n"
                                             "\tjmp " (if (label? func) (label-name func) (reg-name func)) "\n")]
    [else (error 'parse "unknown line ~a" text)]))


(define (parse-func text)
  (match text
    [`(,func ,args ,locals ,stuff ...)
     (string-append (label-name func) ":\n"
                    "\tsubq " (reg-name (* 8 locals)) ", %rsp\n"
                    (let ((spill+locals (* 8 (+ locals (if (> args 6) (- args 6) 0)))))
                      (apply string-append (map (lambda (n) (parse-line n spill+locals)) stuff)))
                    )]))

(define (parse text)
  (string-append prefix1 (label-name (car text)) prefix2
                 (apply string-append (map parse-func (cdr text)))))



(define (do-mov-cmp dist cmpa cmp cmpb)
  (if (integer? cmpa)
      (if (integer? cmpb)
          (string-append "\tmovq " (const-name (if (num-compare cmpa cmp cmpb) 1 0)) ", " (reg-name dist) "\n")
          (string-append "\tcmpq " (reg-name cmpa) ", " (reg-name cmpb) "\n"
                         "\t" (find-cmp-set cmp 'g) (reg-name-8 dist) "\n"
                         "\tmovzbq " (reg-name-8 dist) ", " (reg-name dist) "\n"))
      (string-append "\tcmpq " (reg-name cmpb) ", " (reg-name cmpa) "\n"
                     "\t" (find-cmp-set cmp 'l) (reg-name-8 dist) "\n"
                     "\tmovzbq " (reg-name-8 dist) ", " (reg-name dist) "\n")))


(define (do-jmp-cmp cmpa cmp cmpb yes no)
  (if (integer? cmpa)
      (if (integer? cmpb)
          (if (num-compare cmpa cmp cmpb)
              (string-append "\tjmp " (label-name yes) "\n")
              (string-append "\tjmp " (label-name no) "\n"))
          (string-append "\tcmpq " (reg-name cmpa) ", " (reg-name cmpb) "\n"
                         "\t" (find-cmp-jmp cmp 'g) " " (label-name yes) "\n"
                         "\tjmp " (label-name no) "\n"))
      (string-append "\tcmpq " (reg-name cmpb) ", " (reg-name cmpa) "\n"
                     "\t" (find-cmp-jmp cmp 'l) " " (label-name yes) "\n"
                     "\tjmp " (label-name no) "\n")))


(define (aop-prefix aop)
  (case aop
    [(+=) "\taddq "]
    [(-=) "\tsubq "]
    [(*=) "\timulq "]
    [(&=) "\tandq "]))

(define (sop-prefix sop)
  (case sop
    [(<<=) "\tsalq "]
    [(>>=) "\tsarq "]))

(define (find-cmp-set cmp cmd)
  (case cmp
    ((<=) (if (equal? cmd 'l) "setle " "setge "))
    ((<) (if (equal? cmd 'l) "setl " "setg "))
    ((=) "sete ")))

(define (find-cmp-jmp cmp cmd)
  (case cmp
    ((<=) (if (equal? cmd 'l) "jle " "jge "))
    ((<) (if (equal? cmd 'l) "jl " "jg "))
    ((=) "je ")))

(define (num-compare cmpa cmp cmpb)
  (case cmp
    ((<=) (<= cmpa cmpb))
    ((<) (< cmpa cmpb))
    ((=) (= cmpa cmpb))))


(define (const-name text) 
  (string-append "$" 
                 (cond [(integer? text) (number->string text)]
                       [(label? text) (label-name text)])))

(define (reg-name reg-sym)
  (if (or (label? reg-sym) (integer? reg-sym))
      (const-name reg-sym)
      (string-append "%" (symbol->string reg-sym))))

(define (reg-name-8 reg-sym)
  (if (or (label? reg-sym) (integer? reg-sym))
      (const-name reg-sym)
      (case reg-sym
        [(r8 r9 r10 r11 r12 r13 r14 r15) (string-append "%" (symbol->string reg-sym) "b")]
        [(rax rbx rcx rdx) (string-append "%" (substring (symbol->string reg-sym) 1 2) "l")]
        [(rbp rdi rsi) (string-append "%" (substring (symbol->string reg-sym) 1 3) "l")]
        [else (error 'reg-name-8 "wrong reg here: ~a" reg-sym)])))

(define (mem-line text)
  (match text
    [(? integer?) (const-name text)]
    [(? reg?) (reg-name text)]
    [(? label?) (const-name text)]
    [`(mem ,reg ,offs) (string-append (number->string offs) "(" (reg-name reg) ")")]))

(define (label-name text)
  (string-append "_" (substring (symbol->string text) 1)))



;; Take input file and produce the .s output file

(define file-name (car (vector->list (current-command-line-arguments))))

(define (assm-name file-name)
  (string-append (substring file-name 0 (- (string-length file-name) 3)) ".s"))

(define in (open-input-file (string->path file-name)))
(define out (open-output-file (string->path "prog.s") 
                              #:mode 'binary 
                              #:exists 'replace))
(display (parse (read in)) out)
(close-input-port in)
(close-output-port out)
