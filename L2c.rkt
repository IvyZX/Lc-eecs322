#lang racket
(require "L2c-lib.rkt")
(require "spill.rkt")
(require "liveness-graph.rkt")
(require "graph.rkt")


(define (reg-for-var var allocation)
  (if (var? var)
      (if allocation
          (if (equal? var (caar allocation))
              (cdar allocation)
              (reg-for-var var (cdr allocation)))
          (error 'reg-for-var "could not register allocate"))
      var))

(define (sub-vars-for-line line alloc loc-arg-num)
  (match line
    [`(,(? w? w) <- ,(? s? s)) `(,(reg-for-var w alloc) <- ,(reg-for-var s alloc))]
    [`(,(? w? w) <- (mem ,(? x? x) ,offset)) `(,(reg-for-var w alloc) <- (mem ,(reg-for-var x alloc) ,offset))]
    [`((mem ,(? x? x) ,offset) <- ,(? s? s)) `((mem ,(reg-for-var x alloc) ,offset) <- ,(reg-for-var s alloc))]
    [`(,(? w? w) ,(? aop=? aop) ,(? t? t)) `(,(reg-for-var w alloc) ,aop ,(reg-for-var t alloc))]
    [`(,(? w? w) ,(? sop=? sop) ,(? sx? sx)) `(,(reg-for-var w alloc) ,sop ,(reg-for-var sx alloc))]
    [`(,(? w? w) ,(? sop=? sop) ,(? integer? sx)) `(,(reg-for-var w alloc) ,sop ,sx)]
    [`(,(? w? w) <- ,(? t? a) ,(? cmp? cmp) ,(? t? b)) 
     `(,(reg-for-var w alloc) <- ,(reg-for-var a alloc) ,cmp ,(reg-for-var b alloc))]
    [(? label? l) l]
    [`(goto ,(? label? l)) `(goto ,l)]
    [`(cjump ,(? t? a) ,(? cmp? cmp) ,(? t? b) ,(? label? then) ,(? label? else))
     `(cjump ,(reg-for-var a alloc) ,cmp ,(reg-for-var b alloc) ,then ,else)]
    [`(call ,(? built-in-func? f) ,nat) `(call ,f ,nat)]
    [`(call ,(? u? u) ,nat) `(call ,(reg-for-var u alloc) ,nat)]
    [`(tail-call ,(? u? u) ,nat) `(tail-call ,(reg-for-var u alloc) ,nat)]
    [`(return) `(return)]
    [`(,(? w? w) <- (stack-arg ,nat)) `(,(reg-for-var w alloc) <- (mem rsp ,(+ nat (* loc-arg-num 8))))]))


(define (sub-vars-with-regs the-func allocation)
  (if allocation
      `(,(car the-func) 
        ,(cadr the-func) 
        ,(caddr the-func) 
        ,@(map (lambda (n) (sub-vars-for-line n allocation (caddr the-func))) (cdddr the-func)))
      (format "could not register allocate ~a" (car the-func))))


(define (2-compile-func the-func)
  (define graph-result (graph the-func))
  (define allocation (cdr graph-result))
  (if allocation
      (sub-vars-with-regs the-func allocation)
      (let* ((var-graph (sort (filter (lambda (g) (not (member (car g) regs))) (car graph-result))
                              (lambda (a b) (> (length a) (length b)))))
             (spilled-func the-func))
        (do ([vars (map car var-graph) (cdr vars)])
          ((or (empty? vars) allocation) spilled-func)
          (set! spilled-func (spill spilled-func (car vars) (symbol->string (car vars))))
          (set! allocation (cdr (graph spilled-func))))
        (sub-vars-with-regs spilled-func allocation))))


(define (2-compile the-code)
  (define funcs (map 2-compile-func (cdr the-code)))
  (define error #f)
  (for ([f funcs])
    (if (string? f) (set! error f) (void)))
  (if error
      error
      (cons (car the-code) (map 2-compile-func (cdr the-code)))))



(define (print-code code)
  (printf "(~a\n" (car code))
  (for ([func (cdr code)])
    (printf " (~a ~a ~a\n" (car func) (cadr func) (caddr func))
    (for ([line (cdddr func)])
      (printf "  ~a\n" line))
    (printf " )\n"))
  (printf ")\n"))



(define file-name ;"41.L2")
  (car (vector->list (current-command-line-arguments))))
(define in-port (open-input-file file-name))
(define the-code
  (do ((code (read in-port) (read in-port)))
    ((not (empty? code)) code)
    (void)))
(define compiled-code (2-compile the-code))
(close-input-port in-port)

(with-output-to-file "prog.L1"
  (lambda () (if (string? compiled-code) (print compiled-code) (print-code compiled-code)))
  #:exists 'replace)

;(print compiled-code)
