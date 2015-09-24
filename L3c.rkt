#lang racket
(require "L3c-lib.rkt")

(define id 0)
(define used-labels '())

(define (new-label label)
  (set! id (+ id 1))
  (let ((ret (string->symbol (string-append (symbol->string label) (number->string id)))))
    (if (member ret used-labels)
        (new-label label)
        ret)))

(define (encode var) (if (integer? var) (+ 1 (* 2 var)) var))

; put every argument in a var
(define (initialize-func var-list)
  (let ((lines empty))
    (for ([var var-list] 
          [reg '(rdi rsi rdx rcx r8 r9)])
      (set! lines (append lines `((,var <- ,reg)))))
    (if (> (length var-list) 6)
        (for ([i (build-list (- (length var-list) 6) values)]
              [var (reverse (list-tail var-list 6))])
          (set! lines (append lines `((,var <- (stack-arg ,(* 8 i)))))))
        (void))
    lines))

;; compile different d

(define (compile-biop biop a b the-var)
  (case biop
    [(+) (let ((tttmp (new-label 'tmp)))
           `((,tttmp <- ,(encode a)) (,tttmp += ,(encode b)) (,tttmp -= 1) (,the-var <- ,tttmp)))]
    [(-) (let ((tttmp (new-label 'tmp)))
           `((,tttmp <- ,(encode a)) (,tttmp -= ,(encode b)) (,tttmp += 1) (,the-var <- ,tttmp)))]
    [(*) (let ((tttmp (new-label 'tmp)))
           `((,tttmp <- ,(encode a)) (,tttmp >>= 1) (,the-var <- ,(encode b)) (,the-var >>= 1)
                                     (,the-var *= ,tttmp) (,the-var <<= 1) (,the-var += 1)))]
    [(<) `((,the-var <- ,(encode a) < ,(encode b)) (,the-var <<= 1) (,the-var += 1))]
    [(<=) `((,the-var <- ,(encode a) <= ,(encode b)) (,the-var <<= 1) (,the-var += 1))]
    [(=) `((,the-var <- ,(encode a) = ,(encode b)) (,the-var <<= 1) (,the-var += 1))]))


(define (compile-pred pred val the-var)
  (case pred
    [(a?) `((,the-var <- ,(encode val)) (,the-var &= 1) (,the-var *= -2) (,the-var += 3))]
    [(number?) `((,the-var <- ,(encode val)) (,the-var &= 1) (,the-var *= 2) (,the-var += 1))]))

; pass args in registers and stack, then call the function
(define (compile-application func args the-var tail?)
  (define arg-len (length args))
  (define lines empty)
  (for ([arg args] 
        [reg '(rdi rsi rdx rcx r8 r9)])
    (set! lines (append lines `((,reg <- ,(encode arg))))))
  (if (> arg-len 6)
      (for ([i (build-list (- arg-len 6) values)]
            [arg (list-tail args 6)])
        (set! lines (append lines `(((mem rsp ,(- -16 (* 8 i))) <- ,(encode arg))))))
      (void))
  `(,@lines ,(if tail? `(tail-call ,func ,arg-len) `(call ,func ,arg-len))))


(define (compile-array size val the-var)
  `((rdi <- ,(encode size)) (rsi <- ,(encode val)) (call allocate 2) (,the-var <- rax)))


(define (compile-tuple var-list the-var)
  (define len (length var-list))
  (define lines (map (lambda (i var) `((mem rax ,(+ 8 (* i 8))) <- ,(encode var)))
                     (build-list len values) var-list))
  `((rdi <- ,(encode len)) (rsi <- 1) (call allocate 2) ,@lines (,the-var <- rax)))


(define (compile-aref arr index the-var)
  (let ((bound-pass (new-label ':bpass))
        (bound-fail (new-label ':bfail))
        (bound-zero-check (new-label ':bzcheck))
        (tmp (new-label 'tmp))
        (tttmp (new-label 'tmp)))
    ;; compiled code below
    `((,tmp <- ,(encode index)) 
      (,tmp >>= 1) 
      (,tttmp <- (mem ,arr 0))
      (cjump ,tmp < ,tttmp ,bound-zero-check ,bound-fail)
      ,bound-zero-check
      (cjump -1 < ,tmp ,bound-pass ,bound-fail)
      ,bound-fail
      (rdi <- ,arr) 
      (rsi <- ,(encode index))
      (call array-error 2)
      ,bound-pass
      (,tmp *= 8) 
      (,tmp += ,arr)
      (,the-var <- (mem ,tmp 8)))))



(define (compile-aset arr index val the-var)
  (let ((bound-pass (new-label ':bpass))
        (bound-fail (new-label ':bfail))
        (bound-zero-check (new-label ':bzcheck))
        (tmp (new-label 'tmp))
        (tttmp (new-label 'tmp)))
    ;; compiled code below
    `((,tmp <- ,(encode index)) 
      (,tmp >>= 1) 
      (,tttmp <- (mem ,arr 0))
      (cjump ,tmp < ,tttmp ,bound-zero-check ,bound-fail)
      ,bound-zero-check
      (cjump -1 < ,tmp ,bound-pass ,bound-fail)
      ,bound-fail
      (rdi <- ,arr) 
      (rsi <- ,(encode index))
      (call array-error 2)
      ,bound-pass
      (,tmp *= 8) 
      (,tmp += ,arr)
      ((mem ,tmp 8) <- ,(encode val)) 
      (,the-var <- 1))))



; given the d, write a list of instructions that set its result to var
; these should be continuous and under a single label
; nothing like tail-call or return
(define (compile-d the-d the-var tail?)
  (define body-lines
    (match the-d
      [(? v? val) `((,the-var <- ,(encode val)))]
      [`(,(? biop? biop) ,(? v? a) ,(? v? b)) (compile-biop biop a b the-var)]
      [`(,(? pred? pred) ,(? v? val)) (compile-pred pred val the-var)]
      [`(,(? v? x) ...) (compile-application (car the-d) (cdr the-d) the-var tail?)]
      [`(new-array ,(? v? size) ,(? v? val)) (compile-array size val the-var)]
      [`(new-tuple ,(? v? a) ...) (compile-tuple (cdr the-d) the-var)]
      [`(aref ,(? v? arr) ,(? v? index)) (compile-aref arr index the-var)]
      [`(aset ,(? v? arr) ,(? v? index) ,(? v? val)) (compile-aset arr index val the-var)]
      [`(alen ,(? v? arr)) `((,the-var <- (mem ,arr 0)) (,the-var <<= 1) (,the-var += 1))]
      [`(print ,(? v? val)) `((rdi <- ,(encode val)) (call print 1) (,the-var <- rax))]
      [`(make-closure ,(? label? label) ,(? v? val)) (compile-tuple (list label val) the-var)]
      [`(closure-proc ,(? v? clo)) `((,the-var <- (mem ,clo 8)))]
      [`(closure-vars ,(? v? clo)) `((,the-var <- (mem ,clo 16)))]
      [else (error 'compile-d "wrong d expression ~a" the-d)]))
  (if (and tail? (not (app? the-d)))
      `(,@body-lines (return))
      body-lines))



; always add a return at the end, or do tail-call if the last is a call
; form: let, if, or just d
(define (compile-e the-e)
  (match the-e
    [(? d? the-e) 
     (if (and (app? the-e) (> (length the-e) 7))
         (let ((ret-label (new-label ':retlbl)))
           `(((mem rsp -8) <- ,ret-label) 
             ,@(compile-d the-e 'rax #f)
             ,ret-label (return)))
         (compile-d the-e 'rax #t))]
    [`(if ,(? v? v) ,(? e? then) ,(? e? else))
     (let ((label-then (new-label ':then))
           (label-else (new-label ':else)))
       `((cjump ,(encode v) = 1 ,label-else ,label-then)
         ,label-then ,@(compile-e then)
         ,label-else ,@(compile-e else)))]
    [`(let ((,(? var? var) ,(? d? the-d))) ,(? e? e))
     (if (app? the-d)
         (let ((ret-label (new-label ':retlbl)))
           `(((mem rsp -8) <- ,ret-label) 
             ,@(compile-d the-d var #f) 
             ,ret-label (,var <- rax)
             ,@(compile-e (caddr the-e))))
         `(,@(compile-d the-d var #f)
           ,@(compile-e (caddr the-e))))]))


(define (compile-main main main-label)
  `(,main-label 0 0 ,@(compile-e main)))

(define (compile-func the-func)
  (define func-name (car the-func))
  (define start-lines (initialize-func (cadr the-func)))
  `(,func-name ,(length (cadr the-func)) 0 ,@start-lines ,@(compile-e (caddr the-func))))

(define (3-compile the-code)
  (define main-body (car the-code))
  (define the-funcs (cdr the-code))
  (set! id 0)
  (set! used-labels (map car the-funcs))
  (define main-label (new-label ':main))
  `(,main-label ,(compile-main main-body main-label) ,@(map compile-func the-funcs)))


(define (print-code code)
  (printf "(~a\n" (car code))
  (for ([func (cdr code)])
    (printf " (~a ~a ~a\n" (car func) (cadr func) (caddr func))
    (for ([line (cdddr func)])
      (printf "  ~a\n" line))
    (printf " )\n"))
  (printf ")\n"))






(define file-name ;"03.L3")
  (car (vector->list (current-command-line-arguments))))
(define in-port (open-input-file file-name))
(define the-code (read in-port))
(close-input-port in-port)

(define compiled-code (3-compile the-code))

(with-output-to-file "prog.L2"
  (lambda () (print-code compiled-code))
  #:exists 'replace)

;(print-code compiled-code)