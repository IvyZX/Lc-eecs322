#lang racket
;; this is the library version, used for graph
(provide (all-defined-out))
(require "L2c-lib.rkt")

(define (find-out-of index in succ-list)
  (let ([succs (list-ref succ-list index)])
    (sort (remove-duplicates 
           (apply append (map (lambda (s) (list-ref in s)) succs))) symbol<?)))

(define (find-in-of index out gen-list kill-list)
  (let ((line-gen (list-ref gen-list index))
        (line-kill (list-ref kill-list index))
        (line-out (list-ref out index)))
    (sort (remove-duplicates (append line-gen (remove* line-kill line-out))) symbol<?)))

;; helper functions
(define (gen index func-body)
  (filter 
   (lambda (n) (and (symbol? n) (not (label? n)) (not (equal? n 'rsp))))
   (match (list-ref func-body index)
     [`(,(? w? w) <- ,(? s? s)) (list s)]
     [`(,(? w? w) <- (mem ,(? x? x) ,offset)) (list x)]
     [`((mem ,(? x? x) ,offset) <- ,(? s? s)) (list s x)]
     [`(,(? w? w) ,(? aop=? aop) ,(? t? t)) (list w t)]
     [`(,(? w? w) ,(? sop=? sop) ,(? sx? sx)) (list w sx)]
     [`(,(? w? w) ,(? sop=? sop) ,(? integer? sx)) (list w)]
     [`(,(? w? w) <- ,(? t? a) ,(? cmp? cmp) ,(? t? b)) (list a b)]
     [(? label? l) empty]
     [`(goto ,(? label? l)) empty]
     [`(cjump ,(? t? a) ,(? cmp? cmp) ,(? t? b) ,(? label? then) ,(? label? else)) (list a b)]
     [`(call ,(? built-in-func? f) ,nat) (list-tail arg-regs (- 6 (min nat 6)))]
     [`(call ,(? u? u) ,nat) `(,u ,@(list-tail arg-regs (- 6 (min nat 6))))]
     [`(tail-call ,(? u? u) ,nat) `(,u ,@(list-tail arg-regs (- 6 (min nat 6))) ,@callee-save-regs)]
     [`(return) `(rax ,@callee-save-regs)]
     [`(,(? w? w) <- (stack-arg ,nat)) empty]
     [else (error 'liveness "unknown expression ~s" (list-ref func-body index))])))

; find the kill of this line
(define (kill index func-body)
  (filter 
   (lambda (n) (and (symbol? n) (not (label? n))))
   (match (list-ref func-body index)
     [`(,(? w? w) <- ,(? s? s)) (list w)]
     [`(,(? w? w) <- (mem ,(? x? x) ,offset)) (list w)]
     [`((mem ,(? x? x) ,offset) <- ,(? s? s)) empty]
     [`(,(? w? w) ,(? aop=? aop) ,(? t? t)) (list w)]
     [`(,(? w? w) ,(? sop=? sop) ,(? sx? sx)) (list w)]
     [`(,(? w? w) ,(? sop=? sop) ,(? integer? sx)) (list w)]
     [`(,(? w? w) <- ,(? t? a) ,(? cmp? cmp) ,(? t? b)) (list w)]
     [(? label? l) empty]
     [`(goto ,(? label? l)) empty]
     [`(cjump ,(? t? a) ,(? cmp? cmp) ,(? t? b) ,(? label? then) ,(? label? else)) empty]
     [`(call ,(? built-in-func? f) ,nat) `(rax ,@caller-save-regs)]
     [`(call ,(? u? u) ,nat) `(rax ,@caller-save-regs)]
     [`(tail-call ,(? u? u) ,nat) empty]
     [`(return) empty]
     [`(,(? w? w) <- (stack-arg ,nat)) (list w)]
     [else (error 'liveness "unknown expression ~s" (list-ref func-body index))])))


; return a list of lines that is after of the line with this index
(define (succ index func-body func-size)
  (match (list-ref func-body index)
    [`(goto ,(? label? l)) (list (get-line-index l func-body func-size))]
    [`(cjump ,(? t? a) ,(? cmp? cmp) ,(? t? b) ,(? label? then) ,(? label? else)) 
     (list (get-line-index then func-body func-size) (get-line-index else func-body func-size))]
    [`(call array-error 2) empty]
    [`(tail-call ,(? u? u) ,nat) empty]
    [`(return) empty]
    [else (if (= index (- func-size 1)) empty (list (+ index 1)))]))

; return the index of the first 'line' in the function body
(define (get-line-index line func-body func-size)
  (- func-size (length (member line func-body))))


(define (print-liveness in out)
  (printf "((in")
  (for ([line in])
    (printf "\n  ~a" line))
  (printf ")\n (out")
  (for ([line out])
    (printf "\n  ~a" line))
  (printf "))\n"))


;; the main function
(define (liveness the-func)
  (define func-body (cdddr the-func))
  (define func-size (length func-body))
  (define gen-list (map (lambda (n) (gen n func-body)) (build-list func-size values)))
  (define kill-list (map (lambda (n) (kill n func-body)) (build-list func-size values)))
  (define succ-list (map (lambda (n) (succ n func-body func-size)) (build-list func-size values)))
  (define in (make-list func-size empty))
  (define out (make-list func-size empty))
  
  (let ([new-in (make-list func-size empty)]
        [new-out (make-list func-size empty)]
        [same-in #f]
        [same-out #f])
    (do () ((and same-in same-out) empty)
      (set! same-in #f)
      (set! same-out #f)
      (set! new-in (map (lambda (n) (find-in-of n out gen-list kill-list))
                        (build-list func-size values)))
      (if (equal? in new-in) (set! same-in #t) (set! in new-in))
      (set! new-out (map (lambda (n) (find-out-of n in succ-list))
                         (build-list func-size values)))
      (if (equal? out new-out) (set! same-out #t) (set! out new-out))))
  (cons in out))

;(if (empty? (vector->list (current-command-line-arguments)))
;    (void)
;    (with-input-from-file (string->path (car (vector->list (current-command-line-arguments))))
;      (lambda () (liveness (read) #t))))
                        

;(let ((file-name (car (vector->list (current-command-line-arguments)))))
;  (with-output-to-file (string->path (format "~a.lres" (substring file-name 0 2)))
;    (lambda () (liveness file-name))
;    #:exists 'replace))





