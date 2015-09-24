#lang racket
(require "L2c-lib.rkt")
(provide (all-defined-out))

(define count 0)

(define (sub prefix)
  (set! count (+ 1 count))
  (string->symbol (string-append prefix (number->string (- count 1)))))


(define (parse-line text var loc prefix)
  (match text
    [`(,(? w? w) <- ,(? s? s)) (do-w<-s w s var loc prefix)]
    [`(,(? w? w) <- (mem ,reg ,offset)) (do-w<-mem w reg offset var loc prefix)]
    [`((mem ,reg ,offset) <- ,(? s? s)) (do-mem<-s s reg offset var loc prefix)]
    [`(,(? w? w) ,(? aop=? aop) ,(? t? t)) (do-aop w aop t var loc prefix)]
    [`(,(? w? w) ,(? sop=? sop) ,(? sx? sx)) (do-sop w sop sx var loc prefix)]
    [`(,(? w? w) ,(? sop=? sop) ,(? integer? sx)) (do-sop w sop sx var loc prefix)]
    [`(,(? w? w) <- ,(? t? a) ,(? cmp? cmp) ,(? t? b)) (do-w<-cmp w a cmp b var loc prefix)]
    [`(cjump ,(? t? a) ,(? cmp? cmp) ,(? t? b) ,(? label? then) ,(? label? else)) 
     (do-cjmp a cmp b then else var loc prefix)]
    [`(call ,(? u? u) ,nat) (do-call u nat var loc prefix)]
    [`(tail-call ,(? u? u) ,nat) (do-tail-call u nat var loc prefix)]
    [`(,(? w? w) <- (stack-arg ,num)) (do-w<-stack w num var loc prefix)]
    [else (list text)]))

(define (do-w<-s w s var loc prefix)
  (if (equal? var w)
      (if (equal? var s) '()
        `(((mem rsp ,loc) <- ,s)))
      (if (equal? var s)
          `((,w <- (mem rsp ,loc)))
          `((,w <- ,s)))))

(define (do-w<-mem w reg offset var loc prefix)
  (if (equal? var reg)
      (let ((sub-var (sub prefix)))
        `((,sub-var <- (mem rsp ,loc))
          (,(if (equal? w var) sub-var w) <- (mem ,sub-var ,offset))
          ,@(if (equal? w var) `(((mem rsp ,loc) <- ,sub-var)) `())))
      (if (equal? var w)
          (let ((sub-var (sub prefix)))
            `((,sub-var <- (mem ,reg ,offset)) ((mem rsp ,loc) <- ,sub-var)))
          `((,w <- (mem ,reg ,offset))))))

(define (do-mem<-s s reg offset var loc prefix)
  (if (or (equal? var reg) (equal? var s))
      (let ((sub-var (sub prefix)))
        `((,sub-var <- (mem rsp ,loc))
          ((mem ,(if (equal? reg var) sub-var reg) ,offset) <- ,(if (equal? s var) sub-var s))))
      `(((mem ,reg ,offset) <- ,s))))

(define (do-aop w aop t var loc prefix)
  (if (equal? w var)
      (let ((sub-var (sub prefix)))
        `((,sub-var <- (mem rsp ,loc)) (,sub-var ,aop ,(if (equal? var t) sub-var t)) ((mem rsp ,loc) <- ,sub-var)))
      (if (equal? t var)
          (let ((sub-var (sub prefix)))
            `((,sub-var <- (mem rsp ,loc)) (,w ,aop ,sub-var)))
          `((,w ,aop ,t)))))

(define (do-sop w sop sx var loc prefix)
  (if (equal? w var)
      (let ((sub-var (sub prefix)))
        `((,sub-var <- (mem rsp ,loc)) (,sub-var ,sop ,(if (equal? var sx) sub-var sx)) ((mem rsp ,loc) <- ,sub-var)))
      (if (equal? sx var)
          (let ((sub-var (sub prefix)))
            `((,sub-var <- (mem rsp ,loc)) (,w ,sop ,sub-var)))
          `((,w ,sop ,sx)))))

; (w <- a cmp b)
(define (do-w<-cmp w a cmp b var loc prefix)
  (if (equal? w var)
      (let ((sub-var (sub prefix)))
        (append (if (or (equal? var a) (equal? var b)) `((,sub-var <- (mem rsp ,loc))) `())
                `((,sub-var <- ,(if (equal? var a) sub-var a) ,cmp ,(if (equal? var b) sub-var b))
                  ((mem rsp ,loc) <- ,sub-var))))
      (if (or (equal? var a) (equal? var b))
          (let ((sub-var (sub prefix)))
            `((,sub-var <- (mem rsp ,loc))
              (,w <- ,(if (equal? var a) sub-var a) ,cmp ,(if (equal? var b) sub-var b))))
          `((,w <- ,a ,cmp ,b)))))

(define (do-cjmp a cmp b then else var loc prefix)
  (if (or (equal? a var) (equal? b var))
      (let ((sub-var (sub prefix)))
        `((,sub-var <- (mem rsp ,loc)) 
          (cjump ,(if (equal? var a) sub-var a) ,cmp ,(if (equal? var b) sub-var b) ,then ,else)))
      `((cjump ,a ,cmp ,b ,then ,else))))

(define (do-call u nat var loc prefix)
  (if (equal? u var)
      (let ((sub-var (sub prefix)))
        `((,sub-var <- (mem rsp ,loc)) (call ,sub-var ,nat)))
      `((call ,u ,nat))))

(define (do-tail-call u nat var loc prefix)
  (if (equal? u var)
      (let ((sub-var (sub prefix)))
        `((,sub-var <- (mem rsp ,loc)) (tail-call ,sub-var ,nat)))
      `((tail-call ,u ,nat))))

(define (do-w<-stack w num var loc prefix)
  (if (equal? w var)
      (let ((sub-var (sub prefix)))
        `((,sub-var <- (stack-arg ,num)) ((mem rsp ,loc) <- ,sub-var)))
      `((,w <- (stack-arg ,num)))))


(define (spill func var prefix)
  (define loc (* 8 (third func)))
  (define new-func `(,(car func) ,(cadr func) ,(+ 1 (third func))))
  (for ([line (cdddr func)])
    (set! new-func (append new-func (parse-line line var loc prefix))))
  (set! count 0)
  new-func)



















