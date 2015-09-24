#lang racket
(require "L5c-lib.rkt")

(define func-list '())
(define id 0)

(define (new-var prefix)
  (set! id (+ 1 id))
  (string->symbol (string-append prefix (number->string id))))

;; given a letrec exp, return the equivalent let exp
(define (letrec->let x val body)
  `(let ([,x (new-tuple 0)])
     (begin (aset ,x 0 ,(compile-5e (sub-free val x `(aref ,x 0))))
            ,(compile-5e (sub-free body x `(aref ,x 0))))))

;; given any e, substitute all the free mention of x (a var) with new-x
(define (sub-free body x new-x)
  (match body
    ['() '()]
    [`(let ([,x ,new-val]) ,whatever-body) body]
    [(or (? num?) (? prim?)) body]
    [(? var? var) (if (equal? x var) new-x var)]
    [else (map (lambda (b) (sub-free b x new-x)) body)]))


(define (compile-func arg-vars func-body)
  (let ([free-vars (find-free func-body arg-vars '())]
        [func-label (new-var ":func")])
    (set! func-list
          (cons `(,func-label (vars-tup ,@arg-vars) 
                              ,(fill-free-vars free-vars func-body 0))
                func-list))
    `(make-closure ,func-label (new-tuple ,@free-vars))))

; a recursive function that finds all the free vars in a e
; the done-vars are those that won't be considered free vars in later examination
(define (find-free func-body done-vars free-vars)
  (match func-body
    [(? prim?) free-vars]
    [(? var? var) (if (member var done-vars) free-vars (cons var free-vars))]
    [(? num?) free-vars]
    [`(lambda (,(? var? vars) ...) ,(? 5e? body))
     (find-free body (append done-vars vars) free-vars)]
    [`(let ([,(? var? x) ,(? 5e? val)]) ,(? 5e? body)) 
     (find-free body (cons x done-vars) free-vars)]
    [`(letrec ([,(? var? x) ,(? 5e? val)]) ,(? 5e? body))
     (find-free body (cons x done-vars) free-vars)]
    [`(if ,(? 5e? cond) ,(? 5e? then) ,(? 5e? else))
     (apply append (map (lambda (b) (find-free b done-vars free-vars)) (list cond then else)))]
    [`(new-tuple ,(? 5e? args) ...)
     (apply append (map (lambda (b) (find-free b done-vars free-vars)) args))]
    [`(,(? prim? prim-func) ,(? 5e? args) ...) 
     (apply append (map (lambda (b) (find-free b done-vars free-vars)) args))]
    [`(,(? 5e? func) ...) 
     (apply append (map (lambda (b) (find-free b done-vars free-vars)) func-body))]))


(define (fill-free-vars free-vars func-body count)
  (if (empty? free-vars) (compile-5e func-body)
      `(let ([,(car free-vars) (aref vars-tup ,count)])
         ,(fill-free-vars (cdr free-vars) func-body (+ count 1)))))


(define (compile-app func args)
  (if (var? func) 
      `((closure-proc ,func) (closure-vars ,func) ,@(map compile-5e args))
      (let ((func-var (new-var "func")))
        `(let ((,func-var ,(compile-5e func)))
           ((closure-proc ,func-var) (closure-vars ,func-var) ,@(map compile-5e args))))))



(define (compile-5e the-e)
  (match the-e
    [(? prim?) (compile-5e (prim-lambda the-e))]
    [(? var?) the-e]
    [(? num?) the-e]
    [`(let ([,(? var? x) ,(? 5e? val)]) ,(? 5e? body)) 
     `(let ([,x ,(compile-5e val)]) ,(compile-5e body))]
    [`(letrec ([,(? var? x) ,(? 5e? val)]) ,(? 5e? body)) (letrec->let x val body)]
    [`(if ,(? 5e? cond) ,(? 5e? then) ,(? 5e? else))
     `(if ,(compile-5e cond) ,(compile-5e then) ,(compile-5e else))]
    [`(new-tuple ,(? 5e? args) ...) `(new-tuple ,@(map compile-5e args))]
    [`(lambda (,(? var? vars) ...) ,(? 5e? func-body)) (compile-func vars func-body)]
    [`(,(? prim? prim-func) ,(? 5e? args) ...) `(,prim-func ,@(map compile-5e args))]
    [`(,(? 5e? func) ,(? 5e? args) ...) (compile-app func args)]
    [else (error 'compile-5e "unknown expression ~a" the-e)]
    ))


(define (5-compile the-code)
  (set! func-list '())
  (set! id 0)
  (define main (compile-5e the-code))
  `(,main ,@func-list))




(define file-name ;"robby/34.L5")
  (car (vector->list (current-command-line-arguments))))
(define in-port (open-input-file file-name))
(define the-code (read in-port))
(close-input-port in-port)

(define compiled-code (5-compile the-code))
;(printf "~a\n" compiled-code)

(with-output-to-file "prog.L4"
  (lambda () (printf "~a\n" compiled-code))
  #:exists 'replace)
