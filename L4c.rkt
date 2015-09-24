#lang plai
(require "L4c-lib.rkt")

(define count 0)
(define (fresh-var) 
  (set! count (+ count 1))
  (string->symbol (string-append "vval" (number->string (- count 1)))))


(define-type context
  [let-ctxt (x var?) (b 4e?) (k context?)]
  [if-ctxt (t 4e?) (e 4e?) (k context?)]
  ;[fun-ctxt (a 4e?) (k context?)]
  ;[arg-ctxt (f v?) (k context?)]
  [fun-ctxt (args (listof 4e?)) (k context?)]
  [arg-ctxt (f (lambda (n) (or (3-built-in? n) (v? n)))) 
            (done (listof 4e?)) 
            (rest (listof 4e?)) (k context?)]
  [no-ctxt])

; find: L4e context -> L3e
; takes the next step when a downward arrow points to e. k records the context
(define (parse-exp e k)
  (match e
    [`(let ((,x ,r)) ,b) (parse-exp r (let-ctxt x b k))]
    [`(begin ,e1 ,e2) (parse-exp `(let ((,(fresh-var) ,e1)) ,e2) k)]
    [`(if ,c ,t ,e) (parse-exp c (if-ctxt t e k))]
    [`(,f ...) (fill-exp (car e) (fun-ctxt (cdr e) k))]
    [(? v?) (fill-exp e k)])
  )

; fill: L3d context -> L3e
; does the same for an upward arrow
(define (fill-exp d k)
  (type-case context k
    [let-ctxt (x b k)
              `(let ((,x ,d)) ,(parse-exp b k))]
    [if-ctxt (t e k)
             (if (v? d)
                 `(if ,d ,(parse-exp t k) ,(parse-exp e k))
                 (let ((x (fresh-var)))
                   `(let ((,x ,d)) (if ,x ,(parse-exp t k) ,(parse-exp e k)))))]
    [fun-ctxt (a k)
              (if (or (v? d) (3-built-in? d))
                  (if (empty? a) (fill-exp `(,d) k)
                      (parse-exp (car a) (arg-ctxt d '() (cdr a) k)))
                  (let ([x (fresh-var)])
                    `(let ([,x ,d])
                       ,(if (empty? a) (fill-exp `(,x) k)
                            (parse-exp (car a) (arg-ctxt x '() (cdr a) k))))))]
    [arg-ctxt (f done r k)
              (if (v? d)
                  (if (empty? r)
                      (fill-exp `(,f ,@done ,d) k)
                      (parse-exp (car r) (next-k f done d r k)))
                  (let ((x (fresh-var)))
                    `(let ((,x ,d))
                       ,(if (empty? r) (fill-exp `(,f ,@done ,x) k)
                            (parse-exp (car r) (next-k f done x r k))))))]
    [no-ctxt () d]))

; L4e -> L3e
(define (norm e) (parse-exp e (no-ctxt)))


(define (next-k f done this rest k)
  (arg-ctxt f (append done (list this)) (cdr rest) k))


(define (4-compile the-code)
  (set! count 0)
  `(,(norm (car the-code)) ,@(map (lambda (f) `(,(car f) ,(cadr f) ,(norm (caddr f))))
                                  (cdr the-code))))



(define file-name ;"12.L4")
  (car (vector->list (current-command-line-arguments))))
(define in-port (open-input-file file-name))
(define the-code (read in-port))
(close-input-port in-port)

(define compiled-code (4-compile the-code))
;(printf "~a\n" compiled-code)

(with-output-to-file "prog.L3"
  (lambda () (printf "~a\n" compiled-code))
  #:exists 'replace)




