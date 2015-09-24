#lang racket
(require "liveness-graph.rkt")
(require "L2c-lib.rkt")
(provide (all-defined-out))

; find all the vars in this function, recursively
(define (find-vars func-lines var-list)
  (if (empty? func-lines) 
      (filter (lambda (n) (and (not (integer? n)) (not (label? n)) (not (all-regs? n))))
              (remove-duplicates var-list))
      (match (car func-lines)
        [`(,(? w? w) <- ,(? s? s)) (find-vars (cdr func-lines) `(,s ,w ,@var-list))]
        [`(,(? w? w) <- (mem ,(? x? x) ,offset)) (find-vars (cdr func-lines) `(,x ,w ,@var-list))]
        [`((mem ,(? x? x) ,offset) <- ,(? s? s)) (find-vars (cdr func-lines) `(,s ,x ,@var-list))]
        [`(,(? w? w) ,(? aop=? aop) ,(? t? t)) (find-vars (cdr func-lines) `(,t ,w ,@var-list))]
        [`(,(? w? w) ,(? sop=? sop) ,(? sx? sx)) (find-vars (cdr func-lines) `(,sx ,w ,@var-list))]
        [`(,(? w? w) ,(? sop=? sop) ,(? integer? sx)) (find-vars (cdr func-lines) `(,sx ,w ,@var-list))]
        [`(,(? w? w) <- ,(? t? a) ,(? cmp? cmp) ,(? t? b)) 
         (find-vars (cdr func-lines) `(,w ,a ,b ,@var-list))]
        [(? label? l) (find-vars (cdr func-lines) var-list)]
        [`(goto ,(? label? l)) (find-vars (cdr func-lines) var-list)]
        [`(cjump ,(? t? a) ,(? cmp? cmp) ,(? t? b) ,(? label? then) ,(? label? else))
         (find-vars (cdr func-lines) `(,a ,b ,@var-list))]
        [`(call ,(? built-in-func? f) ,nat) (find-vars (cdr func-lines) var-list)]
        [`(call ,(? u? u) ,nat) (find-vars (cdr func-lines) `(,u ,@var-list))]
        [`(tail-call ,(? u? u) ,nat) (find-vars (cdr func-lines) `(,u ,@var-list))]
        [`(return) (find-vars (cdr func-lines) var-list)]
        [`(,(? w? w) <- (stack-arg ,nat)) (find-vars (cdr func-lines) `(,w ,@var-list))]
        [else (error 'liveness "unknown expression ~s" (car func-lines))])))

; given a line, find whether it's (a <- b) and return the symbol that isn't var
(define (find-<--case var line)
  (match line
    [`(,(? w? w) <- ,(? s? s))
     (if (or (equal? w var) (equal? s var)) (remove* `(,var) (list w s)) empty)]
    [else empty]))

; return a var graph list of `(var ,@list-of-symbol-interfering-with-var) for every var
(define (get-var-graph var func-body in-line out-list kill-list)
  (let ((sop-regs empty)
        (itf-list empty))
    (if (member var in-line)
        (let ((special-var (find-<--case var in-line)))
          (set! itf-list (remove* (cons var special-var) in-line)))
        (void))
    (for ([line func-body]
          [out out-list]
          [kill kill-list])
      (match line 
        [`(,(? w? w) ,(? sop=? sop) ,(? sx? sx)) (if (equal? sx var) (set! sop-regs (remove 'rcx regs)) (void))]
        [else (void)])
      (let* ((special-var (find-<--case var line))
             (new-itfs (if (member var out) (remove-duplicates (append kill out))
                           (if (member var kill) out empty))))
            (set! itf-list (append itf-list (remove* (cons var special-var) new-itfs)))))
    (cons var (sort (remove-duplicates (append sop-regs itf-list)) symbol<?))))

; given a graph, return the line about this var, recursively
(define (find-var-line var graph)
  (if (empty? graph)
      (error 'find-var-line "graph doesn't contain var ~a" graph var)
      (if (equal? var (caar graph))
          (car graph)
          (find-var-line var (cdr graph)))))

; return the list of vars that interfere with this reg
(define (get-reg-itf-list reg var-graph-list)
  (let ((itf-list empty))
    (for ([vgl var-graph-list])
      (if (member reg (cdr vgl)) (set! itf-list (cons (car vgl) itf-list)) (void)))
    (sort itf-list symbol<?)))



;;;;;;;;; allocate registers for variables

(define (build-reg-table graph var-list)
  (let ((color-table (make-color-table (sort graph (lambda (a b) (> (length a) (length b)))) empty)))
    (if color-table
        (map (lambda (var) (cons var (color-num->reg var color-table))) var-list)
        #f)))

; given the color table, find the register for this variable's color
(define (color-num->reg var color-table)
  (let ((color-num (cdr (find-var-line var color-table))))
    (caar (filter (lambda (c) (and (= color-num (cdr c)) (member (car c) regs))) color-table))))

; return a list of register allocation for every var
(define (make-color-table graph color-table)
  (if (empty? graph)
      (sort color-table (lambda (a b) (symbol<? (car a) (car b))))
      (let ((color (find-color (car graph) color-table)))
        (if color
            (make-color-table (cdr graph) (cons (cons (caar graph) color) color-table))
            #f))))

; find the suitable color for this node given the color table
(define (find-color node color-table)
  (let* ((sym (car node))
         (neighbors (cdr node))
         (neigh-colors empty))
    (for ([coloring color-table])
      (if (member (car coloring) neighbors)
          (set! neigh-colors (cons (cdr coloring) neigh-colors))
          (void)))
    (if (= 15 (length (remove-duplicates neigh-colors)))
        #f
        (car (remove* neigh-colors (build-list 15 values))))))


;;;; graph main functions
(define (print-graph graph allocation)
  (printf "(")
  (for ([line graph])
    (printf "~a\n " line))
  (printf ")\n~a\n" allocation))


(define (graph the-func)
  (define func-body (cdddr the-func))
  (define func-size (length func-body))
  (define var-list (sort (find-vars func-body empty) symbol<?))
  (define kill-list (map (lambda (n) (remove-duplicates (kill n func-body))) 
                         (build-list func-size values)))
  (define succ-list (map (lambda (n) (succ n func-body func-size)) (build-list func-size values)))
  (define live-result (liveness the-func))
  (define in-line (caar live-result))
  (define out-list (cdr live-result))
  (define var-graph-list 
    (map (lambda (var) (get-var-graph var func-body in-line out-list kill-list)) var-list))
  (define graph empty)
  ;; make the graph
  (let ((symbols (sort (append var-list regs) symbol<?)))
    (for ([sym symbols])
      (if (member sym regs)
          (let ((reg-itfs (sort (append (remove sym regs)
                                        (get-reg-itf-list sym var-graph-list)) symbol<?)))
            (set! graph (cons (cons sym reg-itfs) graph)))
          (set! graph (cons (find-var-line sym var-graph-list) graph)))))
  (set! graph (reverse graph))
  (cons graph (build-reg-table graph var-list))
  )
