#lang racket


(require "AST.rkt" "lib.rkt")
;(provide parse-p parse-e parse-d parse-v parse-v)
(provide (all-defined-out))
;;;;;;;;;;; REGEX DECLARATIONS ;;;;;;;;;;;;;;;;;;

(define label-str "^:[a-zA-Z_][a-zA-Z_0-9]*")
(define var-str "^[a-zA-Z_][a-zA-Z_0-9]*")
(define num-str "\\b(?:0|[1-9][0-9]*)\\b")
(define pred-str "\\b(?:number[?]|a[?])\\b")
(define biop-str "\\B(?:[-+=*<]|<=)\\B")

(define label-regexp   (pregexp label-str))
(define var-regexp     (pregexp var-str))
(define num-regexp     (pregexp num-str))
(define pred-regexp    (pregexp pred-str))
(define biop-regexp    (pregexp biop-str))

;;;;;;;;;;;; TYPE CHECKERS ;;;;;;;;;;;;;;;;;;;;;;

;; p/regexp token -> bool
(define (match-token? reg raw-token)
  (regexp-match? reg (to-string raw-token)))


;; token -> bool
(define (is-pred? raw-token)
  (match-token? pred-regexp raw-token))

;; token -> bool
(define (is-biop? raw-token)
  (match-token? biop-regexp raw-token))

;; token -> bool
(define (is-var? raw-token)
  (match-token? var-regexp raw-token))

;; token -> bool
(define (is-label? raw-token)
  (match-token? label-regexp raw-token))



;;;;;;;;;; PARSE DEFINITIONS ;;;;;;;;;;;;;;;;;;;;

;; quoted -> program-node
(define (parse-p quoted-expr)
  (let ([main-e           (parse-e          (first quoted-expr))]
        [functions        (map parse-f      (rest  quoted-expr))])
  (p-node  functions main-e)))


;; quoted -> func-node
(define (parse-f  quoted-expr)
  (let ([func-label (parse-v     (first quoted-expr))]
        [args       (map parse-v (second quoted-expr))]
        [body       (parse-e     (third quoted-expr))])
    (f-node body func-label args)))


;; string -> e-node/d-node/v-node
(define (parse-e quoted-expr)
  (match quoted-expr
    [`(let ([,var ,d]) ,e)        (let-node (list (parse-v var) (parse-d d)  (parse-e e)))]
    [`(if ,v ,e1 ,e2)             (if-node  (list (parse-v v)   (parse-e e1) (parse-e e2)))]
    [else                         (parse-d quoted-expr)]))


(define (parse-es . exprs)
  (map parse-e exprs))


;; quote -> d-node/v-node/error
(define (parse-d quoted-expr)
  (match quoted-expr
    [`(,(? is-biop? op) ,v1 ,v2)     (biop-node     (parse-vs op v1 v2))]
    [`(number? ,v)                   (number?-node  (parse-vs v))]
    [`(a? ,v)                        (a?-node       (parse-vs v))]
    [`(new-array ,v1 ,v2)            (new-array-node  (parse-vs v1 v2))]
    [`(new-tuple ,vs ...)            (new-tuple-node  (apply parse-vs vs))]
    [`(aref ,v1 ,v2)                 (aref-node  (parse-vs v1 v2))]
    [`(aset ,v1 ,v2 ,v3)             (aset-node  (parse-vs v1 v2 v3))]
    [`(alen ,v)                      (alen-node  (parse-vs v))]
    [`(print ,v)                     (print-node (parse-vs v))]
    [`(read)                         (read-node  empty)]
    [`(make-closure ,label ,v)       (make-closure-node  (parse-vs label v))]
    [`(closure-proc ,v)              (closure-proc-node  (parse-vs v))]
    [`(closure-vars ,v)              (closure-vars-node  (parse-vs v))]
    [`(,vs ...)                      (func-call-node (apply parse-vs vs))]
    [else                            (parse-v quoted-expr)]))


;; quote -> v-node/error
(define (parse-v quoted-expr)
  (match quoted-expr
    [(? number? n)       (num-node       (list quoted-expr))]
    [(? is-label? l)     (label-node     (list quoted-expr))]
    [(? is-biop?  op)    (biop-op-node   (list quoted-expr))]
    [(? is-var? v)       (var-node       (list quoted-expr))]
    [else                (error (format "parse-v: expression not valid: ~a" quoted-expr))]))


;; quote -> (listof v-node)/error
(define (parse-vs . exprs)
  (map parse-v exprs))