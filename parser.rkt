#lang racket


(require "AST.rkt" "lib.rkt")
;(provide parse-p parse-e parse-d parse-v parse-token)
(provide (all-defined-out))
;;;;;;;;;;; REGEX DECLARATIONS ;;;;;;;;;;;;;;;;;;

(define label-str "^:[a-zA-Z_][a-zA-Z_0-9]*")
(define var-str "^[a-zA-Z_][a-zA-Z_0-9]*")
(define num-str "\\b(?:0|[1-9][0-9]*)\\b")
(define pred-str "\\b(?:number[?]|a[?])\\b")
(define biop-str "\\b(?:<=|[-+*<=])\\b")


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

;; quoted -> program-node%
(define (parse-p quoted-expr)
  (let ([program-label    (parse-e          (first quoted-expr))]
        [functions        (map parse-func   (rest  quoted-expr))])
  (make-object program-node%  program-label  (list program-label functions))))


;; quoted -> func-node%
(define (parse-func  quoted-expr)
  (let ([func-label (parse-token (first quoted-expr))]
        [args       (map parse-token (second quoted-expr))]
        [body       (parse-e (third quoted-expr))])
    (make-object function-node% func-label args body)))


;; string -> e-node%/d-node%/v-node%
(define (parse-e quoted-expr)
  (match quoted-expr
    [`(let ([,var ,d]) ,e)        (make-object let-node% (parse-v var) (parse-d d) (parse-es e))]
    [`(if ,v ,e1 ,e2)             (make-object if-node%  (parse-v v) (parse-es e1 e2))]
    [else                         (parse-d quoted-expr)]))


(define (parse-es . exprs)
  (map parse-e exprs))


(define (parse-d quoted-expr)
  (match quoted-expr
    [`(,(? is-biop? op) ,v1 ,v2)     (make-object biop-node%  (parse-token op)   (parse-vs v1 v2))]
    [`(,(? is-pred? pred) ,v)        (make-object pred-node%  (parse-token pred) (parse-vs v))]
    [`(new-array ,v1 ,v2)            (make-object new-array-node%  (parse-vs v1 v2))]
    [`(new-tuple ,vs ...)            (make-object new-tuple-node%  (apply parse-vs vs))]
    [`(aref ,v1 ,v2)                 (make-object aref-node%  (parse-vs v1 v2))]
    [`(aset ,v1 ,v2 ,v3)             (make-object aset-node%  (parse-vs v1 v2 v3))]
    [`(alen ,v)                      (make-object alen-node%  (parse-vs v))]
    [`(print ,v)                     (make-object print-node% (parse-vs v))]
    [`(make-closure ,label ,v)       (make-object make-closure-node%  (parse-token label) (parse-vs v))]
    [`(closure-proc ,v)              (make-object closure-proc-node%  (parse-vs v))]
    [`(closure-vars ,v)              (make-object closure-vars-node%  (parse-vs v))]
    [`(,vs ...)                          (make-object func-call-node% (apply parse-vs vs))]
    [else               (parse-v quoted-expr)]))



(define (parse-v quoted-expr)
  (match quoted-expr
    [num?                (make-object num-node%   quoted-expr empty)]
    [(? is-label? l)     (make-object label-node%  quoted-expr empty)]
    [(? is-var? v)       (make-object var-node%   quoted-expr empty)]))

(define (parse-vs . exprs)
  (map parse-v exprs))

(define (parse-token quoted-expr)
  (match quoted-expr
    [(? number? n)          (make-object num-node%   quoted-expr empty)]
    [(? is-label? l)     (make-object label-node%  quoted-expr empty)]
    [(? is-biop?  op)    (make-object biop-node%  quoted-expr empty)]
    [(? is-pred?  pred)  (make-object pred-node%  quoted-expr empty)]
    [(? is-var? v)       (make-object var-node%   quoted-expr empty)]))