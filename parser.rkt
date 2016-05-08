#lang racket


(require "AST.rkt")


;;;;;;;;;;; REGEX DECLARATIONS ;;;;;;;;;;;;;;;;;;

(define label-str "^:[a-zA-Z_][a-zA-Z_0-9]*")
(define var-str "^[a-zA-Z_][a-zA-Z_0-9]*")
(define num-str "\\b(?:0|[1-9][0-9]*)\\b")
(define pred-str "\\b(?:number[?]|a[?])\\b")
(define biop-str "\\b(?:+|-|*|<=|<|=)\\b")


(define label-regexp   (pregexp label-str))
(define var-regexp     (pregexp var-str))
(define num-regexp     (pregexp num-str))
(define pred-regexp    (pregexp pred-str))
(define biop-regexp    (pregexp biop-str))



;; quoted -> program-node%
(define (parse-p quoted-expr)
  (let ([program-label    (parse-e          (first quoted-expr))]
        [functions        (map parse-func   (rest  quoted-expr))])
  (new program-node%  empty  (list program-label functions))))



(define (parse-func  quoted-expr)
  (