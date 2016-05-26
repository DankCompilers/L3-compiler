#lang racket


(require "parser.rkt"
         "AST.rkt"
         "backend.rkt")



(provide L3->L2-compile L3->L2)



;; Consumes an L2 program and returns the related L1 program string
;; string? -> string?
(define (L3->L2-compile quoted-raw-L3)
  (L3->L2 (parse-p quoted-raw-L3)))


(define (L3->L2 p-ast)
  (p-node->L2 p-ast))
  