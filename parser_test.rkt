#lang racket

(require "AST.rkt" "lib.rkt" "parser.rkt")
(require rackunit)


(define-values (debug-print debug-apply set-print-debug) (make-debug-printer))




(module+ test
  (define (test-parse func quoted expected)
    (debug-apply check-equal? (func quoted) expected))

  (test-parse parse-token ':label   (make-object label-node% ':label))
  ;(test-parse parse-token  10      (make-object num-node%   10))
  ;(test-parse parse-token 'avar    (make-object var-node%   'avar))
  ;(test-parse parse-token 'number? (make-object pred-label-node% 'number?))
  ;(test-parse parse-token 'a?      (make-object pred-label-node% 'a?))
  ;(test-parse parse-token '+       (make-object biop-op-node%    '+))
  ;(test-parse parse-token '-       (make-object biop-op-node%    '-))
  ;(test-parse parse-token '*       (make-object biop-op-node%    '*))
  ;(test-parse parse-token '<       (make-object biop-op-node%    '<))
  ;(test-parse parse-token '=       (make-object biop-op-node%    '=))
  ;(test-parse parse-token '<=       (make-object biop-op-node%   '<=))
 

  )