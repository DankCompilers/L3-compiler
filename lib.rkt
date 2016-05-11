#lang racket

(provide (all-defined-out))

;; symbol|string -> string
(define (to-string token)
  (format "~a" token))




(define (make-debug-printer)
  (define print-debug #t)
  (define (debug-print str . args)
    (when print-debug (apply printf (append (list str) args)) (printf "\n")))
  (define (debug-apply proc . args)
        (when print-debug (printf "given:    ~a\nexpected: ~a\n\n" (first args) (last args)))
        (apply proc args))
  (define (set-print-debug state)
    (set! print-debug state))
  (values debug-print debug-apply set-print-debug))

