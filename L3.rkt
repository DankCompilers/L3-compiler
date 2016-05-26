#lang racket
(require racket/cmdline)
(require "L3toL2.rkt")

(command-line
 #:program "L3 compiler"
 #:args ((filename #f)) ; expect one command-line argument: <filename>
 (cond [filename
        (call-with-input-file filename
          (lambda (in)
            (let    ([l1-code (L3->L2-compile (read in))])
              (write   l1-code))))]
       [else (error "Provide a filename")]))