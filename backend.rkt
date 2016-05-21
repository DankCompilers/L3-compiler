#lang racket

(require "AST.rkt" "lib.rkt")
(provide (all-defined-out))


;; setup name generators
(define L3-prefix 'L3)
(define label-prefix ':)
(define temp-gen  (make-temp-gen L3-prefix))
(define true-label-gen (let ([generator (make-temp-gen label-prefix)])
                         (lambda () (generator 'true))))
(define false-label-gen (let ([generator (make-temp-gen label-prefix)])
                          (lambda () (generator 'false))))
(define return-label-gen (let ([generator (make-temp-gen label-prefix)])
                          (lambda () (generator 'return))))
(define arg-regs (list 'rdi 'rsi 'rdx 'rcx 'r8 'r9))


;; p-node? -> string?
(define (L3->L2 l3-p-node)
  (to-string (p-node->L2 p-node)))


;; e-node? symbol?/bool? -> string
(define (e-node->L2 e-ast)
  ;; match the three possibilites
  (match e-ast
    [(? let-node?)   (let* ([children (get-children e-ast)]
                        [var-name  (first-child (first children))]
                        [c2  (second children)]
                        [c3  (third children)])
                   (printf "1:~a\n 2:~a\n 3:~a\n" var-name c2 c3)
                   (append
                    (d-node->L2 c2 var-name)
                    (e-node->L2 c3)))]
    
    [(? if-node?)   (let* ([children (get-children e-ast)]
                       [bool-val  (v-node->L2 (first children))]
                       [true-e  (second children)]
                       [false-e  (third children)]
                       [true-label (true-label-gen)]
                       [false-label (false-label-gen)])
                  ;; must be greater than 1 because 1 -> 0 in L2
                  (append `((cjump ,bool-val > 1 ,true-label  ,false-label)
                            ,true-label)
                          (e-node->L2 true-e)
                          `(,false-label)
                          (e-node->L2 false-e)))]
    ;; if a d node in an e spot, must be the last one, needs to return as well
     [else (append (d-node->L2 e-ast 'rax) `((return)))]))



(define (gen-bounds-check x tmp an-array an-index)
  (let ([bounds-pass-label1 (true-label-gen)]
        [bounds-pass-label2 (true-label-gen)]
        [bounds-fail-label  (false-label-gen)])
    ;; checks if positive first
    `((cjump 0 <= ,an-index  ,bounds-pass-label1 ,bounds-fail-label)
      ,bounds-fail-label
      (rdi <- ,an-array)
      (rsi <- ,(encode an-index))
      (call array-error 2)
      ;; now checks if less than array size
      ,bounds-pass-label1
      (,tmp <- (mem ,an-array 0))
      (cjump ,an-index < ,tmp ,bounds-pass-label2 ,bounds-fail-label)
      ,bounds-pass-label2)))



;; d-node? symbol? -> string?
(define (d-node->L2 d-ast bound-var)
  ;(println d-ast)
  (match d-ast
    [(or (? label-node?)
         (? num-node?)
         (? var-node?)
         (? biop-op-node?))  `((,bound-var <- ,(v-node->L2 d-ast)))]
    [else
     (let*  ([x bound-var]
             [children    (get-children d-ast)]
             [c-data      (map  v-node->L2 children)])
       (match d-ast
         [(? biop-node?)        (let* ([op     (first c-data)]
                                       [arg1   (second c-data)]
                                       [arg2   (third  c-data)]
                                       [contains-var (ormap var-node? children)])
                                  (match op
                                    ;; addition/subtraction need slight modification for encoding
                                    ['+           (if contains-var
                                                      ;; has var -> no need to alter
                                                      `((,x <- ,arg1)
                                                        (,x += ,arg2)
                                                        (,x -= 1))
                                                      ;; no var -> needs adjustment
                                                      `((,x <- ,arg1)
                                                        (,x += ,arg2)
                                                        (,x -= 1)))]
                                    ['-           (if contains-var
                                                      ;; has var -> no need to alter
                                                      `((,x <- ,arg1)
                                                        (,x -= ,arg2)
                                                        (,x += 1))
                                                      ;; no var -> needs adjustment
                                                      `((,x <- ,arg1)
                                                        (,x -= ,arg2)
                                                        (,x += 1)))]
                                    ;;multiplication needs a tmp name
                                    ['*           (let ([tmp (temp-gen 'mult)])
                                                    `((,tmp <- ,(decode arg1))
                                                      (,x  <- ,(decode arg2))
                                                      (,x *= ,tmp)
                                                      (,x <<= 1)
                                                      (,x += 1)))]
                                    ;;cmp just need memcmp2w
                                    [(or '= '< '<=) `((,x <- ,arg1 ,op ,arg2))]
                                    ;; signal error
                                    [else          (error (format "biop-to-string: Did not recognize operator ~a" op))]))]
         
         ;;pred nodes
         [(? a?-node?)             `((,x <- ,(first c-data))
                                     (,x &= 1)
                                     (,x *= -2)
                                     (,x += 3))]
         
         [(? number?-node?)      `((,x <- ,(first c-data))
                                   (,x &= 1)
                                   (,x <<= 1)
                                   (,x += 1))]
         
         [(? func-call-node?)      (let* ([func-label (first c-data)]
                                          [ret-label  (return-label-gen)]
                                          [args       (rest c-data)]
                                          [num-args   (length args)]
                                          [offset    -8])
                                     (append
                                      (cons `((mem rsp -8) <- ,ret-label)
                                           (for/list ([i (range num-args)])
                                                     (cond
                                                       [(< i 6)  `(,(list-ref arg-regs i) <- ,(list-ref args i))]
                                                       [else     (set! offset (- offset 8))
                                                                 `((mem rsp ,offset) <- ,(list-ref args i))])))
                                       
                                      `((call ,func-label ,num-args)
                                        ,ret-label
                                        (,x <- rax))))]
         
         ;; array nodes
         [(? new-array-node?)        (let* ([size-arg    (first c-data)]
                                            [val-arg     (second c-data)])
                                       `((rdi <- ,size-arg)
                                         (rsi <- ,val-arg)
                                         (call allocate 2)
                                         (,x <- rax)))]
         
         [(? new-tuple-node?)   (let* ([size-arg    (encode (length c-data))]
                                       [val-arg     (first c-data)]
                                       [offset      8])
                                  
                                   (append `((rdi <- ,size-arg)
                                             (rsi <- ,val-arg)
                                             (call allocate 2)
                                             (,x <- rax))
                                           (for/list ([a-val (rest c-data )])
                                                     (set! offset (+ offset 8))
                                                     `((mem ,x ,offset) <- ,a-val))))]
         
         
         [(? aref-node?)        (let ([an-array    (first c-data)]
                                      [an-index    (decode (second c-data))]
                                      [tmp   (temp-gen 'bcheck)])
                                  ;; check bounds, return val
                                  (append (gen-bounds-check x tmp an-array an-index)
                                          `((,tmp <- ,an-index)
                                            (,tmp *= 8)
                                            (,tmp += ,an-array)
                                            (,x   <- (mem ,tmp 8)))))]
         
         [(? aset-node?)        (let ([tmp   (temp-gen 'bcheck)]
                                      [an-array    (first c-data)]
                                      [an-index    (decode (second c-data))]
                                      [a-val       (third c-data)])
                                  ;; check bounds, set val, return 0
                                  (append (gen-bounds-check x tmp an-array an-index)
                                          `((,x <- ,an-index)
                                            (,x *= 8)
                                            (,x += ,an-array)
                                            ((mem ,x 8)   <- ,a-val)
                                            (,x <- 1))))]   ;; put the final result for aset into x (always 0)
         
         [(? alen-node?)        (let ([an-array (first c-data)])
                                  `((,x <- (mem ,an-array 0))
                                    (,x <<= 1)
                                    (,x += 1)))]
         
         
         ;; closure nodes
         [(? make-closure-node?)   (let* ([size-arg    (encode 2)]
                                          [val1        (first c-data)]
                                          [val2        (second c-data)])
                                     `((rdi <- ,size-arg)
                                       (rsi <- ,val1)
                                       (call allocate 2)
                                       (,x <- rax)
                                       ((mem ,x 16) <- ,val2)))]
         
         [(? closure-proc-node?)    (let ([an-array    (first c-data)]
                                          [an-index    0]
                                          [tmp   (temp-gen 'bcheck)])
                                      ;; check bounds, return val
                                      (append (gen-bounds-check x tmp an-array an-index)
                                              `((,x   <- (mem ,an-array 8)))))]
         
         [(? closure-vars-node?)    (let ([an-array    (first c-data)]
                                          [an-index    1]
                                          [tmp   (temp-gen 'bcheck)])
                                      ;; check bounds, return val
                                      (append (gen-bounds-check x tmp an-array an-index)
                                              `((,x   <- (mem ,an-array 16)))))]
         
         
         [(? print-node?)       `((rdi <- ,(first c-data))
                                  (call print 1))]
         ;; signal error
         [else              (error "d-node->L2: Did not recognize ~a")]))]))



(define (v-node->L2 v-ast)
  (encode (first-child v-ast)))


;; node? -> quoted
(define (f-node->L2 f-ast)
    (match f-ast
      [(? f-node?)   (let* ([func-label (v-node->L2     (get-label f-ast))]
                            [args       (map v-node->L2 (get-args  f-ast))]
                            [num-args   (length args)]
                            [body       (e-node->L2     (get-body  f-ast))]
                            [num-locals (if (> (length args) 6)
                                            (- (length args) 6)
                                            0)]
                            [offset     (* 8 (- num-locals 1))])

                       (append `(,func-label ,num-args ,num-locals)
                               (for/list ([i (range num-args)])
                                         (cond
                                           ;; transfers registers to variables
                                           [(< i 6)  `(,(list-ref args i) <-  ,(list-ref arg-regs i))]
                                           
                                           [else     (set! offset (- offset 8))
                                                     `(,(list-ref args i) <- (stack-arg ,offset))]))
                               body))]
      
      [else            (error "f-node->L2: provided node is no f-node")]))

;; node? -> quoted
(define (p-node->L2 p-ast)
    (match p-ast
      ;; p nodes
      [(? p-node?)       (let* ([main-e      (v-node->L2 (get-main-e p-ast))]
                                [func-asts   (get-children p-ast)]
                                [funcs       (map f-node->L2 func-asts)])
                           (cons main-e funcs))]
      
      ;; did not match any valid cases
      [else   (error "p-node->L2: invalid node: ~a" p-ast)]))