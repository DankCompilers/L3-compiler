#lang racket

;; interface definitions
(define AST<%>
  (interface  () get-data get-children add-datum add-child set-nth-child))

(define p-node<%>
  (interface (AST<%>) get-mainE get-functions))

(define e-node<%>
  (interface (AST<%>) get-type))

(define d-node<%>
  (interface (AST<%>) get-type))

(define v-node<%>
  (interface (AST<%>) get-type))

(define token-node<%>
  (interface (AST<%>) get-type))


;; contracts for class definition
(define (is-p-node? v)
  (is-a? p-node<%> v))
(define (is-e-node? v)
  (is-a? e-node<%> v))
(define (is-d-node? v)
  (is-a? d-node<%> v))
(define (is-v-node? v)
  (is-a? v-node<%> v))
;; TODO: IMPLEMENT IS VAR NODE
(define (is-var-node? v)
  (is-a? v-node<%> v))
(define (is-token-node? v)
  (is-a? token-node<%> v))
(define datum/c (and/c number? symbol?))
(define node/c  (lambda (x) (is-a? x node%)))

;; base class for all AST nodes
;; (listof datum/c) (listof node/c) -> node/c
(define/contract node%
  (class/c (field [children (listof node/c)]
                  [data     (listof datum/c)])
           [add-child         (->m node/c  void?)]
           [add-datum         (->m datum/c void?)]
           [set-nth-child     (->m node/c  number? void?)]
           [set-nth-datum     (->m datum/c  number? void?)]
           [set-first-child   (->m node/c   void?)]
           [set-second-child  (->m node/c   void?)]
           [set-third-child   (->m node/c   void?)]
           [set-first-datum   (->m datum/c  void?)]
           [set-second-datum  (->m datum/c  void?)]
           [set-third-datum   (->m datum/c  void?)])
  (class* object% (AST<%>)
    (super-new)
    
    (init-field [data     empty]
                [children empty])

    ;;getters
    ;;children getters 
    (define/public (get-children)
      (children))
    
    (define/public (nth-child n)
      (list-ref children n))

    (define/public (first-child n)
      (nth-child 1))

    (define/public (second-child n)
      (nth-child 2))

    (define/public (third-child n)
      (nth-child 3))

    ;; data getters
    (define/public (get-data)
      (data))
    
    (define/public (nth-datum n)
      (list-ref data n))
    
    (define/public (first-datum n)
      (nth-child 1))
    
    (define/public (second-datum n)
      (nth-child 2))
    
    (define/public (third-datum n)
      (nth-child 3))


    ;; setters
    ;;children setters
    (define/public (add-child child)
      (set! children (append children (list child))))

    (define/public (set-nth-child n val)
      (set! children (list-set children n val)))

    (define/public (set-first-child val)
      (set-nth-child 1 val))

    (define/public (set-second-child val)
      (set-nth-child 2 val))
    
    (define/public (set-third-child val)
      (set-nth-child 3 val))
    
    ;; data setters
    (define/public (add-datum datum)
      (set! data (append children (list datum))))
    
    (define/public (set-nth-datum n val)
      (set! data (list-set data n val)))
    (define/public (set-first-datum val)
      (set-nth-datum 1 val))
    (define/public (set-second-datum val)
      (set-nth-datum 2 val))
    (define/public (set-third-datum val)
      (set-nth-datum 3 val))))


;;;;;;;;; P-type Nodes ;;;;;;;;;;;;;;;;

;; program-node
(define program-node%
  (class/c [get-mainE      (->m is-e-node?)]
           [get-functions  )


;; function-node
(define function-node%
  (class/c [get-label (->m is-token-node?)]
           [get-args  (->m  (listof is-var-node?))])
  
  (class* node% (p-node<%>)
    
    (define/public (get-label)
      (first (get-children)))

    (get-labe)

    ))



;;;;;;;;; E-type Nodes ;;;;;;;;;;;;;;;;
;; if node for E types
(define/contract if-node%
  (class/c [get-v  (->m is-v-node?)]
           [get-e1 (->m is-e-node?)]
           [get-e2 (->m is-e-node?)])

  (class* node% (e-node<%>)
    (inherit get-children get-data nth-child first-child second-child third-child
             set-nth-child set-first-child set-second-child set-third-child)

    (define/public (get-type) 'if)
    ;; getters
    (define/public (get-v)
      (first-child))

    (define/public (get-e1)
      (second-child))

    (define/public (get-e2)
      (third-child))))

;; let node for E types
(define/contract let-node%
  (class/c [get-var  (->m is-var-node?)]
           [get-d    (->m is-d-node?)]
           [get-e    (->m is-e-node?)])

  (class* node% (e-node<%>)
    (inherit get-children get-data nth-child first-child second-child third-child
             set-nth-child set-first-child set-second-child set-third-child)

    (define/public (get-type) 'let)
    
    ;; getters
    (define/public (get-var)
      (first-child))
    (define/public (get-d)
      (second-child))
    (define/public (get-e)
      (third-child))))




;;;;;;;;;;;;;;; D type Nodes ;;;;;;;;;;;;;;;;

;; Biop node for D types
(define/contract biop-node%
  (class/c [get-op    (->m is-token-node?)]
           [get-v1    (->m is-v-node?)]
           [get-v2    (->m is-v-node?)])

  (class* node% (d-node<%>)
    (inherit get-children get-data nth-child first-child second-child third-child
             set-nth-child set-first-child set-second-child set-third-child)

    (define/public (get-type) 'biop)
    
    ;; getters
    (define/public (get-op)
      (first-child))
    (define/public (get-v1)
      (second-child))
    (define/public (get-v2)
      (third-child))))


;; pred node for D types
(define/contract pred-node%
  (class/c [get-pred    (->m is-token-node?)]
           [get-v       (->m is-v-node?)])

  (class* node% (d-node<%>)
    (inherit get-children get-data nth-child first-child second-child third-child
             set-nth-child set-first-child set-second-child set-third-child)

    (define/public (get-type) 'pred)
    
    ;; getters
    (define/public (get-pred)
      (first-child))
    (define/public (get-v)
      (second-child))))

;; function call
(define/contract func-call-node%
  (class/c [get-func    (->m is-token-node?)]
           [get-args    (->m (listof is-v-node?))])

  (class* node% (d-node<%>)
    (inherit get-children get-data nth-child first-child second-child third-child
             set-nth-child set-first-child set-second-child set-third-child)

    (define/public (get-type) 'func-call)
    
    ;; getters
    (define/public (get-func)
      (first-child))
    (define/public (get-args)
      (rest (get-children)))))


;; new array
(define/contract new-array-node%
  (class/c [get-v1    (->m is-v-node?)]
           [get-v2    (->m is-v-node?)])

  (class* node% (d-node<%>)
    (inherit get-children get-data nth-child first-child second-child third-child
             set-nth-child set-first-child set-second-child set-third-child)

    (define/public (get-type) 'new-array)
    
    ;; getters
    (define/public (get-v1)
      (first-child))
    (define/public (get-v2)
      (second-child))))


;; aref
(define/contract aref-node%
  (class/c [get-v1    (->m is-v-node?)]
           [get-v2    (->m is-v-node?)])

  (class* node% (d-node<%>)
    (inherit get-children get-data nth-child first-child second-child third-child
             set-nth-child set-first-child set-second-child set-third-child)

    (define/public (get-type) 'aref)
    
    ;; getters
    (define/public (get-v1)
      (first-child))
    (define/public (get-v2)
      (second-child))))



;; aset
(define/contract aset-node%
  (class/c [get-v1    (->m is-v-node?)]
           [get-v2    (->m is-v-node?)]
           [get-v3    (->m is-v-node?)])

  (class* node% (d-node<%>)
    (inherit get-children get-data nth-child first-child second-child third-child
             set-nth-child set-first-child set-second-child set-third-child)

    (define/public (get-type) 'aset)
    
    ;; getters
    (define/public (get-v1)
      (first-child))
    (define/public (get-v2)
      (second-child))
    (define/public (get-v3)
      (second-child))))

;; alen
(define/contract alen-node%
  (class/c [get-v    (->m is-v-node?)])

  (class* node% (d-node<%>)
    (inherit get-children get-data nth-child first-child second-child third-child
             set-nth-child set-first-child set-second-child set-third-child)

    (define/public (get-type) 'alen)
    
    ;; getters
    (define/public (get-v)
      (first-child))))


;; print
(define/contract print-node%
  (class/c [get-v    (->m is-v-node?)])

  (class* node% (d-node<%>)
    (inherit get-children get-data nth-child first-child second-child third-child
             set-nth-child set-first-child set-second-child set-third-child)

    (define/public (get-type) 'print)
    
    ;; getters
    (define/public (get-v)
      (first-child))))



;;;;;;; Closure D's

;; make-closure
(define/contract make-closure-node%
  (class/c [get-label    (->m is-token-node?)]
           [get-v        (->m is-v-node?)])

  (class* node% (d-node<%>)
    (inherit get-children get-data nth-child first-child second-child third-child
             set-nth-child set-first-child set-second-child set-third-child)

    (define/public (get-type) 'aref)
    
    ;; getters
    (define/public (get-label)
      (first-child))
    (define/public (get-v)
      (second-child))))



;; closure-proc
(define/contract closure-proc-node%
  (class/c [get-v    (->m is-v-node?)])

  (class* node% (d-node<%>)
    (inherit get-children get-data nth-child first-child second-child third-child
             set-nth-child set-first-child set-second-child set-third-child)

    (define/public (get-type) 'aref)
    
    ;; getters
    (define/public (get-v)
      (first-child))))



;; closure-vars
(define/contract closure-vars-node%
  (class/c [get-v    (->m is-v-node?)])

  (class* node% (d-node<%>)
    (inherit get-children get-data nth-child first-child second-child third-child
             set-nth-child set-first-child set-second-child set-third-child)

    (define/public (get-type) 'aref)
    
    ;; getters
    (define/public (get-v)
      (first-child))))



;;;;;;;;;;;; V Nodes




