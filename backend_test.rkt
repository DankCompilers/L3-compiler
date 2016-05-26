#lang racket

(require "backend.rkt" "AST.rkt" "lib.rkt" "parser.rkt")
(require rackunit)


(define-values (debug-print debug-apply set-print-debug) (make-debug-printer))


(module+ test
  (define (test-backend func parse-func quoted expected)
   (debug-apply check-equal? (func (parse-func quoted)) expected))
  
  (define (test-backend-v quoted expected)
    (test-backend v-node->L2 parse-v quoted expected))
  
  (define (test-backend-d quoted expected)
    (debug-apply check-equal? (d-node->L2 (parse-d quoted) 'ret) expected))

  
  (define (test-backend-e quoted expected)
    (test-backend e-node->L2 parse-e quoted expected))

  (define (test-backend-f quoted expected)
    (test-backend f-node->L2 parse-f quoted expected))
  
  (define (test-backend-p quoted expected)
    (test-backend p-node->L2 parse-p quoted expected))


  
  ;; v nodes
  (test-backend-v ':hello  ':hello)
  (test-backend-v 'hello   'hello)
  (test-backend-v 5        11)

  ;; test backend d

  ;; v nodes
  (test-backend-d ':hello  `((ret <- :hello)))
  (test-backend-d 'hello   `((ret <- hello)))
  (test-backend-d 5        `((ret <- 11)))


  ;; test biops
  (test-backend-d `(+ 1 3) `((ret <- 3)
                             (ret += 7)
                             (ret -= 1)))

  (test-backend-d `(+ avar 3) `((ret <- avar)
                                (ret += 7)
                                (ret -= 1)))

  (test-backend-d `(+ avar bvar) `((ret <- avar)
                                (ret += bvar)
                                (ret -= 1)))

  (test-backend-d `(- 1 3) `((ret <- 3)
                             (ret -= 7)
                             (ret += 1)))

  (test-backend-d `(* 2 3) `((L3mult0 <- 2)
                             (ret <- 3)
                             (ret *= L3mult0)
                             (ret <<= 1)
                             (ret += 1)))

  (test-backend-d `(< 1 3) `((ret <- 3 < 7)))
  (test-backend-d `(<= 1 3) `((ret <- 3 <= 7)))
  (test-backend-d `(= 1 3) `((ret <- 3 = 7)))

  ;; test pred
  (test-backend-d `(number? 1) `((ret <- 3)
                                 (ret &= 1)
                                 (ret <<= 1)
                                 (ret += 1)))

  (test-backend-d `(a? avar)    `((ret <- avar)
                                 (ret &= 1)
                                 (ret *= -2)
                                 (ret += 3)))


  ;; test arrays
  (test-backend-d `(new-array 10 0) `((rdi <- 21)
                                     (rsi <- 1)
                                     (call allocate 2)
                                     (ret <- rax)))
  
  (test-backend-d `(alen arr) `((ret <- (mem arr 0))
                                (ret <<= 1)
                                (ret += 1)))
  
  (test-backend-d `(aref arr 1) `((cjump 0 <= 1 :true0 :false0)
                                     :false0
                                     (rdi <- arr)
                                     (rsi <- 3)
                                     (call array-error 2)
                                     :true0
                                     (L3bcheck1 <- (mem arr 0))
                                     (cjump 1 < L3bcheck1 :true1 :false0)
                                     :true1
                                     (L3bcheck1 <- 1)
                                     (L3bcheck1 *= 8)
                                     (L3bcheck1 += arr)
                                     (ret <- (mem L3bcheck1 8))))

  (test-backend-d `(aref arr avar) `((cjump 0 <= avar :true2 :false1)
                                     :false1
                                     (rdi <- arr)
                                     (rsi <- avar)
                                     (call array-error 2)
                                     :true2
                                     (L3bcheck2 <- (mem arr 0))
                                     (cjump avar < L3bcheck2 :true3 :false1)
                                     :true3
                                     (L3bcheck2 <- avar)
                                     (L3bcheck2 *= 8)
                                     (L3bcheck2 += arr)
                                     (ret <- (mem L3bcheck2 8))))

  (test-backend-d `(aset arr index 10) `((cjump 0 <= index :true4 :false2)
                                         :false2
                                         (rdi <- arr)
                                         (rsi <- index)
                                         (call array-error 2)
                                         :true4
                                         (L3bcheck3 <- (mem arr 0))
                                         (cjump index < L3bcheck3 :true5 :false2)
                                         :true5
                                         (ret <- index)
                                         (ret *= 8)
                                         (ret += arr)
                                         ((mem ret 8) <- 21)
                                         (ret <- 1)))

  (test-backend-d `(new-tuple 10) `((rdi <- 3)
                                    (rsi <- 21)
                                    (call allocate 2)
                                    (ret <- rax)))

  (test-backend-d `(new-tuple 10 0 1 avar) `((rdi <- 9)
                                             (rsi <- 21)
                                             (call allocate 2)
                                             (ret <- rax)
                                             ((mem ret 16) <- 1)
                                             ((mem ret 24) <- 3)
                                             ((mem ret 32) <- avar)))


    ;; test closures
  (test-backend-d `(make-closure :label 10) `((rdi <- 5)
                                              (rsi <- :label)
                                              (call allocate 2)
                                              (ret <- rax)
                                              ((mem ret 16) <- 21)))

  (test-backend-d `(closure-proc arr) `((cjump 0 <= 0 :true6 :false3)
                                         :false3
                                         (rdi <- arr)
                                         (rsi <- 1)
                                         (call array-error 2)
                                         :true6
                                         (L3bcheck4 <- (mem arr 0))
                                         (cjump 0 < L3bcheck4 :true7 :false3)
                                         :true7
                                         (ret <- (mem arr 8))))

  (test-backend-d `(closure-vars arr) `((cjump 0 <= 1 :true8 :false4)
                                         :false4
                                         (rdi <- arr)
                                         (rsi <- 3)
                                         (call array-error 2)
                                         :true8
                                         (L3bcheck5 <- (mem arr 0))
                                         (cjump 1 < L3bcheck5 :true9 :false4)
                                         :true9
                                         (ret <- (mem arr 16))))

  


  ;; test prints
  (test-backend-d `(print avar) `((rdi <- avar)
                                  (call print 1)))
  (test-backend-d `(print 10)   `((rdi <- 21)
                                  (call print 1)))

  ;; test func calls

  (test-backend-d `(a-func arg1 arg2)   `(((mem rsp -8) <- :return0)
                                          (rdi <- arg1)
                                          (rsi <- arg2)
                                          (call a-func 2)
                                          :return0
                                          (ret <- rax)))

  (test-backend-d `(a-func)   `(((mem rsp -8) <- :return1)
                                (call a-func 0)
                                :return1
                                (ret <- rax)))

  (test-backend-d `(a-func arg1 arg2 arg3 arg4 arg5 6 7 8)   `(((mem rsp -8) <- :return2)
                                                           (rdi <- arg1)
                                                           (rsi <- arg2)
                                                           (rdx <- arg3)
                                                           (rcx <- arg4)
                                                           (r8 <- arg5)
                                                           (r9 <- 13)
                                                           ((mem rsp -16) <- 15)
                                                           ((mem rsp -24) <- 17)
                                                           (call a-func 8)
                                                           :return2
                                                           (ret <- rax)))
                  


  ;; test e-nodes


  ;; test lets
  (test-backend-e `(let ([avar (+ 1 1)]) avar) `((avar <- 3)
                                                 (avar += 3)
                                                 (avar -= 1)
                                                 (rax <- avar)
                                                 (return)))

  (test-backend-e `(let ([avar 10]) (+ avar 20)) `((avar <- 21)
                                                   (rax <- avar)
                                                   (rax  += 41)
                                                   (rax -= 1)
                                                   (return)))

  (test-backend-e `(let ([avar (aref arr 10)])
                     (+ avar 20))              `((cjump 0 <= 10 :true10 :false5)
                                                 :false5
                                                 (rdi <- arr)
                                                 (rsi <- 21)
                                                 (call array-error 2)
                                                 :true10
                                                 (L3bcheck6 <- (mem arr 0))
                                                 (cjump 10 < L3bcheck6 :true11 :false5)
                                                 :true11
                                                 (L3bcheck6 <- 10)
                                                 (L3bcheck6 *= 8)
                                                 (L3bcheck6 += arr)
                                                 (avar <- (mem L3bcheck6 8))
                                                 (rax <- avar)
                                                 (rax += 41)
                                                 (rax -= 1)
                                                 (return)))

  (test-backend-e `(let ([avar (aref arr 10)])
                     (let ([add-20 (+ avar 20)])
                       (- add-20 20)))          `((cjump 0 <= 10 :true12 :false6)
                                                 :false6
                                                 (rdi <- arr)
                                                 (rsi <- 21)
                                                 (call array-error 2)
                                                 :true12
                                                 (L3bcheck7 <- (mem arr 0))
                                                 (cjump 10 < L3bcheck7 :true13 :false6)
                                                 :true13
                                                 (L3bcheck7 <- 10)
                                                 (L3bcheck7 *= 8)
                                                 (L3bcheck7 += arr)
                                                 (avar <- (mem L3bcheck7 8))
                                                 (add-20 <- avar)
                                                 (add-20 += 41)
                                                 (add-20 -= 1)
                                                 (rax <- add-20)
                                                 (rax -= 41)
                                                 (rax += 1)
                                                 (return)))


  ;; test ifs
    (test-backend-e `(if 1 (+ avar 1) (- avar 1))    `((cjump 3 > 1 :true14 :false7)
                                                       :true14
                                                       (rax <- avar)
                                                       (rax += 3)
                                                       (rax -= 1)
                                                       (return)
                                                       :false7
                                                       (rax <- avar)
                                                       (rax -= 3)
                                                       (rax += 1)
                                                       (return)))


  (test-backend-e `(if 1
                           (if 0
                               (+ avar 1) (- avar 1))
                           (if 1
                               (+ avar 1) (- avar 1)))    `((cjump 3 > 1 :true15 :false8)
                                                             :true15
                                                             (cjump 1 > 1 :true16 :false9)
                                                             :true16
                                                             (rax <- avar)
                                                             (rax += 3)
                                                             (rax -= 1)
                                                             (return)
                                                             :false9
                                                             (rax <- avar)
                                                             (rax -= 3)
                                                             (rax += 1)
                                                             (return)
                                                             :false8
                                                             (cjump 3 > 1 :true17 :false10)
                                                             :true17
                                                             (rax <- avar)
                                                             (rax += 3)
                                                             (rax -= 1)
                                                             (return)
                                                             :false10
                                                             (rax <- avar)
                                                             (rax -= 3)
                                                             (rax += 1)
                                                             (return)))

  (test-backend-e `(let ([cond (+ -1 2)])
                         (if cond
                           (if 0
                               (+ avar 1) (- avar 1))
                           (if 1
                               (+ avar 1) (- avar 1))))    `((cond <- -1)
                                                             (cond += 5)
                                                             (cond -= 1)
                                                             (cjump cond > 1 :true18 :false11)
                                                             :true18
                                                             (cjump 1 > 1 :true19 :false12)
                                                             :true19
                                                             (rax <- avar)
                                                             (rax += 3)
                                                             (rax -= 1)
                                                             (return)
                                                             :false12
                                                             (rax <- avar)
                                                             (rax -= 3)
                                                             (rax += 1)
                                                             (return)
                                                             :false11
                                                             (cjump 3 > 1 :true20 :false13)
                                                             :true20
                                                             (rax <- avar)
                                                             (rax += 3)
                                                             (rax -= 1)
                                                             (return)
                                                             :false13
                                                             (rax <- avar)
                                                             (rax -= 3)
                                                             (rax += 1)
                                                             (return)))


  ;; test backend p
  (test-backend-f   `(:a-func (arg1 arg2 arg3)
                              (let ([sum1 (+ arg1 arg2)])
                                (+ sum1 arg3)))           `(:a-func 3
                                                                     0
                                                                     (arg1 <- rdi)
                                                                     (arg2 <- rsi)
                                                                     (arg3 <- rdx)
                                                                     (sum1 <- arg1)
                                                                     (sum1 += arg2)
                                                                     (sum1 -= 1)
                                                                     (rax <- sum1)
                                                                     (rax += arg3)
                                                                     (rax -= 1)
                                                                     (return)))

  (test-backend-f   `(:a-func (arg1 arg2 arg3 arg4)
                              (let ([a-num (number? arg1)])
                                (if a-num
                                    (let ([sum1 (+ arg1 arg2)])
                                      (+ sum1 arg3))
                                    0)))                           `(:a-func
                                                                     4
                                                                     0
                                                                     (arg1 <- rdi)
                                                                     (arg2 <- rsi)
                                                                     (arg3 <- rdx)
                                                                     (arg4 <- rcx)
                                                                     (a-num <- arg1)
                                                                     (a-num &= 1)
                                                                     (a-num <<= 1)
                                                                     (a-num += 1)
                                                                     (cjump a-num > 1 :true21 :false14)
                                                                     :true21
                                                                     (sum1 <- arg1)
                                                                     (sum1 += arg2)
                                                                     (sum1 -= 1)
                                                                     (rax <- sum1)
                                                                     (rax += arg3)
                                                                     (rax -= 1)
                                                                     (return)
                                                                     :false14
                                                                     (rax <- 1)
                                                                     (return)))

  (test-backend-p `((let ([some-val (:a-func 1 2 3 4)])
                     some-val)
                     (:a-func (arg1 arg2 arg3 arg4)
                              (let ([a-num (number? arg1)])
                                (if a-num
                                    (let ([sum1 (+ arg1 arg2)])
                                      (+ sum1 arg3))
                                    0))))                           `(:L_1
                                                                      (:L_1 0
                                                                            0
                                                                            ((mem rsp -8) <- :return3)
                                                                            (rdi <- 3)
                                                                            (rsi <- 5)
                                                                            (rdx <- 7)
                                                                            (rcx <- 9)
                                                                            (call :a-func 4)
                                                                            :return3
                                                                            (some-val <- rax)
                                                                            (rax <- some-val)
                                                                            (return))
                                                                      (:a-func
                                                                       4
                                                                       0
                                                                       (arg1 <- rdi)
                                                                       (arg2 <- rsi)
                                                                       (arg3 <- rdx)
                                                                       (arg4 <- rcx)
                                                                       (a-num <- arg1)
                                                                       (a-num &= 1)
                                                                       (a-num <<= 1)
                                                                       (a-num += 1)
                                                                       (cjump a-num > 1 :true22 :false15)
                                                                       :true22
                                                                       (sum1 <- arg1)
                                                                       (sum1 += arg2)
                                                                       (sum1 -= 1)
                                                                       (rax <- sum1)
                                                                       (rax += arg3)
                                                                       (rax -= 1)
                                                                       (return)
                                                                       :false15
                                                                       (rax <- 1)
                                                                       (return)))) 
  )
