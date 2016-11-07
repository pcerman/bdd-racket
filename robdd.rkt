#lang racket

;;
;; bdd-racket: experimental library implementing BDDs in racket
;; programming language (https://racket-lang.org)
;;
;; This source code is released under MIT License
;; Copyright (c) 2016 Peter Cerman (https://github.com/pcerman)
;;

(require "bdd.rkt"
         "b-expr.rkt"
         "utils.rkt"
         (only-in eopl define-datatype cases))


;;****************************************************************************

(provide make-robdd
         robdd-apply
         robdd-and
         robdd-or
         robdd-xor
         robdd-not
         robdd-restrict
         robdd-sat-count
         robdd-simplify
         robdd->b-expr)

;;****************************************************************************

(define (robdd-root dd)
  (cases robdd dd
    [robdd-value (v) v]
    [robdd-nodes (ns) (sub1 (vector-length ns))]))

(define (robdd-var dd x)
  (if (boolean? x) (if x 1000000001 1000000000)
      (cases robdd dd
        [robdd-value (v)  (if v 1000000001 1000000000)]
        [robdd-nodes (ns) (bdd-node-var (vector-ref ns x))])))

(define (robdd-lo dd x)
  (if (boolean? x) x
      (cases robdd dd
        [robdd-value (v)  v]
        [robdd-nodes (ns) (bdd-node-lo (vector-ref ns x))])))

(define (robdd-hi dd x)
  (if (boolean? x) x
      (cases robdd dd
        [robdd-value (v)  v]
        [robdd-nodes (ns) (bdd-node-hi (vector-ref ns x))])))

;;****************************************************************************

(define (make-node tt ht var lo hi)
  (if (eqv? lo hi)
      (values tt ht lo)
      (let ([bn (bdd-node var lo hi)])
        (if (hash-has-key? ht bn)
            (values tt ht (hash-ref ht bn))
            (let* ([un (car tt)]
                   [tt (list* (add1 un) bn (cdr tt))]
                   [ht (hash-set ht bn un)])
              (values tt ht un))))))

;;****************************************************************************

(define (make-robdd bexp vars)
  (define (build tt ht bexp vars var-id)
    (cond [(boolean? bexp)
               (values tt ht bexp)]
          [(pair? vars)
               (let*-values ([(tt ht u0) (build tt ht (b-subst bexp (car vars) #f) (cdr vars) (add1 var-id))]
                             [(tt ht u1) (build tt ht (b-subst bexp (car vars) #t) (cdr vars) (add1 var-id))])
                 (make-node tt ht var-id u0 u1))]
          [else
               (error 'make-bdd "boolean expression '~A' is not reduced!" bexp)]))

  (let-values ([(tt ht un) (build '(0) (hash) bexp vars 1)])
    (if (boolean? un)
        (robdd-value un)
        (robdd-nodes (list->vector (reverse (cdr tt)))))))

;;****************************************************************************

(define (robdd-apply op dd1 dd2)
  (define apply-fn (cond [(symbol? op)
                              (lambda (b1 b2) (b-eval (list op b1 b2)))]
                         [(procedure? op)
                              op]
                         [else
                              (bdd-assert #f 'robdd-apply "argument op has to be either symbol or function")]))

  (define (app tt ht gt x1 x2)
    (let ([xk (cons x1 x2)])
      (cond [(hash-has-key? gt xk)
                 (values tt ht gt (hash-ref gt xk))]

            [(and (boolean? x1) (boolean? x2))
                 (values tt ht gt (apply-fn x1 x2))]

            [else
                 (let ([va1 (robdd-var dd1 x1)]
                       [va2 (robdd-var dd2 x2)])
                   (cond [(eqv? va1 va2)
                              (let*-values ([(tt ht gt lo) (app tt ht gt (robdd-lo dd1 x1) (robdd-lo dd2 x2))]
                                            [(tt ht gt hi) (app tt ht gt (robdd-hi dd1 x1) (robdd-hi dd2 x2))]
                                            [(tt ht xn) (make-node tt ht va1 lo hi)])
                                (values tt ht (hash-set gt xk xn) xn))]
                         [(< va1 va2)
                              (let*-values ([(tt ht gt lo) (app tt ht gt (robdd-lo dd1 x1) x2)]
                                            [(tt ht gt hi) (app tt ht gt (robdd-hi dd1 x1) x2)]
                                            [(tt ht xn) (make-node tt ht va1 lo hi)])
                                (values tt ht (hash-set gt xk xn) xn))]
                         [else ;;(> va1 va2)
                              (let*-values ([(tt ht gt lo) (app tt ht gt x1 (robdd-lo dd2 x2))]
                                            [(tt ht gt hi) (app tt ht gt x1 (robdd-hi dd2 x2))]
                                            [(tt ht xn) (make-node tt ht va2 lo hi)])
                                (values tt ht (hash-set gt xk xn) xn))]))])))

  (let-values ([(tt ht gt xn) (app '(0) (hash) (hash) (robdd-root dd1) (robdd-root dd2))])
    (if (boolean? xn)
        (robdd-value xn)
        (robdd-nodes (list->vector (reverse (cdr tt)))))))

(define (robdd-and dd1 dd2)
  (robdd-apply 'and dd1 dd2))

(define (robdd-or dd1 dd2)
  (robdd-apply 'or dd1 dd2))

(define (robdd-xor dd1 dd2)
  (robdd-apply 'xor dd1 dd2))

(define (robdd-not dd)
  (define (bnot v)
    (if (boolean? v) (not v) v))

  (cases robdd dd
    [robdd-value (v)  (robdd-value (not v))]
    [robdd-nodes (ns) (robdd-nodes (vector-map (lambda (nn)
                                                 (bdd-node (bdd-node-var nn)
                                                           (bnot (bdd-node-lo nn))
                                                           (bnot (bdd-node-hi nn))))
                                               ns))]))

;;****************************************************************************

(define (robdd-restrict dd var val)
  (define (restrict tt ht gt x)
    (cond [(boolean? x)
               (values tt ht gt x)]

          [(hash-has-key? gt x)
               (values tt ht gt (hash-ref gt x))]

          [else
               (let ([va (robdd-var dd x)])
                 (if (eqv? va var)
                     (restrict tt ht gt (if (not val) (robdd-lo dd x) (robdd-hi dd x)))
                     (let*-values ([(tt ht gt u0) (restrict tt ht gt (robdd-lo dd x))]
                                   [(tt ht gt u1) (restrict tt ht gt (robdd-hi dd x))]
                                   [(tt ht un) (make-node tt ht va u0 u1)])
                       (values tt ht (hash-set gt x un) un))))]))

  (bdd-assert (and (integer? var)
                   (positive? var))
              'robdd-restrict
              "argument var has to be positive integer! ~A" var)
  (bdd-assert (boolean? val)
              'robdd-restrict
              "argument val has to be boolean value! ~A" val)

  (cases robdd dd
    [robdd-value (v)  dd]
    [robdd-nodes (ns)
      (let-values ([(tt ht gt un) (restrict '(0) (hash) (hash) (robdd-root dd))])
        (if (boolean? un)
            (robdd-value un)
            (robdd-nodes (list->vector (reverse (cdr tt))))))]))

;;****************************************************************************

(define (robdd-sat-count dd nvars)
  (define (var u)
    (if (boolean? u) (add1 nvars)
        (robdd-var dd u)))

  (define (count gt x)
    (cond [(boolean? x)
               (values gt (if x 1 0))]
          [(hash-has-key? gt x)
               (values gt (hash-ref gt x))]
          [else
               (let ([lo (robdd-lo dd x)]
                     [hi (robdd-hi dd x)])
                 (let*-values ([(gt cnlo) (count gt lo)]
                               [(gt cnhi) (count gt hi)])
                   (let ([cn (+ (* (expt 2 (- (var lo) (var x) 1)) cnlo)
                                (* (expt 2 (- (var hi) (var x) 1)) cnhi))])
                     (values (hash-set gt x cn) cn))))]))

  (cases robdd dd
    [robdd-value (v)  (if v (expt 2 nvars) 0)]
    [robdd-nodes (ns) (let-values ([(gt cn) (count (hash) (robdd-root dd))])
                        (* (expt 2 (sub1 (var (robdd-root dd)))) cn))]))

;;****************************************************************************

(define (robdd-simplify dd du)
  (define (sim tt ht xd xu)
    (cond [(eqv? xd #f)
               (values tt ht #f)]
          [(boolean? xu)
               (values tt ht xu)]
          [(eqv? xd #t)
               (let*-values [((tt ht lo) (sim tt ht xd (robdd-lo du xu)))
                             ((tt ht hi) (sim tt ht xd (robdd-hi du xu)))]
                 (make-node tt ht (robdd-var du xu) lo hi))]
          [(eqv? (robdd-var dd xd) (robdd-var du xu))
               (cond [(eqv? (robdd-lo dd xd) #f)
                          (sim tt ht (robdd-hi dd xd) (robdd-hi du xu))]
                     [(eqv? (robdd-hi dd xd) #f)
                          (sim tt ht (robdd-lo dd xd) (robdd-lo du xu))]
                     [else
                          (let*-values [((tt ht lo) (sim tt ht (robdd-lo dd xd) (robdd-lo du xu)))
                                        ((tt ht hi) (sim tt ht (robdd-hi dd xd) (robdd-hi du xu)))]
                            (make-node tt ht (robdd-var du xu) lo hi))])]
          [(< (robdd-var dd xd) (robdd-var du xu))
               (let*-values [((tt ht lo) (sim tt ht (robdd-lo dd xd) xu))
                             ((tt ht hi) (sim tt ht (robdd-hi dd xd) xu))]
                 (make-node tt ht (robdd-var dd xd) lo hi))]
          [else
               (let*-values [((tt ht lo) (sim tt ht xd (robdd-lo du xu)))
                             ((tt ht hi) (sim tt ht xd (robdd-hi du xu)))]
                 (make-node tt ht (robdd-var du xu) lo hi))]))

  (cases robdd dd
    [robdd-value (v)
      (if v du (robdd-value #f))]

    [robdd-nodes (ns)
      (cases robdd du
        [robdd-value (_)
          du]
        [robdd-nodes (_)
          (let-values ([(tt ht xn) (sim '(0) (hash) (robdd-root dd) (robdd-root du))])
            (if (boolean? xn)
                (robdd-value xn)
                (robdd-nodes (list->vector (reverse (cdr tt))))))])]))

;;****************************************************************************

(define (robdd->b-expr dd vars)
  (define (dd->be x vars var-id)
    (cond [(boolean? x) x]
          [(null? vars) (error 'bdd->b-expr "not enough variables for building boolean expression!")]
          [(eqv? (robdd-var dd x) var-id)
               (let ([e0 (dd->be (robdd-lo dd x) (cdr vars) (add1 var-id))]
                     [e1 (dd->be (robdd-hi dd x) (cdr vars) (add1 var-id))])
                 `(or (and ,(car vars) ,e1)
                      (and (not ,(car vars)) ,e0)))]
          [else
               (dd->be x (cdr vars) (add1 var-id))]))

  (cases robdd dd
    [robdd-value (v) v]
    [robdd-nodes (ns)
      (b-eval (dd->be (robdd-root dd) vars 1))]))

;;****************************************************************************

(define (test-robdd)
  (define be-0 #f)
  (define be-1 '(and x y))
  (define be-2 '(and x (not y)))
  (define be-3 'x)
  (define be-4 '(and (not x) y))
  (define be-5 'y)
  (define be-6 '(xor x y))
  (define be-7 '(or x y))
  (define be-8 '(and (not x) (not y)))
  (define be-9 '(eqv x y))
  (define be-a '(not y))
  (define be-b '(or x (not y)))
  (define be-c '(not x))
  (define be-d '(or (not x) y))
  (define be-e '(or (not x) (not y)))
  (define be-f #t)

  (define dd-0 (make-robdd be-0 '(x y)))
  (define dd-1 (make-robdd be-1 '(x y)))
  (define dd-2 (make-robdd be-2 '(x y)))
  (define dd-3 (make-robdd be-3 '(x y)))
  (define dd-4 (make-robdd be-4 '(x y)))
  (define dd-5 (make-robdd be-5 '(x y)))
  (define dd-6 (make-robdd be-6 '(x y)))
  (define dd-7 (make-robdd be-7 '(x y)))
  (define dd-8 (make-robdd be-8 '(x y)))
  (define dd-9 (make-robdd be-9 '(x y)))
  (define dd-a (make-robdd be-a '(x y)))
  (define dd-b (make-robdd be-b '(x y)))
  (define dd-c (make-robdd be-c '(x y)))
  (define dd-d (make-robdd be-d '(x y)))
  (define dd-e (make-robdd be-e '(x y)))
  (define dd-f (make-robdd be-f '(x y)))

  (define-syntax test-robdd-count
    (syntax-rules ()
      ((_ id dd cn)
       (let ((sc (robdd-sat-count dd 2)))
         (bdd-assert (eqv? sc cn)
                     'robdd-sat-count
                     "~A (!= ~A) is incorrect value for ~A!" sc cn id)))))

  (test-robdd-count 'dd-0 dd-0 0)
  (test-robdd-count 'dd-1 dd-1 1)
  (test-robdd-count 'dd-2 dd-2 1)
  (test-robdd-count 'dd-3 dd-3 2)
  (test-robdd-count 'dd-4 dd-4 1)
  (test-robdd-count 'dd-5 dd-5 2)
  (test-robdd-count 'dd-6 dd-6 2)
  (test-robdd-count 'dd-7 dd-7 3)
  (test-robdd-count 'dd-8 dd-8 1)
  (test-robdd-count 'dd-9 dd-9 2)
  (test-robdd-count 'dd-a dd-a 2)
  (test-robdd-count 'dd-b dd-b 3)
  (test-robdd-count 'dd-c dd-c 2)
  (test-robdd-count 'dd-d dd-d 3)
  (test-robdd-count 'dd-e dd-e 3)
  (test-robdd-count 'dd-f dd-f 4))

;;(test-robdd)

;;****************************************************************************

(define ex '(or (and a y) (and x y) (and (not a) (not b) x)))
(define bex (make-robdd ex '(a b x y))) 
