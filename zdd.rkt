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
         (only-in eopl cases))

;;****************************************************************************

(provide make-zdd
         zdd-union
         zdd-intersect
         zdd-difference
         zdd-count
         zdd->b-expr)

;;****************************************************************************

(define (zdd-root dd)
  (cases zdd dd
    [zdd-value (v) v]
    [zdd-nodes (ns) (sub1 (vector-length ns))]))

(define (zdd-var dd x)
  (if (boolean? x) (if x 1000000001 1000000000)
      (cases zdd dd
        [zdd-value (v)  (if v 1000000001 1000000000)]
        [zdd-nodes (ns) (bdd-node-var (vector-ref ns x))])))

(define (zdd-lo dd x)
  (if (boolean? x) x
      (cases zdd dd
        [zdd-value (v)  v]
        [zdd-nodes (ns) (bdd-node-lo (vector-ref ns x))])))

(define (zdd-hi dd x)
  (if (boolean? x) x
      (cases zdd dd
        [zdd-value (v)  v]
        [zdd-nodes (ns) (bdd-node-hi (vector-ref ns x))])))

;;****************************************************************************

(define (make-node tt ht var lo hi)
  (if (eqv? hi #f)
      (values tt ht lo)
      (let ([bn (bdd-node var lo hi)])
        (if (hash-has-key? ht bn)
            (values tt ht (hash-ref ht bn))
            (let* ([un (car tt)]
                   [tt (list* (add1 un) bn (cdr tt))]
                   [ht (hash-set ht bn un)])
              (values tt ht un))))))

;;****************************************************************************

(define (make-zdd bexp vars)
  (define (build tt ht bexp vars var-id)
    (cond [(and (boolean? bexp) (empty? vars))
               (values tt ht bexp)]
          [(pair? vars)
               (let*-values ([(tt ht u0) (build tt ht (b-subst bexp (car vars) #f) (cdr vars) (add1 var-id))]
                             [(tt ht u1) (build tt ht (b-subst bexp (car vars) #t) (cdr vars) (add1 var-id))])
                 (make-node tt ht var-id u0 u1))]
          [else
               (error 'make-bdd "boolean expression '~A' is not reduced!" bexp)]))

  (let-values ([(tt ht un) (build '(0) (hash) bexp vars 1)])
    (if (boolean? un)
        (zdd-value un)
        (zdd-nodes (list->vector (reverse (cdr tt)))))))

;;****************************************************************************

(define (zdd-union dd1 dd2)
  (define (z-op tt ht gt z1 z2)
    (cond [(and (boolean? z1) (boolean? z2))
               (values tt ht gt (or z1 z2))]
          [(hash-has-key? gt (cons z1 z2))
               (values tt ht gt (hash-ref gt (cons z1 z2)))]
          [else
               (let ([va1 (zdd-var dd1 z1)]
                     [va2 (zdd-var dd2 z2)])
                 (cond [(eqv? va1 va2)
                            (let*-values ([(tt ht gt lo) (z-op tt ht gt (zdd-lo dd1 z1) (zdd-lo dd2 z2))]
                                          [(tt ht gt hi) (z-op tt ht gt (zdd-hi dd1 z1) (zdd-hi dd2 z2))]
                                          [(tt ht xn) (make-node tt ht va1 lo hi)])
                              (values tt ht (hash-set gt (cons z1 z2) xn) xn))]
                       [(< va1 va2)
                            (let*-values ([(tt ht gt lo) (z-op tt ht gt (zdd-lo dd1 z1) z2)]
                                          [(tt ht gt hi) (z-op tt ht gt (zdd-hi dd1 z1) #f)]
                                          [(tt ht xn) (make-node tt ht va1 lo hi)])
                              (values tt ht (hash-set gt (cons z1 z2) xn) xn))]
                       [else ;;(> va1 va2)
                            (let*-values ([(tt ht gt lo) (z-op tt ht gt z1 (zdd-lo dd2 z2))]
                                          [(tt ht gt hi) (z-op tt ht gt #f (zdd-hi dd2 z2))]
                                          [(tt ht xn) (make-node tt ht va2 lo hi)])
                              (values tt ht (hash-set gt (cons z1 z2) xn) xn))]))]))

  (let-values ([(tt ht gt xn) (z-op '(0) (hash) (hash) (zdd-root dd1) (zdd-root dd2))])
    (if (boolean? xn)
        (zdd-value xn)
        (zdd-nodes (list->vector (reverse (cdr tt)))))))

(define (zdd-intersect dd1 dd2)
  (define (z-op tt ht gt z1 z2)
    (cond [(and (boolean? z1) (boolean? z2))
               (values tt ht gt (and z1 z2))]
          [(hash-has-key? gt (cons z1 z2))
               (values tt ht gt (hash-ref gt (cons z1 z2)))]
          [else
               (let ([va1 (zdd-var dd1 z1)]
                     [va2 (zdd-var dd2 z2)])
                 (cond [(eqv? va1 va2)
                            (let*-values ([(tt ht gt lo) (z-op tt ht gt (zdd-lo dd1 z1) (zdd-lo dd2 z2))]
                                          [(tt ht gt hi) (z-op tt ht gt (zdd-hi dd1 z1) (zdd-hi dd2 z2))]
                                          [(tt ht xn) (make-node tt ht va1 lo hi)])
                              (values tt ht (hash-set gt (cons z1 z2) xn) xn))]
                       [(< va1 va2)
                            (z-op tt ht gt (zdd-lo dd1 z1) z2)]
                       [else ;;(> va1 va2)
                            (z-op tt ht gt z1 (zdd-lo dd2 z2))]))]))

  (let-values ([(tt ht gt xn) (z-op '(0) (hash) (hash) (zdd-root dd1) (zdd-root dd2))])
    (if (boolean? xn)
        (zdd-value xn)
        (zdd-nodes (list->vector (reverse (cdr tt)))))))

(define (zdd-difference dd1 dd2)
  (define (z-op tt ht gt z1 z2)
    (cond [(and (boolean? z1) (boolean? z2))
               (values tt ht gt (and z1 (not z2)))]
          [(hash-has-key? gt (cons z1 z2))
               (values tt ht gt (hash-ref gt (cons z1 z2)))]
          [else
               (let ([va1 (zdd-var dd1 z1)]
                     [va2 (zdd-var dd2 z2)])
                 (cond [(eqv? va1 va2)
                            (let*-values ([(tt ht gt lo) (z-op tt ht gt (zdd-lo dd1 z1) (zdd-lo dd2 z2))]
                                          [(tt ht gt hi) (z-op tt ht gt (zdd-hi dd1 z1) (zdd-hi dd2 z2))]
                                          [(tt ht xn) (make-node tt ht va1 lo hi)])
                              (values tt ht (hash-set gt (cons z1 z2) xn) xn))]
                       [(< va1 va2)
                            (let*-values ([(tt ht gt lo) (z-op tt ht gt (zdd-lo dd1 z1) z2)]
                                          [(tt ht gt hi) (z-op tt ht gt (zdd-hi dd1 z1) #f)]
                                          [(tt ht xn) (make-node tt ht va1 lo hi)])
                              (values tt ht (hash-set gt (cons z1 z2) xn) xn))]
                       [else ;;(> va1 va2)
                            (z-op tt ht gt z1 (zdd-lo dd2 z2))]))]))

  (let-values ([(tt ht gt xn) (z-op '(0) (hash) (hash) (zdd-root dd1) (zdd-root dd2))])
    (if (boolean? xn)
        (zdd-value xn)
        (zdd-nodes (list->vector (reverse (cdr tt)))))))

;;****************************************************************************

(define (zdd-count dd)
  (define (count gt x)
    (cond [(boolean? x)
               (values gt (if x 1 0))]
          [(hash-has-key? gt x)
               (values gt (hash-ref gt x))]
          [else
               (let ([lo (zdd-lo dd x)]
                     [hi (zdd-hi dd x)])
                 (let*-values ([(gt cn-lo) (count gt lo)]
                               [(gt cn-hi) (count gt hi)])
                   (let ([cn (+ cn-lo cn-hi)])
                     (values (hash-set gt x cn) cn))))]))

  (cases zdd dd
    [zdd-value (v)  (if v 1 0)]
    [zdd-nodes (ns) (let-values ([(gt cn) (count (hash) (zdd-root dd))])
                        cn)]))

;;****************************************************************************

(define (zdd->b-expr dd vars)
  (define (dd->be x vars var-id)
    (cond [(eqv? x #f)
               #f]
          [(eqv? x #t)
               (if (null? vars) #t
                   (list* 'and (map (lambda (v) `(not ,v)) vars)))]
          [(null? vars)
               (error 'bdd->b-expr "not enough variables for building boolean expression!")]
          [(eqv? (zdd-var dd x) var-id)
               (let ([lo (zdd-lo dd x)]
                     [hi (zdd-hi dd x)])
                 (if (eqv? lo hi)
                     (dd->be lo (cdr vars) (add1 var-id))
                     (let ([e0 (dd->be lo (cdr vars) (add1 var-id))]
                           [e1 (dd->be hi (cdr vars) (add1 var-id))])
                       `(or (and ,(car vars) ,e1)
                            (and (not ,(car vars)) ,e0)))))]
          [else
               `(and (not ,(car vars)) ,(dd->be x (cdr vars) (add1 var-id)))]))

  (cases zdd dd
    [zdd-value (v)
      (dd->be v vars 1)]
    [zdd-nodes (ns)
      (b-eval (dd->be (zdd-root dd) vars 1))]))

;;****************************************************************************

(define (test-zdd)
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

  (define zd-0 (make-zdd be-0 '(x y)))
  (define zd-1 (make-zdd be-1 '(x y)))
  (define zd-2 (make-zdd be-2 '(x y)))
  (define zd-3 (make-zdd be-3 '(x y)))
  (define zd-4 (make-zdd be-4 '(x y)))
  (define zd-5 (make-zdd be-5 '(x y)))
  (define zd-6 (make-zdd be-6 '(x y)))
  (define zd-7 (make-zdd be-7 '(x y)))
  (define zd-8 (make-zdd be-8 '(x y)))
  (define zd-9 (make-zdd be-9 '(x y)))
  (define zd-a (make-zdd be-a '(x y)))
  (define zd-b (make-zdd be-b '(x y)))
  (define zd-c (make-zdd be-c '(x y)))
  (define zd-d (make-zdd be-d '(x y)))
  (define zd-e (make-zdd be-e '(x y)))
  (define zd-f (make-zdd be-f '(x y)))

  (define-syntax test-zdd-count
    (syntax-rules ()
      ((_ id zd cn)
       (let ((sc (zdd-count zd)))
         (bdd-assert (eqv? sc cn)
                     'zdd-count
                     "~A (!= ~A) is incorrect value for ~A!" sc cn id)))))

  (test-zdd-count 'zd-0 zd-0 0)
  (test-zdd-count 'zd-1 zd-1 1)
  (test-zdd-count 'zd-2 zd-2 1)
  (test-zdd-count 'zd-3 zd-3 2)
  (test-zdd-count 'zd-4 zd-4 1)
  (test-zdd-count 'zd-5 zd-5 2)
  (test-zdd-count 'zd-6 zd-6 2)
  (test-zdd-count 'zd-7 zd-7 3)
  (test-zdd-count 'zd-8 zd-8 1)
  (test-zdd-count 'zd-9 zd-9 2)
  (test-zdd-count 'zd-a zd-a 2)
  (test-zdd-count 'zd-b zd-b 3)
  (test-zdd-count 'zd-c zd-c 2)
  (test-zdd-count 'zd-d zd-d 3)
  (test-zdd-count 'zd-e zd-e 3)
  (test-zdd-count 'zd-f zd-f 4))

;;(test-zdd)
