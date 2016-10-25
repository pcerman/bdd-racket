#lang racket

;;
;; bdd-racket: experimental library implementing BDDs in racket
;; programming language (https://racket-lang.org)
;;
;; This source code is released under MIT License
;; Copyright (c) 2016 Peter Cerman (https://github.com/pcerman)
;;

(require "utils.rkt")

(provide b-subst
         b-eval
         b-vars
         ex1
         ex2
         ex3)


;;****************************************************************************

(define (b-subst ex var val)
  (define (subst ex)
    (cond [(equal? ex var) val]
          [(pair? ex)      (list* (car ex) (map subst (cdr ex)))]
          [else            ex]))
  (b-eval (subst ex)))

;;****************************************************************************

(define (b-eval ex)
  (define (eval-not ex)
    (cond [(boolean? ex)
               (not ex)]
          [(and (pair? ex) (eq? 'not (car ex)))
               (cadr ex)]
          [else
               (list 'not ex)]))

  (cond [(boolean? ex) ex]
        [(symbol? ex) ex]
        [(pair? ex)
             (case (car ex)
               [(and) (let loop ([ex (cdr ex)] [er '()])
                        (if (null? ex) (cond [(null? er) #t]
                                             [(null? (cdr er)) (car er)]
                                             [else (cons 'and (reverse er))])
                            (let ([et (b-eval (car ex))])
                              (cond [(eqv? et #f) #f]
                                    [(eqv? et #t) (loop (cdr ex) er)]
                                    [(and (pair? et) (eq? 'and (car et)))
                                         (loop (cdr ex) (rappend (cdr et) er))]
                                    [else
                                         (loop (cdr ex) (cons et er))]))))]

               [(or)  (let loop ([ex (cdr ex)] [er '()])
                        (if (null? ex) (cond [(null? er) #f]
                                             [(null? (cdr er)) (car er)]
                                             [else (cons 'or (reverse er))])
                            (let ([et (b-eval (car ex))])
                              (cond [(eqv? et #t) #t]
                                    [(eqv? et #f) (loop (cdr ex) er)]
                                    [(and (pair? et) (eq? 'or (car et)))
                                         (loop (cdr ex) (rappend (cdr et) er))]
                                    [else
                                         (loop (cdr ex) (cons et er))]))))]

               [(not) (if (length-1? (cdr ex))
                          (eval-not (b-eval (cadr ex)))
                          (error 'b-eval "wrong not expression! '~A'" ex))]

               [(xor) (if (length-2? (cdr ex))
                          (let ([e1 (b-eval (cadr ex))]
                                [e2 (b-eval (caddr ex))])
                            (cond [(eqv? e1 #f) e2]
                                  [(eqv? e1 #t) (eval-not e2)]
                                  [(eqv? e2 #f) e1]
                                  [(eqv? e2 #t) (eval-not e1)]
                                  [else        (list 'xor e1 e2)]))
                          (error 'b-eval "wrong xor expression! '~A'" ex))]

               [(eqv) (if (length-2? (cdr ex))
                          (let ([e1 (b-eval (cadr ex))]
                                [e2 (b-eval (caddr ex))])
                            (cond [(eqv? e1 #t) e2]
                                  [(eqv? e1 #f) (eval-not e2)]
                                  [(eqv? e2 #t) e1]
                                  [(eqv? e2 #f) (eval-not e1)]
                                  [else         (list 'eqv e1 e2)]))
                          (error 'b-eval "wrong eqv expression! '~A'" ex))]

               [(imp) (if (length-2? (cdr ex))
                          (let ([e1 (b-eval (cadr ex))]
                                [e2 (b-eval (caddr ex))])
                            (cond [(eqv? e1 #f) #t]
                                  [(eqv? e1 #t) e2]
                                  [(eqv? e2 #f) (eval-not e1)]
                                  [(eqv? e2 #t) #t]
                                  [else (list 'imp e1 e2)]))
                          (error 'b-eval "wrong imp expression! '~A'" ex))]

               [else (error 'b-eval "unknown boolean operator! '~A'" ex)])]
        [else (error 'b-eval "invalid boolean expression! '~A'" ex)]))

;;****************************************************************************

(define (b-vars ex)
  (define (vars ex)
    (cond [(symbol? ex) (list ex)]
          [(pair? ex)   (foldl (lambda (ex vs)
                                 (set-union vs (vars ex)))
                               '()
                               (cdr ex))]
          [else '()]))
  (sort (vars ex) symbol<?))

;;****************************************************************************

(define ex1 '(and a b (or c (not d))))
(define ex2 '(or (and (not a) b) (and a (not b))))
(define ex3 '(or (and a b) (and (not a) (not b))))
