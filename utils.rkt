#lang racket

;;
;; bdd-racket: experimental library implementing BDDs in racket
;; programming language (https://racket-lang.org)
;;
;; This source code is released under MIT License
;; Copyright (c) 2016 Peter Cerman (https://github.com/pcerman)
;;

(provide >>>
         length-1?
         length-2?
         rappend
         bdd-assert)


;;****************************************************************************

(define-syntax >>>
  (syntax-rules ()
    [(_ va e1 e2)
         (let ((va e1))
           e2)]
    [(_ va e1 e2 ...)
         (let ((va e1))
           (>>> va e2 ...))]))

;;****************************************************************************

(define (length-1? ls)
  (and (pair? ls)
       (null? (cdr ls))))

(define (length-2? ls)
  (and (pair? ls)
       (pair? (cdr ls))
       (null? (cddr ls))))

;;(define (rappend l1 l2)
;;  (if (null? l1) l2
;;      (rappend (cdr l1) (cons (car l1) l2))))

(define-syntax rappend
  (syntax-rules ()
    ((_ l1 l2)
         (foldl cons l2 l1))))

;;****************************************************************************

(define (bdd-assert assert fn-name msg . args)
  (when (not assert)
    (apply error fn-name (string-append "assert >>> " msg) args)))
