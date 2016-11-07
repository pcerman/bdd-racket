#lang racket

;;
;; bdd-racket: experimental library implementing BDDs in racket
;; programming language (https://racket-lang.org)
;;
;; This source code is released under MIT License
;; Copyright (c) 2016 Peter Cerman (https://github.com/pcerman)
;;

(require "utils.rkt"
         (only-in eopl define-datatype cases))

(provide bdd-node
         bdd-node?
         bdd-node-var
         bdd-node-lo
         bdd-node-hi

         zdd
         zdd?
         zdd-value
         zdd-nodes

         robdd
         robdd?
         robdd-value
         robdd-nodes

         bdd->tgf
         bdd->tree)

;;****************************************************************************

(struct bdd-node (var lo hi) #:transparent)

(define-datatype zdd zdd?
  (zdd-value (v boolean?))
  (zdd-nodes (ns vector?)))

(define-datatype robdd robdd?
  (robdd-value (v boolean?))
  (robdd-nodes (ns vector?)))

;;****************************************************************************

(define (bdd->tgf dd [port #f] [var-names #f] [t-vals #f])
  (define t-nodes (if (length-2? t-vals) (list->vector t-vals) '#("._." ".T.")))

  (define (t-node v)
    (vector-ref t-nodes (if v 1 0)))

  (define (bdd-print fn)
    (cond [(port? port)        (fn port)]
          [(string? port)      (let ([filename port])
                                 (when (file-exists? filename)
                                   (delete-file filename))
                                 (call-with-output-file filename fn))]
          [else                (fn (current-output-port))])
    (void))

  (define (print-value v)
    (bdd-print (lambda (port)
                 (fprintf port "1 ~A~%#~%" (t-node v)))))

  (define (print-nodes ns)
    (define cns (vector-length ns))

    (define max-var (let loop ([i (sub1 cns)] [ma 0])
                      (if (< i 0) ma
                          (loop (sub1 i) (max ma (bdd-node-var (vector-ref ns i)))))))

    (define vars (list->vector (if (pair? var-names) var-names
                                   (map (lambda (n) (format "V~A" n)) (range 1 (add1 max-var))))))

    (define (var-name id)
      (vector-ref vars (sub1 id)))

    (define (idx-cnd ix)
      (if (integer? ix) (- cns ix) ix))

    (define (fold-nodes fn va)
      (let loop ([i (sub1 (vector-length ns))] [va va])
        (if (< i 0) va
            (let ([nd (vector-ref ns i)])
              (loop (sub1 i) (fn (idx-cnd i)
                                 (bdd-node-var nd)
                                 (idx-cnd (bdd-node-lo nd))
                                 (idx-cnd (bdd-node-hi nd))
                                 va))))))

    (if (> max-var (vector-length vars))
        (error 'bdd->tgf "not enough variables for building graph!")
        (bdd-print (lambda (port)
                     (fold-nodes (lambda (id var lo hi va)
                                   (fprintf port "~A ~A~%" id (var-name var)) va)
                                 #f)
                     (fold-nodes (lambda (id var lo hi va)
                                   (cond [(and (eqv? lo hi) (boolean? lo))
                                              (fprintf port "~A ~A~%" va (t-node lo))
                                              (add1 va)]
                                         [(and (boolean? lo) (boolean? hi))
                                              (fprintf port "~A ~A~%" va (t-node lo))
                                              (fprintf port "~A ~A~%" (add1 va) (t-node hi))
                                              (+ va 2)]
                                         [(boolean? lo)
                                              (fprintf port "~A ~A~%" va (t-node lo))
                                              (add1 va)]
                                         [(boolean? hi)
                                              (fprintf port "~A ~A~%" va (t-node hi))
                                              (add1 va)]
                                         [else va]))
                                 (add1 cns))
                     (fprintf port "#~%")
                     (fold-nodes (lambda (id var lo hi va)
                                   (cond [(and (eqv? lo hi) (boolean? lo))
                                              (fprintf port "~A ~A~%" id va)
                                              (fprintf port "~A ~A *~%" id va)
                                              (add1 va)]
                                         [(and (boolean? lo) (boolean? hi))
                                              (fprintf port "~A ~A~%" id va)
                                              (fprintf port "~A ~A *~%" id (add1 va))
                                              (+ va 2)]
                                         [(boolean? lo)
                                              (fprintf port "~A ~A~%" id va)
                                              (fprintf port "~A ~A *~%" id hi)
                                              (add1 va)]
                                         [(boolean? hi)
                                              (fprintf port "~A ~A~%" id lo)
                                              (fprintf port "~A ~A *~%" id va)
                                              (add1 va)]
                                         [else
                                              (fprintf port "~A ~A~%" id lo)
                                              (fprintf port "~A ~A *~%" id hi)
                                              va]))
                                 (add1 cns))))))

  (cond [(zdd? dd)
             (cases zdd dd
               [zdd-value (v)
                          (print-value v)]

               [zdd-nodes (ns)
                          (print-nodes ns)])]

        [(robdd? dd)
             (cases robdd dd
               [robdd-value (v)
                            (print-value v)]

               [robdd-nodes (ns)
                            (print-nodes ns)])]

        [else
             (error 'bdd->tgf "unknown argument type!")]))

;;****************************************************************************

(define (bdd->tree dd)
  (define (make-tree ns)
    (let loop ([i 0] [ht (hash #f #f #t #t)] [tr #f])
      (if (< i (vector-length ns))
          (let* ([nd (vector-ref ns i)]
                 [tr (vector (bdd-node-var nd)
                             (hash-ref ht (bdd-node-lo nd))
                             (hash-ref ht (bdd-node-hi nd)))])
            (loop (add1 i) (hash-set ht i tr) tr))
          tr)))

  (cond [(robdd? dd)
             (cases robdd dd
               [robdd-value (v)
                            v]

               [robdd-nodes (ns)
                            (make-tree ns)])]

        [(zdd? dd)
             (cases zdd dd
               [zdd-value (v)
                          v]

               [zdd-nodes (ns)
                          (make-tree ns)])]

        [else
             (error 'bdd->tree "unknown argument type!")]))
