#lang racket

;;
;; bdd-racket: experimental library implementing BDDs in racket
;; programming language (https://racket-lang.org)
;;
;; This source code is released under MIT License
;; Copyright (c) 2016 Peter Cerman (https://github.com/pcerman)
;;

(require "bdd.rkt"
         "robdd.rkt"
         "utils.rkt")


;;----------------------------------------------------------------------------
;; Full adder
;;----------------------------------------------------------------------------
;; X, Y - input bits
;; Ci   - incoming carry bit
;; S    - sum
;; Co   - outgoing carry bit

;; S  <-- (X xor Y) xor Ci
;; Co <-- (X and Y) or ((X xor Y) and Ci)


(define fa-S  (make-robdd '(xor (xor X Y) Ci) '(X Y Ci)))
;;
;;                        ( X )
;;                         / \
;;                        /   *
;;                       /     \
;;                    ( Y )   ( Y )
;;                      |\     /|
;;                      | *   * |
;;                      |  \ /  |
;;                      |   X   |
;;                      |  / \  |
;;                      | /   \ |
;;                    ( Ci ) ( Ci )
;;                     /|      | \
;;                    / *      |  *
;;                   /  |      |   \
;;                 [_] [T]    [T]  [_]
;;

(define fa-Co (make-robdd '(or (and X Y) (and Ci (xor X Y))) '(X Y Ci)))
;;
;;                        ( X )
;;                         / \
;;                        /   *
;;                       /     \
;;                    ( Y )   ( Y )
;;                     / \     / \
;;                    /   *   /   *
;;                   /     \ /     \
;;                 [_]    ( Ci )   [T]
;;                         / \
;;                        /   *
;;                       /     \
;;                     [_]     [T]
;;

;;****************************************************************************
;; test
(let ([fa-S-count  (robdd-sat-count fa-S 3)]
      [fa-Co-count (robdd-sat-count fa-Co 3)])
  (bdd-assert (eqv? fa-S-count 4)
              'full-adder-sum "~A is incorect value for count of sum-bit == 1 !" fa-S-count)
  (bdd-assert (eqv? fa-Co-count 4)
              'full-adder-carry "~A is incorect value for count of carry-bit == 1 !" fa-Co-count))


;;----------------------------------------------------------------------------
;; Queens problem
;;----------------------------------------------------------------------------
;; example how chessboard 4x4 is numbered
;;
;;          +----+----+----+----+
;;          | 13 | 14 | 15 | 16 |
;;          |----+----+----+----|
;;          |  9 | 10 | 11 | 12 |
;;          |----+----+----+----|
;;          |  5 |  6 |  7 |  8 |
;;          |----+----+----+----|
;;          |  1 |  2 |  3 |  4 |
;;          +----+----+----+----+
;;

(define (queens n)
  (define n+1 (add1 n))
  (define n-1 (sub1 n))

  (define (threaten-row i j)
    (let ([sn (add1 (* (sub1 i) n))])
      (remove (+ sn j -1) (range sn (+ sn n)))))

  (define (threaten-col i j)
    (let ([vn (+ (* (sub1 i) n) j)])
      (remove vn (range j (add1 (* n n)) n))))

  (define (threaten-dg1 i j)
    (let ([vn (+ (* (sub1 i) n) j)]
          [si (sub1 (min i j))]
          [ei (- n+1 (max i j))])
      (remove vn (range (- vn (* n+1 si))
                        (+ vn (* n+1 ei))
                        n+1))))

  (define (threaten-dg2 i j)
    (let ([vn (+ (* (sub1 i) n) j)]
          [si (min (sub1 i) (- n j))]
          [ei (min (- n+1 i) j)])
      (remove vn (range (- vn (* n-1 si))
                        (+ vn (* n-1 ei))
                        n-1))))

  (define (robdd-threaten-formula i j)
    (let ([vn (+ (* (sub1 i) n) j)])
      (>>> vs
           (sort (cons vn (append (threaten-row i j)
                                  (threaten-col i j)
                                  (threaten-dg1 i j)
                                  (threaten-dg2 i j))) >)

           (foldl (lambda (v va)
                    (if (boolean? va)
                        (list 0 (if (eqv? v vn) (bdd-node v #f #t)
                                                (bdd-node v #t #f)))
                        (list* (add1 (car va))
                               (if (eqv? v vn) (bdd-node v #f (car va))
                                               (bdd-node v (car va) #f))
                               (cdr va))))
                  #f
                  vs)

           (robdd-nodes (list->vector (reverse (cdr vs)))))))

  (if (< n 1)
      (robdd-value #f)
      (let ([rcs (range 1 n+1)])
        (foldl (lambda (i fm)
                 (robdd-and (foldl (lambda (j fm)
                                     (robdd-or (robdd-threaten-formula i j) fm))
                                   (robdd-value #f)
                                   rcs)
                            fm))
               (robdd-value #t)
               rcs))))

(define (queens-count n)
  (robdd-sat-count (queens n) (* n n)))

;;****************************************************************************
;; test
(let ([qn4 (queens-count 4)]
      [qn5 (queens-count 5)])
  (bdd-assert (eqv? qn4 2)
              'queens-count "~A is incorect value for number of queens(4) solutions!" qn4)
  (bdd-assert (eqv? qn5 10)
              'queens-count "~A is incorect value for number of queens(5) solutions!" qn5))
