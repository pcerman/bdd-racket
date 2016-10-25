#lang racket

;;
;; bdd-racket: experimental library implementing BDDs in racket
;; programming language (https://racket-lang.org)
;;
;; This source code is released under MIT License
;; Copyright (c) 2016 Peter Cerman (https://github.com/pcerman)
;;

(provide bdd-node
         bdd-node?
         bdd-node-var
         bdd-node-lo
         bdd-node-hi)


;;****************************************************************************

(struct bdd-node (var lo hi) #:transparent)
