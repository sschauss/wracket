#lang info
(define collection "wracket")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/wracket.scrbl" ())))
(define pkg-desc "Lisp-like language to WebAssembly")
(define version "0.0")
(define pkg-authors '(Simon Schauß))
