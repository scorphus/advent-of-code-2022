#lang info
(define collection "advent-of-code-2022")
(define pkg-desc "Solutions to Advent of Code 2022")
(define pkg-authors '(scorphus))
(define version "0.0")
(define license '(BSD-3-Clause))
(define deps '("base"))
(define build-deps
  '("scribble-lib" ; --
    "racket-doc" ; --
    "rackunit-lib" ; --
    "advent-of-code" ; --
    "cover" ; --
    "fmt" ; --
    "review" ; --
    ))
(define scribblings '(("scribblings/advent-of-code-2022.scrbl" ())))
