; This file is part of Advent of Code 2022
; https://github.com/scorphus/advent-of-code-2022

; Licensed under the BSD-3-Clause license:
; https://opensource.org/licenses/BSD-3-Clause
; Copyright (c) 2022, Pablo S. Blum de Aguiar <scorphus@gmail.com>

#lang racket/base

(require racket/file
         racket/list
         racket/string)

(module* main #f
  (let ([lines (file->lines "input%%DAY%%.txt")])
    (printf "part 1: ~a~n" (part-1 lines))
    (printf "part 2: ~a~n" (part-2 lines))))

(define (part-1 lines)
  1)

(define (part-2 lines)
  2)

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define sample
    (list
     ; -- "" <-- a list of strings!
     ))

  (define suite
    (test-suite "day %%DAY%% tests"
                (test-equal? "part 1 with sample input" (part-1 sample) 1)
                (test-equal? "part 2 with sample input" (part-2 sample) 2)))

  (run-tests suite))
