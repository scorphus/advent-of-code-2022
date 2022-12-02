; This file is part of Advent of Code 2022
; https://github.com/scorphus/advent-of-code-2022

; Licensed under the BSD-3-Clause license:
; https://opensource.org/licenses/BSD-3-Clause
; Copyright (c) 2022, Pablo S. Blum de Aguiar <scorphus@gmail.com>

; Day 2 - https://adventofcode.com/2022/day/2

#lang racket/base

(require racket/file
         racket/match)

(module* main #f
  (let ([lines (file->lines "input.txt")])
    (printf "part 1: ~a~n" (part-1 lines))
    (printf "part 2: ~a~n" (part-2 lines))))

(define (part-1 lines)
  (for/sum ([line lines])
           (match line
             ["A X" (+ 1 3)]
             ["A Y" (+ 2 6)]
             ["A Z" (+ 3 0)]
             ["B X" (+ 1 0)]
             ["B Y" (+ 2 3)]
             ["B Z" (+ 3 6)]
             ["C X" (+ 1 6)]
             ["C Y" (+ 2 0)]
             ["C Z" (+ 3 3)])))

(define (part-2 lines)
  (for/sum ([line lines])
           (match line
             ["A X" (+ 3 0)]
             ["A Y" (+ 1 3)]
             ["A Z" (+ 2 6)]
             ["B X" (+ 1 0)]
             ["B Y" (+ 2 3)]
             ["B Z" (+ 3 6)]
             ["C X" (+ 2 0)]
             ["C Y" (+ 3 3)]
             ["C Z" (+ 1 6)])))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define sample (list "A Y" "B X" "C Z"))

  (define suite
    (test-suite "day 2 tests"
                (test-equal? "part 1 with sample input" (part-1 sample) 15)
                (test-equal? "part 2 with sample input" (part-2 sample) 12)))

  (run-tests suite))
