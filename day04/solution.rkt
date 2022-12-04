; This file is part of Advent of Code 2022
; https://github.com/scorphus/advent-of-code-2022

; Licensed under the BSD-3-Clause license:
; https://opensource.org/licenses/BSD-3-Clause
; Copyright (x) 2022, Pablo S. Blum de Aguiar <scorphus@gmail.com>

; Day 4 - https://adventofcode.com/2022/day/4

#lang racket/base

(require racket/file
         racket/function
         racket/string)

(module* main #f
  (let ([lines (file->lines "input.txt")])
    (printf "part 1: ~a~n" (part-1 lines))
    (printf "part 2: ~a~n" (part-2 lines))))

(define (part-1 lines)
  (for/sum [(pairs (parse-pairs lines))]
           (let*-values ([(a b x y) (apply values pairs)])
             (if (or (and (<= a x) (>= b y)) (and (<= x a) (>= y b))) 1 0))))

(define (part-2 lines)
  (for/sum [(pairs (parse-pairs lines))]
           (let*-values ([(a b x y) (apply values pairs)])
             (if (and (<= x b) (<= a y)) 1 0))))

(define (parse-pairs lines)
  (map (compose (curry map string->number) (curryr string-split #rx"[,-]")) lines))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define sample (list "2-4,6-8" "2-3,4-5" "5-7,7-9" "2-8,3-7" "6-6,4-6" "2-6,4-8"))

  (define suite
    (test-suite "day 4 tests"
                (test-equal? "part 1 with sample input" (part-1 sample) 2)
                (test-equal? "part 2 with sample input" (part-2 sample) 4)))

  (run-tests suite))
