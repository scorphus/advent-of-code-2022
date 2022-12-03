; This file is part of Advent of Code 2022
; https://github.com/scorphus/advent-of-code-2022

; Licensed under the BSD-3-Clause license:
; https://opensource.org/licenses/BSD-3-Clause
; Copyright (c) 2022, Pablo S. Blum de Aguiar <scorphus@gmail.com>

; Day 3 - https://adventofcode.com/2022/day/3

#lang racket/base

(require racket/file
         racket/list
         racket/set)

(module* main #f
  (let ([lines (file->lines "input.txt")])
    (printf "part 1: ~a~n" (part-1 lines))
    (printf "part 2: ~a~n" (part-2 lines))))

(define (part-1 lines)
  (for/sum
   ([line lines])
   (let* ([rucksack-1 (list->set (string->list (substring line 0 (/ (string-length line) 2))))]
          [rucksack-2 (list->set (string->list (substring line (/ (string-length line) 2))))])
     (calculate-priority (set-first (set-intersect rucksack-1 rucksack-2))))))

(define (part-2 lines)
  (group-priority lines 0))

(define (calculate-priority item-type)
  (let* ([priority (char->integer item-type)]
         [lower-diff (- (char->integer #\a) 1)]
         [upper-diff (- (char->integer #\A) 27)])
    (if (char<=? #\a item-type #\z) (- priority lower-diff) (- priority upper-diff))))

(define (group-priority lines priority)
  (if (null? lines)
      priority
      (let* ([group-set (map (compose list->set string->list) (take lines 3))]
             [item-type (set-first (apply set-intersect group-set))])
        (group-priority (drop lines 3) (+ priority (calculate-priority item-type))))))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define sample
    (list "vJrwpWtwJgWrhcsFMMfFFhFp"
          "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
          "PmmdzqPrVvPwwTWBwg"
          "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
          "ttgJtRGJQctTZtZT"
          "CrZsJsPPZsGzwwsLwLmpwMDw"))

  (define suite
    (test-suite "day 3 tests"
                (test-equal? "part 1 with sample input" (part-1 sample) 157)
                (test-equal? "part 2 with sample input" (part-2 sample) 70)))

  (run-tests suite))
