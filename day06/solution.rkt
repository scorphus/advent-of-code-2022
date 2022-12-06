; This file is part of Advent of Code 2022
; https://github.com/scorphus/advent-of-code-2022

; Licensed under the BSD-3-Clause license:
; https://opensource.org/licenses/BSD-3-Clause
; Copyright (c) 2022, Pablo S. Blum de Aguiar <scorphus@gmail.com>

; Day 6 - https://adventofcode.com/2022/day/6

#lang racket/base

(require racket/file
         racket/list
         racket/set)

(module* main #f
  (let ([lines (file->lines "input.txt")])
    (printf "part 1: ~a~n" (part-1 lines))
    (printf "part 2: ~a~n" (part-2 lines))))

(define (part-1 lines)
  (for/fold ([a #\ ] [b #\ ] [c #\ ] [i 1] #:result i)
            ([d (string->list (first lines))] #:break (and (> i 3) (= (set-count (set a b c d)) 4)))
    (values b c d (add1 i))))

(define (part-2 lines)
  (define line (first lines))
  (for/first ([i (in-range (- (string-length line) 14))]
              #:do [(define substr (substring line i (+ i 14)))]
              #:when (eq? 14 (set-count (list->set (string->list substr)))))
    (+ i 14)))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define cases
    (list '("mjqjpqmgbljsphdztnvjfqwrcgsmlb" 7 19)
          '("bvwbjplbgvbhsrlpgdmjqwftvncz" 5 23)
          '("nppdvjthqldpwncqszvftbrmjlhg" 6 23)
          '("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 10 29)
          '("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 11 26)
          '("qwertyuiopasdfqwertyuiopasdf" 4 14)))

  (define suite-part-1
    (test-suite "day 6 > part-1 tests"
                (for ([case cases] [i (in-range 1 (add1 (length cases)))])
                  (let*-values ([(sample expected _) (apply values case)])
                    (test-equal? (format "sampled case ~a" i) (part-1 (list sample)) expected)))))

  (run-tests suite-part-1)

  (define suite-part-2
    (test-suite "day 6 > part-2 tests"
                (for ([case cases] [i (in-range 1 (add1 (length cases)))])
                  (let*-values ([(sample _ expected) (apply values case)])
                    (test-equal? (format "sampled case ~a" i) (part-2 (list sample)) expected)))))

  (run-tests suite-part-2))
