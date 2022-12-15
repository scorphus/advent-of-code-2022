; This file is part of Advent of Code 2022
; https://github.com/scorphus/advent-of-code-2022

; Licensed under the BSD-3-Clause license:
; https://opensource.org/licenses/BSD-3-Clause
; Copyright (c) 2022, Pablo S. Blum de Aguiar <scorphus@gmail.com>

; Day 13 - https://adventofcode.com/2022/day/13

#lang racket/base

(require racket/file
         racket/function
         racket/list
         racket/match
         racket/port
         racket/string)

(module* main #f
  (let ([lines (file->lines "input.txt")])
    (printf "part 1: ~a~n" (part-1 lines))
    (printf "part 2: ~a~n" (part-2 lines))))

(define (part-1 lines)
  (for/fold ([left #f] [sum 0] [index 1] #:result sum) ([right (parse-packets lines)])
    (cond
      [(not left) (values right sum index)]
      [(packet<? left right) (values #f (+ sum index) (add1 index))]
      [else (values #f sum (add1 index))])))

(define (part-2 lines)
  (define dvider-2 '((2)))
  (define dvider-6 '((6)))
  (let ([packets (sort (cons dvider-2 (cons dvider-6 (parse-packets lines))) packet<?)])
    (* (add1 (index-of packets dvider-2)) (add1 (index-of packets dvider-6)))))

(define (parse-packets lines)
  (map (compose (curryr call-with-input-string read) (curryr string-replace "," " "))
       (filter (λ (l) (not (equal? l ""))) lines)))

(define (packet<? left right)
  (cond
    [(and (null? left) (null? right)) '?]
    [(null? left) #t]
    [(null? right) #f]
    [(and (list? left) (list? right))
     (match (packet<? (first left) (first right))
       ['? (packet<? (rest left) (rest right))]
       [result result])]
    [(list? left) (packet<? left (list right))]
    [(list? right) (packet<? (list left) right)]
    [(= left right) '?]
    [else (< left right)]))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define sample
    (list "[1,1,3,1,1]"
          "[1,1,5,1,1]"
          ""
          "[[1],[2,3,4]]"
          "[[1],4]"
          ""
          "[9]"
          "[[8,7,6]]"
          ""
          "[[4,4],4,4]"
          "[[4,4],4,4,4]"
          ""
          "[7,7,7,7]"
          "[7,7,7]"
          ""
          "[]"
          "[3]"
          ""
          "[[[]]]"
          "[[]]"
          ""
          "[1,[2,[3,[4,[5,6,7]]]],8,9]"
          "[1,[2,[3,[4,[5,6,0]]]],8,9]"))

  (define suite
    (test-suite "day 13 tests"
                (test-equal? "part 1 with sample input" (part-1 sample) 13)
                (test-equal? "part 2 with sample input" (part-2 sample) 140)))

  (run-tests suite))
