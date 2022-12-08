; This file is part of Advent of Code 2022
; https://github.com/scorphus/advent-of-code-2022

; Licensed under the BSD-3-Clause license:
; https://opensource.org/licenses/BSD-3-Clause
; Copyright (c) 2022, Pablo S. Blum de Aguiar <scorphus@gmail.com>

; Day 8 - https://adventofcode.com/2022/day/8

#lang racket/base

(require racket/file
         racket/function
         racket/string)

(module* main #f
  (let ([lines (file->lines "input.txt")])
    (printf "part 1: ~a~n" (part-1 lines))
    (printf "part 2: ~a~n" (part-2 lines))))

(define deltas-nesw '((0 . -1) (1 . 0) (0 . 1) (-1 . 0)))

(define (part-1 lines)
  (let ([trees (parse-trees lines)])
    (for*/sum ([i (length lines)]
               [j (length lines)]
               #:when (for/first ([deltas deltas-nesw]
                                  #:when (visible? trees i j (car deltas) (cdr deltas)))
                        #t))
              1)))

(define (part-2 lines)
  (let ([trees (parse-trees lines)])
    (for*/fold ([max-score 0]) ([i (length lines)] [j (length lines)])
      (max max-score (score trees i j)))))

(define (parse-trees lines)
  (list->vector
   (map (compose list->vector (curry map string->number) (curryr string-split #rx"(?<=.)(?=.)"))
        lines)))

(define (visible? trees i j di dj (steps 1))
  (let* ([ni (+ i (* di steps))] [nj (+ j (* dj steps))])
    (if (and (< -1 ni (vector-length trees)) (< -1 nj (vector-length trees)))
        (if (< (height trees ni nj) (height trees i j)) (visible? trees i j di dj (add1 steps)) #f)
        #t)))

(define (height trees i j)
  (vector-ref (vector-ref trees i) j))

(define (score trees i j)
  (for/product ([deltas deltas-nesw]) (distance trees i j (car deltas) (cdr deltas))))

(define (distance trees i j di dj (steps 1))
  (let* ([ni (+ i (* di steps))] [nj (+ j (* dj steps))])
    (if (and (< -1 ni (vector-length trees)) (< -1 nj (vector-length trees)))
        (if (< (height trees ni nj) (height trees i j)) (distance trees i j di dj (add1 steps)) steps)
        (sub1 steps))))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define sample (list "30373" "25512" "65332" "33549" "35390"))

  (define suite
    (test-suite "day 8 tests"
                (test-equal? "part 1 with sample input" (part-1 sample) 21)
                (test-equal? "part 2 with sample input" (part-2 sample) 8)))

  (run-tests suite))
