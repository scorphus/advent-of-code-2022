; This file is part of Advent of Code 2022
; https://github.com/scorphus/advent-of-code-2022

; Licensed under the BSD-3-Clause license:
; https://opensource.org/licenses/BSD-3-Clause
; Copyright (c) 2022, Pablo S. Blum de Aguiar <scorphus@gmail.com>

; Day 5 - https://adventofcode.com/2022/day/5

#lang racket/base

(require racket/file
         racket/list
         racket/string
         racket/vector)

(module* main #f
  (let ([lines (file->lines "input.txt")])
    (printf "part 1: ~a~n" (part-1 lines))
    (printf "part 2: ~a~n" (part-2 lines))))

(define (part-1 lines)
  (let* ([stacks (list->vector (parse-stacks lines))]
         [procedure (parse-procedure (drop lines (+ (length (vector-argmax length stacks)) 2)))])
    (for ([step procedure])
      (let*-values ([(qty fro to) (apply values step)]
                    [(fro to) (apply values (map sub1 (list fro to)))])
        (for ([_ (in-range qty)])
          (vector-set! stacks to (cons (first (vector-ref stacks fro)) (vector-ref stacks to)))
          (vector-set! stacks fro (rest (vector-ref stacks fro))))))
    (list->string (vector->list (vector-map first stacks)))))

(define (part-2 lines)
  (let* ([stacks (list->vector (parse-stacks lines))]
         [procedure (parse-procedure (drop lines (+ (length (vector-argmax length stacks)) 2)))])
    (for ([step procedure])
      (let*-values ([(qty fro to) (apply values step)]
                    [(fro to) (apply values (map sub1 (list fro to)))]
                    [(crates) (for/fold ([crates '()]) ([_ (in-range qty)])
                                (let ([crates (cons (first (vector-ref stacks fro)) crates)])
                                  (vector-set! stacks fro (rest (vector-ref stacks fro)))
                                  crates))])
        (for ([crate crates])
          (vector-set! stacks to (cons crate (vector-ref stacks to))))))
    (list->string (vector->list (vector-map first stacks)))))

(define (parse-stacks rows (stacks '()))
  (if (equal? "" (second rows))
      (map reverse stacks)
      (parse-stacks (rest rows) (parse-row (string->list (first rows)) stacks))))

(define (parse-row row stacks (new-stacks '()))
  (let*-values ([(crate) (second (take row 3))]
                [(stacks stack)
                 (if (empty? stacks) (values '() '()) (values (rest stacks) (first stacks)))]
                [(new-stacks) (cons (if (equal? #\  crate) stack (cons crate stack)) new-stacks)])
    (if (null? (drop row 3)) (reverse new-stacks) (parse-row (drop row 4) stacks new-stacks))))

(define (parse-procedure lines (procedure '()))
  (if (null? lines)
      (reverse procedure)
      (let*-values ([(_m qty _f fro _t to)
                     (apply values (map string->number (string-split (first lines) " ")))])
        (parse-procedure (rest lines) (cons (list qty fro to) procedure)))))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define sample
    (list "    [D]    "
          "[N] [C]    "
          "[Z] [M] [P]"
          " 1   2   3 "
          ""
          "move 1 from 2 to 1"
          "move 3 from 1 to 3"
          "move 2 from 2 to 1"
          "move 1 from 1 to 2"))

  (define suite
    (test-suite "day 5 tests"
                (test-equal? "part 1 with sample input" (part-1 sample) "CMZ")
                (test-equal? "part 2 with sample input" (part-2 sample) "MCD")))

  (run-tests suite))
