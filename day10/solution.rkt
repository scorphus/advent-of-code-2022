; This file is part of Advent of Code 2022
; https://github.com/scorphus/advent-of-code-2022

; Licensed under the BSD-3-Clause license:
; https://opensource.org/licenses/BSD-3-Clause
; Copyright (c) 2022, Pablo S. Blum de Aguiar <scorphus@gmail.com>

; Day 10 - https://adventofcode.com/2022/day/10

#lang racket/base

(require racket/file
         racket/function
         racket/list
         racket/match
         racket/string)

(define CRT-WIDTH 40)
(define SPRITE-WIDTH 3)

(module* main #f
  (let ([lines (file->lines "input.txt")])
    (printf "part 1: ~a~n" (part-1 lines))
    (printf "part 2: ~n~a~n" (part-2 lines))))

(define (part-1 lines)
  (for/fold ([cycle 1] [register 1] [sum 0] #:result sum)
            ([instr (map (curryr string-split " ") lines)])
    (let*-values ([(sum) (if (= 0 (remainder (- cycle (/ CRT-WIDTH 2)) CRT-WIDTH))
                             (+ sum (* cycle register))
                             sum)]
                  [(cycle register sum _) (run-instruction instr (add1 cycle) register #:sum sum)])
      (values cycle register sum))))

(define (part-2 lines)
  (for/fold ([cycle 1] [register 1] [crt '()] #:result (reveal-crt (reverse crt)))
            ([instr (map (curryr string-split " ") lines)])
    (let*-values ([(crt) (set-pixel crt cycle register)]
                  [(cycle register _ crt) (run-instruction instr (add1 cycle) register #:crt crt)])
      (values cycle register crt))))

(define (run-instruction instr cycle register #:sum [sum 0] #:crt [crt '()])
  (match instr
    [(list _ n)
     (values (add1 cycle)
             (+ register (string->number n))
             (if (= 0 (remainder (- cycle 20) CRT-WIDTH)) (+ sum (* cycle register)) sum)
             (set-pixel crt cycle register))]
    [_ (values cycle register sum crt)]))

(define (set-pixel crt cycle register)
  (let* ([position (remainder (sub1 cycle) CRT-WIDTH)]
         [pixel (if (<= (/ SPRITE-WIDTH -2) (- register position) (/ SPRITE-WIDTH 2)) "#" ".")])
    (values (cons pixel crt))))

(define (reveal-crt crt (res '()))
  (if (null? crt)
      (string-join (reverse res) "\n")
      (reveal-crt (drop crt CRT-WIDTH) (cons (string-join (take crt CRT-WIDTH) "") res))))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define sample (file->lines "sample.txt"))

  (define suite
    (test-suite
     "day 10 tests"
     (test-equal? "part 1 with sample input" (part-1 sample) 13140)
     (test-equal?
      "part 2 with sample input"
      (part-2 sample)
      #<<expected
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....
expected
      )))

  (run-tests suite))
