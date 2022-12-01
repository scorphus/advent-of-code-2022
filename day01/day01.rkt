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
  (let ([lines (file->lines "input01.txt")])
    (printf "part 1: ~a~n" (part-1 lines))
    (printf "part 2: ~a~n" (part-2 lines))))

(define (part-1 lines)
  (first (sort (for/fold ([sums (list 0)] #:result sums) ([l lines])
                 (if (equal? "" l)
                     (cons 0 sums)
                     (let* ([sum (first sums)] [sums (cdr sums)])
                       (cons (+ (string->number l) sum) sums))))
               >)))

(define (part-2 lines)
  (apply +
         (take (sort (for/fold ([sums (list 0)] #:result sums) ([l lines])
                       (if (equal? "" l)
                           (cons 0 sums)
                           (let* ([sum (first sums)] [sums (cdr sums)])
                             (cons (+ (string->number l) sum) sums))))
                     >)
               3)))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define sample
    (list "1000" "2000" "3000" "" "4000" "" "5000" "6000" "" "7000" "8000" "9000" "" "10000"))

  (define suite
    (test-suite "day 01 tests"
                (test-equal? "part 1 with sample input" (part-1 sample) 24000)
                (test-equal? "part 2 with sample input" (part-2 sample) 45000)))

  (run-tests suite))
