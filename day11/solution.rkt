; This file is part of Advent of Code 2022
; https://github.com/scorphus/advent-of-code-2022

; Licensed under the BSD-3-Clause license:
; https://opensource.org/licenses/BSD-3-Clause
; Copyright (c) 2022, Pablo S. Blum de Aguiar <scorphus@gmail.com>

; Day 11 - https://adventofcode.com/2022/day/11

#lang racket/base

(require racket/date
         racket/file
         racket/function
         racket/hash
         racket/list
         racket/match
         racket/set
         racket/stream
         racket/string
         racket/vector)

(module* main #f
  (let ([lines (file->lines "input.txt")])
    (printf "part 1: ~a~n" (part-1 lines))
    (printf "part 2: ~a~n" (part-2 lines))))

(define (part-1 lines)
  (for/fold ([monkeys (parse-monkeys lines)]
             #:result
             (apply * (take (sort (map (curryr hash-ref "insp") (hash-values monkeys)) >) 2)))
            ([_ (in-range 20)])
    (take-turn monkeys)))

(define (part-2 lines)
  (let* ([monkeys (parse-monkeys lines)]
         [divider (apply lcm (map (curryr hash-ref "div") (hash-values monkeys)))]
         [proc (curryr remainder divider)])
    (for/fold ([monkeys monkeys]
               #:result
               (apply * (take (sort (map (curryr hash-ref "insp") (hash-values monkeys)) >) 2)))
              ([_ (in-range 10000)])
      (take-turn monkeys proc))))

(define (parse-monkeys lines)
  (for/fold ([monkeys '()] [monkey #hash()] #:result (list->hash (reverse (cons monkey monkeys))))
            ([line lines])
    (match (string-split (string-replace line "," ""))
      [(list "Starting" _ numbers ...) ; wow items
       (values monkeys (hash-set monkey "out" (map string->number numbers)))]
      [(list "Operation:" _ ... "*" "old") ; much square
       (values monkeys (hash-set monkey "op" (curryr expt 2)))]
      [(list "Operation:" _ ... "*" n) ; very mul
       (values monkeys (hash-set monkey "op" (curryr * (string->number n))))]
      [(list "Operation:" _ ... n) ; many add
       (values monkeys (hash-set monkey "op" (curryr + (string->number n))))]
      [(list "Test:" _ ... div) ; so test
       (values monkeys (hash-set monkey "div" (string->number div)))]
      [(list "If" "true:" _ ... to) ; much true
       (values monkeys (hash-set monkey #t (string->number to)))]
      [(list "If" _ ... to) ; much false
       (values monkeys (hash-set monkey #f (string->number to)))]
      ['() (values (cons monkey monkeys) #hash())] ;-D new monkey, yay!
      [_ (values monkeys monkey)])))

(define (list->hash l (i 0) (h #hash()))
  (if (null? l) h (list->hash (rest l) (add1 i) (hash-set h i (first l)))))

(define (take-turn monkeys (proc (curryr quotient 3)) (i 0))
  (cond
    [(= (hash-count monkeys) i) monkeys]
    [(and (null? (hash-ref (hash-ref monkeys i) "out"))
          (null? (hash-ref (hash-ref monkeys i) "in" '())))
     (take-turn monkeys proc (add1 i))]
    [(null? (hash-ref (hash-ref monkeys i) "out"))
     (let* ([boss (hash-ref monkeys i)]
            [boss (hash-set boss "out" (reverse (hash-ref boss "in" '())))]
            [boss (hash-set boss "in" '())])
       (take-turn (hash-set monkeys i boss) proc i))]
    [else
     (let* ([boss (hash-ref monkeys i)]
            [item (proc ((hash-ref boss "op") (first (hash-ref boss "out"))))]
            [boss (hash-update (hash-update boss "out" rest) "insp" add1 0)]
            [j (hash-ref boss (= 0 (remainder item (hash-ref boss "div"))))]
            [chip (hash-update (hash-ref monkeys j) "in" (curry cons item) '())])
       (take-turn (hash-set (hash-set monkeys i boss) j chip) proc i))]))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define sample
    (list "Monkey 0:"
          "  Starting items: 79, 98"
          "  Operation: new = old * 19"
          "  Test: divisible by 23"
          "    If true: throw to monkey 2"
          "    If false: throw to monkey 3"
          ""
          "Monkey 1:"
          "  Starting items: 54, 65, 75, 74"
          "  Operation: new = old + 6"
          "  Test: divisible by 19"
          "    If true: throw to monkey 2"
          "    If false: throw to monkey 0"
          ""
          "Monkey 2:"
          "  Starting items: 79, 60, 97"
          "  Operation: new = old * old"
          "  Test: divisible by 13"
          "    If true: throw to monkey 1"
          "    If false: throw to monkey 3"
          ""
          "Monkey 3:"
          "  Starting items: 74"
          "  Operation: new = old + 3"
          "  Test: divisible by 17"
          "    If true: throw to monkey 0"
          "    If false: throw to monkey 1"))

  (define suite
    (test-suite "day 11 tests"
                (test-equal? "part 1 with sample input" (part-1 sample) 10605)
                (test-equal? "part 2 with sample input" (part-2 sample) 2713310158)))

  (run-tests suite))
