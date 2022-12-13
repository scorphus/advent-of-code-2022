; This file is part of Advent of Code 2022
; https://github.com/scorphus/advent-of-code-2022

; Licensed under the BSD-3-Clause license:
; https://opensource.org/licenses/BSD-3-Clause
; Copyright (c) 2022, Pablo S. Blum de Aguiar <scorphus@gmail.com>

; Day 12 - https://adventofcode.com/2022/day/12

#lang racket/base

(require racket/file
         racket/function
         racket/list
         racket/set
         racket/string)

(module* main #f
  (let ([lines (file->lines "input.txt")])
    (printf "part 1: ~a~n" (part-1 lines))
    (printf "part 2: ~a~n" (part-2 lines))))

(define S (char->integer #\S))
(define E (char->integer #\E))
(define E-decoy (add1 (char->integer #\z)))
(define E-decoy-str (list->string (list (integer->char E-decoy))))
(define deltas-nesw '((0 -1) (1 0) (0 1) (-1 0)))

(define (part-1 lines)
  (let* ([temp-heights (parse-heights lines)]
         [start (heights-find temp-heights S)]
         [heights (parse-heights
                   (map (λ (l) (string-replace (string-replace l "E" E-decoy-str) "S" "a")) lines))])
    (shortest-path heights start E-decoy (λ (h1 h2) (<= (- h1 h2) 1)))))

(define (part-2 lines)
  (let* ([temp-heights (parse-heights lines)]
         [start (heights-find temp-heights E)]
         [heights (parse-heights
                   (map (λ (l) (string-replace (string-replace l "E" "z") "S" "a")) lines))])
    (shortest-path heights start (char->integer #\a) (λ (h1 h2) (<= (- h2 h1) 1)))))

(define (parse-heights lines)
  (list->vector (map (compose list->vector (curry map char->integer) string->list) lines)))

(define (heights-find heights value (row 0) (col 0))
  (cond
    [(>= row (vector-length heights)) '()]
    [(>= col (vector-length (vector-ref heights 0))) (heights-find heights value (add1 row))]
    [(= (heights-get heights (list row col)) value) (list row col)]
    [else (heights-find heights value row (add1 col))]))

(define (heights-get heights position)
  (cond
    [(or (>= (first position) (vector-length heights)) (< (first position) 0)) 0]
    [else
     (let ([row (vector-ref heights (first position))])
       (cond
         [(or (>= (second position) (vector-length row)) (< (second position) 0)) 0]
         [else (vector-ref row (second position))]))]))

(define (shortest-path heights
                       start
                       goal
                       reachable?
                       (next-sq (list (cons start 0)))
                       (nexxt-sq '())
                       (seen (set start)))
  (cond
    [(and (null? next-sq) (null? nexxt-sq)) -1]
    [(null? next-sq) (shortest-path heights start goal reachable? (reverse nexxt-sq) '() seen)]
    [else
     (let* ([curr (car (first next-sq))]
            [steps (cdr (first next-sq))]
            [height (heights-get heights curr)])
       (if (= height goal)
           steps
           (let* ([neigh (filter (λ (pos)
                                   (and (reachable? (heights-get heights pos) height)
                                        (not (set-member? seen pos))))
                                 (for*/list ([pos (list curr)] [d deltas-nesw])
                                   (map + pos d)))]
                  [nexxt-sq (append (for*/list ([n neigh] [s (list steps)])
                                      (cons n (add1 s)))
                                    nexxt-sq)]
                  [seen (set-union seen (list->set neigh))])
             (shortest-path heights curr goal reachable? (rest next-sq) nexxt-sq seen))))]))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define sample (list "Sabqponm" "abcryxxl" "accszExk" "acctuvwj" "abdefghi"))

  (define suite
    (test-suite "day 12 tests"
                (test-equal? "part 1 with sample input" (part-1 sample) 31)
                (test-equal? "part 2 with sample input" (part-2 sample) 29)))

  (run-tests suite))
