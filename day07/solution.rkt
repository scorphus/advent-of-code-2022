; This file is part of Advent of Code 2022
; https://github.com/scorphus/advent-of-code-2022

; Licensed under the BSD-3-Clause license:
; https://opensource.org/licenses/BSD-3-Clause
; Copyright (c) 2022, Pablo S. Blum de Aguiar <scorphus@gmail.com>

; Day 7 - https://adventofcode.com/2022/day/7

#lang racket/base

(require racket/file
         racket/function
         racket/list
         racket/match
         racket/string)

(module* main #f
  (let ([lines (file->lines "input.txt")])
    (printf "part 1: ~a~n" (part-1 lines))
    (printf "part 2: ~a~n" (part-2 lines))))

(define max-size 100000)
(define total-space 70000000)
(define free-space 30000000)

(define (part-1 lines)
  (for/sum ([size (hash-values (parse-tree lines))] #:when (<= size max-size)) size))

(define (part-2 lines)
  (let ([tree (parse-tree lines)])
    (apply min
           (filter (curryr >= (- free-space (- total-space (hash-ref tree '("/")))))
                   (hash-values tree)))))

(define (parse-tree lines (tree #hash()) (path '()))
  (if (null? lines)
      tree
      (match (string-split (first lines) " ")
        [(list (or "$" "dir") _) (parse-tree (rest lines) tree path)]
        [(list "$" "cd" "..") (parse-tree (rest lines) tree (rest path))]
        [(list "$" "cd" dir) (parse-tree (rest lines) tree (cons dir path))]
        [(list size _)
         (parse-tree (rest lines) (update-tree tree path (string->number size)) path)])))

(define (update-tree tree path size)
  (if (null? path) tree (update-tree (hash-update tree path (curry + size) 0) (rest path) size)))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define sample
    (list "$ cd /"
          "$ ls"
          "dir a"
          "14848514 b.txt"
          "8504156 c.dat"
          "dir d"
          "$ cd a"
          "$ ls"
          "dir e"
          "29116 f"
          "2557 g"
          "62596 h.lst"
          "$ cd e"
          "$ ls"
          "584 i"
          "$ cd .."
          "$ cd .."
          "$ cd d"
          "$ ls"
          "4060174 j"
          "8033020 d.log"
          "5626152 d.ext"
          "7214296 k"))

  (define suite
    (test-suite "day 7 tests"
                (test-equal? "part 1 with sample input" (part-1 sample) 95437)
                (test-equal? "part 2 with sample input" (part-2 sample) 24933642)))

  (run-tests suite))
