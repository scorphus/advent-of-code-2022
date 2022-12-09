; This file is part of Advent of Code 2022
; https://github.com/scorphus/advent-of-code-2022

; Licensed under the BSD-3-Clause license:
; https://opensource.org/licenses/BSD-3-Clause
; Copyright (c) 2022, Pablo S. Blum de Aguiar <scorphus@gmail.com>

; Day 9 - https://adventofcode.com/2022/day/9

#lang racket/base

(require racket/file
         racket/list
         racket/match
         racket/set
         racket/string)

(module* main #f
  (let ([lines (file->lines "input.txt")])
    (printf "part 1: ~a~n" (part-1 lines))
    (printf "part 2: ~a~n" (part-2 lines))))

(define (part-1 lines)
  (set-count (for/fold ([hx 0] [hy 0] [tx 0] [ty 0] [visited (set '(0 . 0))] #:result visited)
                       ([motion (parse-motions lines)])
               (let*-values ([(hx hy) (values (+ hx (car motion)) (+ hy (cdr motion)))]
                             [(tx ty) (move-tail hx hy tx ty)])
                 (values hx hy tx ty (set-add visited (cons tx ty)))))))

(define (part-2 lines)
  (set-count
   (for/fold ([hx 0] [hy 0] [knots (make-list 9 '(0 . 0))] [visited (set '(0 . 0))] #:result visited)
             ([motion (parse-motions lines)])
     (let* ([hx (+ hx (car motion))] [hy (+ hy (cdr motion))] [knots (move-knots knots hx hy)])
       (values hx hy knots (set-add visited (last knots)))))))

(define (parse-motions lines)
  (let ([deltas (hash "U" '(0 . 1) "D" '(0 . -1) "R" '(1 . 0) "L" '(-1 . 0))])
    (apply append
           (for/list ([line lines])
             (let*-values ([(direction steps) (apply values (string-split line " "))])
               (make-list (string->number steps) (hash-ref deltas direction)))))))

(define (move-tail hx hy tx ty)
  (match (cons (- hx tx) (- hy ty))
    [(cons dx dy)
     #:when (< (max (abs dx) (abs dy)) 2)
     (values tx ty)]
    [(cons dx 0) (values (+ tx (/ dx (abs dx))) ty)]
    [(cons 0 dy) (values tx (+ ty (/ dy (abs dy))))]
    [(cons dx dy) (values (+ tx (/ dx (abs dx))) (+ ty (/ dy (abs dy))))]))

(define (move-knots knots hx hy)
  (cond
    [(empty? knots) '()]
    [else
     (let-values ([(tx ty) (move-tail hx hy (car (first knots)) (cdr (first knots)))])
       (cons (cons tx ty) (move-knots (rest knots) tx ty)))]))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define sample1 '("R 4" "U 4" "L 3" "D 1" "R 4" "D 1" "L 5" "R 2"))
  (define sample2 '("R 5" "U 8" "L 8" "D 3" "R 17" "D 10" "L 25" "U 20"))

  (define suite
    (test-suite "day 9 tests"
                (test-equal? "part 1 with sample input 1" (part-1 sample1) 13)
                (test-equal? "part 1 with sample input 2" (part-1 sample2) 88)
                (test-equal? "part 2 with sample input 1" (part-2 sample1) 1)
                (test-equal? "part 2 with sample input 2" (part-2 sample2) 36)))

  (run-tests suite))
