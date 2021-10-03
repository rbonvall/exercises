#lang racket

(require "programs.rkt")
(require rackunit)

(test-case "Example"
  (check-equal? (+ 1 1) 2)
)
