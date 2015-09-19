#lang racket

(require "programs.rkt")
(require rackunit)

(test-case "Chapter 1"
  (check-true  (atom? 'atom))
  (check-true  (atom? 1492))
  (check-true  (atom? '*abc?))
  (check-false (atom? '()))
  (check-false (atom? '(1 2 3))))

(test-case "Chapter 2"
  (check-true  (lat? '(Jack Sprat could eat no chicken fat)))
  (check-false (lat? '((Jack) Sprat could eat no chicken fat)))
  (check-false (lat? '(Jack (Sprat could) eat no chicken fat)))
  (check-true  (lat? '()))

  (check-true  (member? 'tea     '(coffee tea or milk)))
  (check-false (member? 'poached '(fried eggs and scrambled eggs)))
  (check-true  (member? 'meat    '(mashed potatoes and meat gravy)))
  (check-false (member? 'liver   '(bagels and lox))))


