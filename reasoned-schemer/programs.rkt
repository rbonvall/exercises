#lang racket

(provide (all-defined-out))

; From https://www.monolune.com/using-racket-for-the-reasoned-schemer/
(require minikanren)
(define succeed (== #t #t))
(define fail    (== #t #f))
(define else succeed)
(current-readtable
  (make-readtable (current-readtable)
                  #\s
                  'dispatch-macro
                  (lambda (a b c d e f) succeed)))
(current-readtable
  (make-readtable (current-readtable)
                  #\u
                  'dispatch-macro
                  (lambda (a b c d e f) fail)))



