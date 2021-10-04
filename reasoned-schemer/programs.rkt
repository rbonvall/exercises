#lang racket

(provide (all-defined-out))

; From https://www.monolune.com/using-racket-for-the-reasoned-schemer/
(require minikanren)
(define SUCCEED (== #t #t))
(define FAIL    (== #t #f))
(define else SUCCEED)
(current-readtable
  (make-readtable (current-readtable)
                  #\s
                  'dispatch-macro
                  (lambda (a b c d e f) SUCCEED)))
(current-readtable
  (make-readtable (current-readtable)
                  #\u
                  'dispatch-macro
                  (lambda (a b c d e f) FAIL)))



