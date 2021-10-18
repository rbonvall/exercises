#lang racket

(require minikanren)
(require (except-in rackunit fail))

(test-case "Chapter 1"
  (check-equal? (run* (q) fail)           '())
  (check-equal? (run* (q) (== 'pea 'pod)) '())
  (check-equal? (run* (q) (==    q 'pea)) '(pea))
  (check-equal? (run* (q) (== 'pea    q)) '(pea))
  (check-equal? (run* (q) succeed)        '(_.0))
  (check-equal? (run* (q) (== 'pea 'pea)) '(_.0))
  (check-equal? (run* (q) (==    q    q)) '(_.0))

  (check-equal? (run* (q)
                  (fresh (x)
                    (== 'pea q)))         '(pea))

  (check-equal? (run* (q)
                  (fresh (x)
                    (== 'pea x)))         '(_.0))

  (check-equal? (run* (q)
                  (fresh (x)
                    (== (cons x '()) q))) '((_.0)))

  (check-equal? (run* (q)
                  (fresh (x)
                    (== `(,x) q)))        '((_.0)))
  ; `(,x) is a shorthand for (cons x '()).
  ; Commas in a backtick expression can only precede variables.
  ; What is not a variable behaves as if it were quoted.

)
