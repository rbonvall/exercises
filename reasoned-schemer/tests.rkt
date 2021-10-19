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

  (check-equal? (run* (q)
                  (fresh (x)
                    (== x q)))           '(_.0))
  ; x and q are fused, but remain fresh.
  ; They get the same association.

  (check-equal? (run* (q)
                  (== '(((pea)) pod)
                      '(((pea)) pod)))   '(_.0))

  (check-equal? (run* (q)
                  (== '(((pea)) pod)
                      `(((pea))  ,q)))   '(pod))

  (check-equal? (run* (q)
                  (== `((( ,q)) pod)
                      '(((pea)) pod)))   '(pea))

  (check-equal? (run* (q)
                  (fresh (x)
                    (== `(((,q)) pod)
                        `(((,x)) pod)))) '(_.0))
  ; q remains fresh, even though x is fused with it.

  (check-equal? (run* (q)
                  (fresh (x)
                    (== `(((,q))  ,x)
                        `(((,x)) pod)))) '(pod))
  ; pod is associated with x, and x is fused with q.

  (check-equal? (run* (q)
                  (fresh (x)
                    (== `(,x ,x) q)))    '((_.0 _.0)))
  ; Every instance of the same fresh variable
  ; is replaced by the same reified variable.

  (check-equal? (run* (q)
                  (fresh (x)
                    (fresh (y)
                      (== `(,q      ,y)
                          `((,x ,y) ,x))))) '((_.0 _.0)))
  ; The value of `(,x ,y) is associated with q.
  ; y is fused with x, making them the same.

  (check-equal? (run* (q)
                  (fresh (x)
                    (fresh (y)
                      (== `(,x ,y) q)))) '((_.0 _.1)))
  ; Each different variable is reified with a different subscript.

  (check-equal? (run* (q)
                  (fresh (x)
                    (fresh (y)
                      (== `(,x ,y ,x) q)))) '((_.0 _.1 _.0)))

)
