#lang racket

(require minikanren)
(provide (all-defined-out))

; From https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd/blob/master/trs2-impl.scm

(define (conj2 g1 g2)
  (lambda (s)
    (append-map-inf g2 (g1 s))))

(define (disj2 g1 g2)
  (lambda (s)
    (append-inf (g1 s) (g2 s))))

(define (append-inf s-inf t-inf)
  (cond
    ((null? s-inf) t-inf)
    ((pair? s-inf) 
     (cons (car s-inf)
       (append-inf (cdr s-inf) t-inf)))
    (else (lambda () 
            (append-inf t-inf (s-inf))))))

(define (append-map-inf g s-inf)
  (cond
    ((null? s-inf) '())
    ((pair? s-inf)
     (append-inf (g (car s-inf))
       (append-map-inf g (cdr s-inf))))
    (else (lambda () 
            (append-map-inf g (s-inf))))))

