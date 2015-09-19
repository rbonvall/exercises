#lang racket

(provide (all-defined-out))

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (lat? l)
  (or (null? l)
      (and (atom? (car l))
           (lat?  (cdr l)))))

(define (member? a lat)
  (and (not (null? lat))
       (or (eq?     a (car lat))
           (member? a (cdr lat)))))

(define (rember a lat)
  (cond
    [(null? lat)       '()]
    [(eq? a (car lat)) (cdr lat)]
    [else              (cons (car lat)
                             (rember a (cdr lat)))]))

(define (firsts l)
  (cond
    [(null? l) '()]
    [else      (cons (car    (car l))
                     (firsts (cdr l)))]))

(define (insertR new old lat)
  (cond
    [(null? lat)         '()]
    [(eq? old (car lat)) (cons old
                               (cons new
                                     (cdr lat)))]
    [else                (cons (car lat)
                               (insertR new old (cdr lat)))]))
(define (insertL new old lat)
  (cond
    [(null? lat)         '()]
    [(eq? old (car lat)) (cons new lat)]
    [else                (cons (car lat)
                               (insertL new old (cdr lat)))]))

(define (subst new old lat)
  (cond
    [(null? lat)         '()]
    [(eq? old (car lat)) (cons new (cdr lat))]
    [else                (cons (car lat)
                               (subst new old (cdr lat)))]))
(define (subst2 new o1 o2 lat)
  (cond
    [(null? lat)         '()]
    [(eq? o1 (car lat)) (cons new (cdr lat))]
    [(eq? o2 (car lat)) (cons new (cdr lat))]
    [else                (cons (car lat)
                               (subst2 new o1 o2 (cdr lat)))]))

(define (multirember a lat)
  (cond
    [(null? lat)       '()]
    [(eq? a (car lat)) (multirember a (cdr lat))]
    [else              (cons (car lat)
                             (multirember a (cdr lat)))]))

(define (multiinsertR new old lat)
  (cond
    [(null? lat) '()]
    [(eq? old (car lat)) (cons old
                               (cons new
                                     (multiinsertR new old (cdr lat))))]
    [else                (cons (car lat)
                               (multiinsertR new old (cdr lat)))]))

(define (multiinsertL new old lat)
  (cond
    [(null? lat) '()]
    [(eq? old (car lat)) (cons new
                               (cons old
                                     (multiinsertL new old (cdr lat))))]
    [else                (cons (car lat)
                               (multiinsertL new old (cdr lat)))]))

(define (multisubst new old lat)
  (cond
    [(null? lat) '()]
    [(eq? old (car lat))  (cons new
                                (multisubst new old (cdr lat)))]
    [else                 (cons (car lat)
                                (multisubst new old (cdr lat)))]))

(define (plus a b)
  (cond
    [(zero? a) b]
    [(zero? b) a]
    [else      (plus (add1 a) (sub1 b))]))

(define (minus a b)
  (cond
    [(zero? b) a]
    [else      (minus (sub1 a) (sub1 b))]))

(define (addtup tup)
  (cond
    [(null? tup) 0]
    [else        (plus (car tup) (addtup (cdr tup)))]))

(define (× a b)
  (cond
    [(zero? a) 0]
    [else      (plus (× (sub1 a) b)
                     b)]))

(define (tup+ a b)
  (cond
    [(null? a) b]
    [(null? b) a]
    [else      (cons (plus (car a) (car b))
                     (tup+ (cdr a) (cdr b)))]))

(define (> n m)
  (cond
    [(zero? n) #f]
    [(zero? m) #t]
    [else      (> (sub1 n) (sub1 m))]))

(define (< n m)
  (cond
    [(zero? m) #f]
    [(zero? n) #t]
    [else      (< (sub1 n) (sub1 m))]))

(define (= n m)
  (not (or (< n m) (> n m))))

(define (÷ n m)
  (cond
    [(< n m) 0]
    [else       (add1 (÷ (- n m) m))]))

(define (↑ n m)
  (cond
    [(zero? m) 1]
    [else      (× (↑ n (sub1 m))
                  n)]))

(define (length lat)
  (cond
    [(null? lat) 0]
    [else        (add1 (length (cdr lat)))]))

(define (pick n lat)
  (cond
    [(zero? (sub1 n)) (car lat)]
    [else             (pick (sub1 n) (cdr lat))]))

(define (rempick n lat)
  (cond
    [(zero? (sub1 n)) (cdr lat)]
    [else             (cons (car lat)
                            (rempick (sub1 n) (cdr lat)))]))

(define (no-nums lat)
  (cond
    [(null? lat)         '()]
    [(number? (car lat)) (no-nums (cdr lat))]
    [else                (cons (car lat)
                               (no-nums (cdr lat)))]))

(define (eqan? a1 a2)
  (or (and      (number? a1)       (number? a2)  (=   a1 a2))
      (and (not (number? a1)) (not (number? a2)) (eq? a1 a2))))
