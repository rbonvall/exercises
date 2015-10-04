#lang racket/base

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

(define (occur a lat)
  (cond
    [(null? lat)         0]
    [(eqan? a (car lat)) (add1 (occur a (cdr lat)))]
    [else                (occur a (cdr lat))]))

(define (one? n)
  (zero? (sub1 n)))

(define (one?-based-rempick n lat)
  (cond
    [(one? n) (cdr lat)]
    [else     (cons (car lat)
                    (one?-based-rempick (sub1 n) (cdr lat)))]))

(define (rember* a l)
  (cond
    [(null? l)       '()]
    [(list? (car l)) (cons (rember* a (car l))
                           (rember* a (cdr l)))]
    [(eq? (car l) a) (rember* a (cdr l))]
    [else            (cons (car l)
                           (rember* a (cdr l)))]))

(define (insertR* new old l)
  (cond
    [(null? l) '()]
    [(list? (car l))   (cons (insertR* new old (car l))
                             (insertR* new old (cdr l)))]
    [(eq? (car l) old) (cons old
                             (cons new
                                   (insertR* new old (cdr l))))]
    [else              (cons (car l)
                             (insertR* new old (cdr l)))]))

(define (occur* a l)
  (cond
    [(null? l) 0]
    [(list? (car l)) (plus (occur* a (car l))
                           (occur* a (cdr l)))]
    [(eq? (car l) a) (add1 (occur* a (cdr l)))]
    [else            (occur* a (cdr l))]))

(define (subst* new old l)
  (cond
    [(null? l) '()]
    [(list? (car l))   (cons (subst* new old (car l))
                             (subst* new old (cdr l)))]
    [(eq? (car l) old) (cons new
                             (subst* new old (cdr l)))]
    [else              (cons (car l)
                             (subst* new old (cdr l)))]))

(define (insertL* new old l)
  (cond
    [(null? l) '()]
    [(list? (car l))   (cons (insertL* new old (car l))
                             (insertL* new old (cdr l)))]
    [(eq? (car l) old) (cons new
                             (cons old
                                   (insertL* new old (cdr l))))]
    [else              (cons (car l)
                             (insertL* new old (cdr l)))]))

(define (member* a l)
  (cond
    [(null? l)       #f]
    [(list? (car l)) (or (member* a (car l))
                         (member* a (cdr l)))]
    [(eq? (car l) a) #t]
    [else            (member* a (cdr l))]))

(define (leftmost l)
  (cond
    [(atom? (car l)) (car l)]
    [else            (leftmost (car l))]))

(define (eqlist? l1 l2)
  (or (and (null? l1)
           (null? l2))
      (and (not (null? l1))
           (not (null? l2))
           (or (and (atom? (car l1))
                    (atom? (car l2))
                    (eqan?   (car l1) (car l2)))
               (and (list? (car l1))
                    (list? (car l2))
                    (eqlist? (car l1) (car l2))))
           (eqlist? (cdr l1) (cdr l2)))))

; eqlist? from the book
(define (book-eqlist? l1 l2)
  (cond
    [(and (null? l1) (null? l2)) #t]
    [(or  (null? l1) (null? l2)) #f]
    [(and (atom? (car l1))
          (atom? (car l2)))     (and (eqan?        (car l1) (car l2))
                                     (book-eqlist? (cdr l1) (cdr l2)))]
    [(or  (atom? (car l1))
          (atom? (car l2)))     #f]
    [else                       (and (book-eqlist? (car l1) (car l2))
                                     (book-eqlist? (cdr l1) (cdr l2)))]))

(define (equal? s1 s2)
  (cond
    [(and (atom? s1) (atom? s2))  (eqan? s1 s2)]
    [(or  (atom? s1) (atom? s2))  #f]
    [else                         (eqlist? s1 s2)]))

; simplified version presented in the book
(define (simple-eqlist? l1 l2)
  (cond
    [(and (null? l1) (null? l2)) #t]
    [(or  (null? l1) (null? l2)) #f]
    [else (and (equal?         (car l1) (car l2))
               (simple-eqlist? (cdr l1) (cdr l2)))]))

(define (numbered? aexp)
  (cond
    [(atom? aexp) (number? aexp)]
    [else         (and (numbered? (car aexp))
                       (numbered? (car (cdr (cdr aexp)))))]))

(define 1st-sub-exp car)
(define 2nd-sub-exp (compose car cdr cdr))

(define (value nexp)
  (cond
    [(atom? nexp)      nexp]
    [(eq? (car (cdr nexp)) '+) (plus (value (1st-sub-exp nexp))
                                     (value (2nd-sub-exp nexp)))]
    [(eq? (car (cdr nexp)) '×) (×    (value (1st-sub-exp nexp))
                                     (value (2nd-sub-exp nexp)))]
    [(eq? (car (cdr nexp)) '↑) (↑    (value (1st-sub-exp nexp))
                                     (value (2nd-sub-exp nexp)))]))

(define (set? lat)
  (or (null? lat)
      (and (not (member? (car lat) (cdr lat)))
           (set? (cdr lat)))))

(define (m-makeset lat)
  (cond
    [(null? lat)                   '()]
    [(member? (car lat) (cdr lat)) (m-makeset (cdr lat))]
    [else                          (cons (car lat) (m-makeset (cdr lat)))]))

(define (mr-makeset lat)
  (cond
    [(null? lat) '()]
    [else        (cons (car lat)
                       (multirember (car lat) (mr-makeset (cdr lat))))]))

(define makeset mr-makeset)

(define (subset? set1 set2)
  (or (null? set1)
      (and (member? (car set1) set2)
           (subset? (cdr set1) set2))))

(define (eqset? set1 set2)
  (and (subset? set1 set2)
       (subset? set2 set1)))

(define (intersect? set1 set2)
  (and (not (null? set1))
       (or (member?    (car set1) set2)
           (intersect? (cdr set1) set2))))

(define (intersect set1 set2)
  (cond
    [(null? set1)              '()]
    [(member? (car set1) set2) (cons (car set1)
                                     (intersect (cdr set1) set2))]
    [else                      (intersect (cdr set1) set2)]))

(define (union set1 set2)
  (cond
    [(null? set1)              set2]
    [(member? (car set1) set2) (union (cdr set1) set2)]
    [else                      (cons (car set1)
                                     (union (cdr set1) set2))]))

(define (intersectall l-set)
  (cond
    [(null? l-set)       '()]
    [(null? (cdr l-set)) (car l-set)]
    [else                (intersect (car l-set)
                                    (intersectall (cdr l-set)))]))

(define (a-pair? x)
  (and (list? x)
       (not (null? x))
       (not (null? (cdr x)))
       (null? (cdr (cdr x)))))

(define (first p)
  (car p))
(define (second p)
  (car (cdr p)))
(define (third p)
  (car (cdr (cdr p))))
(define (build s1 s2)
  (cons s1 (cons s2 '())))

(define (fun? rel)
  (set? (firsts rel)))

(define (revpair pair)
  (build (second pair) (first pair)))

(define (revrel rel)
  (cond
    [(null? rel) '()]
    [else        (cons (revpair (car rel))
                       (revrel  (cdr rel)))]))

(define (fullfun? fun)
  (fun? (revrel fun)))

(define (rember-f test? a l)
  (cond
    [(null? l)         '()]
    [(test? a (car l)) (cdr l)]
    [else              (cons (car l)
                             (rember-f test? a (cdr l)))]))

(define ((eq?-c a) x)
  (eq? x a))

(define eq?-salad (eq?-c 'salad))

(define ((curried-rember-f test?) a l)
  (cond
    [(null? l)         '()]
    [(test? a (car l)) (cdr l)]
    [else              (cons (car l)
                             ((curried-rember-f test?) a (cdr l)))]))

(define rember-eq? (curried-rember-f eq?))

(define ((insertL-f test?) new old lat)
  (cond
    [(null? lat)           '()]
    [(test? old (car lat)) (cons new lat)]
    [else                  (cons (car lat)
                                 ((insertL-f test?) new old (cdr lat)))]))

(define ((insertR-f test?) new old lat)
  (cond
    [(null? lat)           '()]
    [(test? old (car lat)) (cons old
                                 (cons new
                                       (cdr lat)))]
    [else                  (cons (car lat)
                                 ((insertR-f test?) new old (cdr lat)))]))
