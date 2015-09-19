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

(test-case "Chapter 3"
  (check-equal? (rember 'mint '(lamb chops and mint jelly))
                              '(lamb chops and      jelly))
  (check-equal? (rember 'mint '(lamb chops and mint flavored mint jelly))
                              '(lamb chops and      flavored mint jelly))
  (check-equal? (rember 'toast '(bacon lettuce and tomato))
                               '(bacon lettuce and tomato))
  (check-equal? (rember 'cup '(coffee cup tea cup and hick cup))
                             '(coffee     tea cup and hick cup))
  (check-equal? (rember 'and '(bacon lettuce and tomato))
                             '(bacon lettuce     tomato))

  (check-equal? (firsts '((apple peach pumpkin)
                          (plum pear cherry)
                          (grape raisin pea)
                          (bean carrot eggplant)))
                '(apple plum grape bean))
  (check-equal? (firsts '((a b) (c d) (e f)))
                        '( a     c     e   ))
  (check-equal? (firsts '()) '())
  (check-equal? (firsts '((five plums)
                          (four)
                          (eleven green oranges)))
                '(five four eleven))
  (check-equal? (firsts '(((five plums) four)
                          (eleven green oranges)
                          ((no) more)))
                '((five plums) eleven (no)))

  (check-equal? (insertR 'topping 'fudge '(ice cream with fudge         for dessert))
                                         '(ice cream with fudge topping for dessert))
  (check-equal? (insertR 'jalapeño 'and '(tacos tamales and          salsa))
                                        '(tacos tamales and jalapeño salsa))
  (check-equal? (insertR 'e 'd '(a b c d   f g d h))
                               '(a b c d e f g d h))

  (check-equal? (insertL 'e 'd '(a b c   d f g d h))
                               '(a b c e d f g d h))

  (check-equal? (subst 'e 'd '(a b c d f g d h))
                             '(a b c e f g d h))
  (check-equal? (subst 'topping 'fudge '(ice cream with fudge   for dessert))
                                       '(ice cream with topping for dessert))

  (check-equal? (subst2 'vanilla 'chocolate 'banana '(banana  ice cream with chocolate topping))
                                                    '(vanilla ice cream with chocolate topping))

  (check-equal? (multirember 'cup '(coffee cup tea cup and hick cup))
                                  '(coffee     tea     and hick    ))
  (check-equal? (multiinsertR 'e 'd '(a b c d   f g d   h))
                                    '(a b c d e f g d e h))
  (check-equal? (multiinsertL 'e 'd '(a b c   d f g   d h))
                                    '(a b c e d f g e d h))
  (check-equal? (multisubst 'e 'd '(a b c d f g d h))
                                  '(a b c e f g e h)))
