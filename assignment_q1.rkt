#lang racket
(cons 1 2)
(cons 1(cons 2(cons 3 empty)))
(cons "one" (cons 2(cons (cons 3(cons 4(cons 5 empty))) empty)))
(list "one" 2 (list 3 4 5))
(append '("one") '(2) '(( 3 4 5)))