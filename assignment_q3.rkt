#lang racket
(define tree (list (list empty 2 (list empty 4 empty)) 5 (list (list empty 7 empty) 8 empty)))
(define tree2(list empty 3 empty))
(define lst (list 1 31 11 5235 2252 5 25 2 52 5252 252))

(define (displayBTree tree)
  (unless (empty? (car tree))
    (displayBTree (car tree)))
  (println (cadr tree))
  (unless (empty? (caddr tree))
    (displayBTree (caddr tree))))
(displayBTree tree)

(define (inBTree? item tree)
  (cond[(empty? tree)
        #f]
       [(= (cadr tree) item)
        #t]
       [(< item (cadr tree))
        (inBTree? item (car tree))]
       [else
        (inBTree? item (caddr tree))]))
(inBTree? 7 tree)

(define (insertBTree item tree)
  (cond[(empty? tree)
        (list empty item empty)]
       [(= item (cadr tree))
        tree]
       [(< item (cadr tree))
        (list (insertBTree item (car tree)) (cadr tree) (caddr tree))]
       [else
        (list (car tree) (cadr tree) (insertBTree item (caddr tree)))]))
tree
(insertBTree 10 tree)

(define (insertListBTree lst tree)
  (cond[(empty? lst)
        tree]
   [else
    (insertListBTree (cdr lst) (insertBTree (car lst) tree ))]))
(insertListBTree (list 1 2 3 4 5 6 7 8 9 123) tree)

(define (sortList lst)
  (displayBTree(insertListBTree (cdr lst) (list empty (car lst) empty))))
;(sortList lst)

(define (insertBTreeReverse item tree)
  (cond[(empty? tree)
        (list empty item empty)]
       [(= item (cadr tree))
        tree]
       [(> item (cadr tree))
        (list (insertBTreeReverse item (car tree)) (cadr tree) (caddr tree))]
       [else
        (list (car tree) (cadr tree) (insertBTreeReverse item (caddr tree)))]))

(define (insertListBTreeReverse lst tree)
  (cond[(empty? lst)
        tree]
   [else
    (insertListBTreeReverse (cdr lst) (insertBTreeReverse (car lst) tree ))]))

(define (insertBTreeByLastDigit item tree)
  (cond[(empty? tree)
        (list empty item empty)]
       [(= item (cadr tree))
        tree]
       [(< (modulo item 10) (modulo (cadr tree) 10))
        (list (insertBTreeByLastDigit item (car tree)) (cadr tree) (caddr tree))]
       [else
        (list (car tree) (cadr tree) (insertBTreeByLastDigit item (caddr tree)))]))

(define (insertListBTreeByLastDigit lst tree)
  (cond[(empty? lst)
        tree]
   [else
    (insertListBTreeByLastDigit (cdr lst) (insertBTreeByLastDigit (car lst) tree ))]))

(define (sortListHighOrder lst insertFunc)
  (displayBTree(insertFunc (cdr lst) (list empty (car lst) empty))))
(sortListHighOrder lst insertListBTreeByLastDigit)