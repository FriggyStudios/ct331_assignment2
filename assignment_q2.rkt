#lang racket
(define lst (list 1 2 34))
(provide ins_beg)
(provide ins_end)
(provide count_top_level)
(provide count_instances_tr)
(provide count_instances_deep)

(define (ins_beg el lst)
  (append (list el) lst))

(define (ins_end lst el)
  (append lst (list el)))

(define (count_top_level lst)
  (length lst))

(define (count_instances lst)
  (cond[(empty? lst)
       0]
       [else
        (+ 1 (count_instances (cdr lst)))]))

(define (count_instances_tr lst)
  (count_instances_tail lst 0))

 (define (count_instances_tail lst count)
   (cond[(empty? lst)
       count]
       [else
        (count_instances_tail (cdr lst) (+ count 1))]))

(define (atom? x) (not (pair? x)))

(define (count_instances_deep lst)
  (cond[(empty? lst)
       0]
       [(not (list? (car lst)))
        (+ 1 (count_instances_deep (cdr lst)))]
       [else
        (+ (count_instances_deep (car lst)) (count_instances_deep (cdr lst)))]))
