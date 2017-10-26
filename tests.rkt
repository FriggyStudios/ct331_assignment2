#lang racket

(require (file "assignment_q2.rkt")
         (file "assignment_q3.rkt"))

;
;Don't worry about this file unless you are doing the extra credit tests. 
;

;This structure allows a single function call
;to run every test in sequence, rather than
;calling each function separately. 
(define (runTests)
  (begin
    (display "Running tests...\n")
    ;begin calling test functions
    (printf "1: ~a\n" (test_ins_beg))
    (printf "2: ~a\n" (test_ins_end))
    (printf "3: ~a\n" (test_count_top_level))
    (printf "4: ~a\n" (test_count_instances_tr))
    (printf "5: ~a\n" (test_count_instances_deep))
    ;end calling test functions
    (display "\nTests complete!\n")))

;Begin test functions
(define (test_ins_beg)
  (equal? (ins_beg 1 '(2 3 4)) '(1 2 3 4)))
(define (test_ins_end)
  (equal? (ins_end '(1 2 3) 4) '(1 2 3 4)))
(define (test_count_top_level)
  (equal? (count_top_level '(1 2 3 55)) 4))
(define (test_count_instances_tr)
  (equal? (count_instances_tr '(1 2 3 55)) 4))
(define (test_count_instances_deep)
  (equal? (count_instances_deep (list 1 2 3(list 1 3))) 5))

;End test functions

;Run the tests
(runTests)