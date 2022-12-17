#lang sicp

; EX 2.54
; two lists are said to be equal? if they contain equal elements arranged in the same order
; define equal? as a procedure
(define (equal? l1 l2)
  (cond
    [(and (eq? '() l1) (eq? '() l2)) #t]
    [(or (and (not (list? l1)) (list? l2))
         (and (list? l1) (not (list? l2)))) #f]
    [(and (not (list? l1)) (not (list? l2)))
     (eq? l1 l2)]
    [else (if (equal? (car l1) (car l2))
              (equal? (cdr l1) (cdr l2))
              #f)]))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
    