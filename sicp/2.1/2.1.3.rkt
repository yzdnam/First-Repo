#lang sicp

(define (even? n) (= 0 (remainder n 2)))

; EX 2.4
; an alternative procedural representation of pairs follows:
(define (alt-cons x y)
  (lambda (m) (m x y)))
(define (alt-car z)
  (z (lambda (p q) p)))
; write the corresponding definition for alt-cdr
(define (alt-cdr z)
  (z (lambda (p q) q)))

; EX 2.5
; represent pairs of nonnegative integers by representing the pair a and b as the integer that is the product 2^a * 3^b and give the corresponding definitions
; of the procedures cons, car, and cdr
(define (cons2.5 a b)
  (* (expt 2 a) (expt 3 b)))
(define (car2.5 p)
  (if (= (remainder p 3) 0) (car2.5 (/ p 3)) (/ (log p) (log 2))))
(define (cdr2.5 p)
  (if (even? p) (cdr2.5 (/ p 2)) (/ (log p) (log 3))))

; EX 2.6
; in a language that can manipulate procedures, we can get by without numbers by implementing 0 and the operation of adding 1 as:
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
; define one and two directly
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
; give a direct definition of the addition procedure
(define (add a b)
  (lambda (f) (lambda (x) (((a f) ((b f) x))))))
