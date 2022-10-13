#lang sicp

(define (square x) (* x x))

; EX 1.6 answer - "if" needs to be provided as a special form because it prevents its arguments from being evaluated prior to the predicate. When new-if is called for
; sqrt-iter, sqrt-iter recurses on itself infinitely because the predicate is never brought to the front to be evaluated.

; (define (new-if predicate then-clause else-clause)
; (cond (predicate then-clause)
; (else else-clause)))

(define (sqrt-iter guess x)
(if (good-enough? guess (improve guess x) x)
guess
(sqrt-iter (improve guess x) x)))

(define (improve guess x)
(average guess (/ x guess)))

(define (average x y)
(/ (+ x y) 2))

; EX 1.7 - good-enough? is inadequate for very small and very large numbers because squaring numbers with many digits on computers is inexact

(define (good-enough? guess next-guess x)
(< (abs (- (square guess) x)) 0.00000001))

(define (sqrt x)
(sqrt-iter 1.0 x))

(sqrt 1000000)

(define (cube x) (* x x x))

(define (cube-iter guess x)
  (if (good-enough-cube? guess (improve-cube guess x) x)
      guess
      (cube-iter (improve-cube guess x) x)))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough-cube? guess next-guess x)
;  (< (abs (/ (- guess next-guess) guess)) 0.0000001))
  (< (abs (- (cube guess) x)) 0.0000001))

(define (cubert x)
  (cube-iter 1.0 x))