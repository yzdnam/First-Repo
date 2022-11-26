#lang sicp

(define (cube n) (* n n n))

(define (average n1 n2) (/ (+ n1 n2) 2))

(define (square n) (* n n))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
     tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

; transforms a function, g, into f where where f's fixed point will yield the solution to g(x) = 0
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; finds a square root using newton's method to find a zero of the function y ↦ y^2 - x
(define (newts-sqrt x)
  (newtons-method
    (lambda (y) (- (square y) x)) 1.0))

; compute-function transform-function number -> fixed point of the transformed function
; abstraction for finding a fixed point of some transformation of a given function
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; sqrt function using the fixed point of the average damped version of y ↦ x/y and fixed-point-of-transform
(define (sqrt1.1 x)
  (fixed-point-of-transform
    (lambda (y) (/ x y)) average-damp 1.0))

; sqrt function using newton's method and fixed-point-of-transform
(define (newts-sqrt2 x)
  (fixed-point-of-transform
    (lambda (y) (- (square y) x)) newton-transform 1.0))

; EX 1.40
; define a procedure, cubic, that can be used together with the newtons-method procedure to approximate zeros of the cubic x^3+ax^2+bx+c
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

; EX 1.41
; define a procedure, double, that takes a procedure of one argument as an argument and returns a procedure that applies the original procedure twice.
(define (double p)
  (lambda (x) (p (p x))))

; EX 1.42
; define a compose procedure that creates a function x ↦ f(g(x)) when given f and g
(define (compose f g)
  (lambda (x) (f (g x))))

; EX 1.43
; write a procedure that takes as inputs a procedure that computes f and a positive integer n and returns the procedure that computes the nth repeated application of f
(define (repeated f n)
  (if (= n 1) f (compose f (repeated f (- n 1)))))

; EX 1.44
; write a procedure, smooth, that takes as input a procedure that computes f and returns a procedure that computes the smoothed f. the smoothed version of f is the
; function whose value at point x is the average of f(x-dx), f(x), and f(x+dx).
(define (smooth f)
  (let ((dx 0.1))
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))

; to obtain the n-fold smoothed version of f, run the following:
; ((repeated smooth n) f)

; EX 1.45
; implement a procedure for computing nth roots using fixed-point, average-damp, and the repeated procedure
; average-damp needs to be applied one additional time for every power of two in n, or floor(log-base2 n)
(define (root x n)
  (fixed-point ((repeated average-damp (floor (/ (log n) (log 2)))) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

; EX 1.46
; write a procedure iterative-improve that takes two procedures as arguments:
; a method for tellling whether a guess is good enough and a method for improving a guess
; iterative-improve should return as its value a procedure that takes a guess as argument and keeps improving the guess until it is good enough
(define (iterative-improve good-enough? improve)
  (lambda (x) (if (good-enough? x) x ((iterative-improve good-enough? improve) (improve x)))))

; version of sqrt using iterative-improve
(define (sqrt1.46 x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001)) (lambda (guess) (average guess (/ x guess)))) 1.0))

; version of fixed-point using iterative-improve
(define (fp1.46 f)
  (let ((tolerance 0.00001))
  ((iterative-improve (lambda (guess) (< (abs (- (f guess) guess)) tolerance)) (lambda (guess) (f guess))) 1.0)))