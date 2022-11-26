#lang sicp

(define pi 3.141592654)

; Finding roots of equations by the half-interval method

(define (average x y) (/ (+ x y) 2))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
(let ((midpoint (average neg-point pos-point)))
  (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value)
               (search f neg-point midpoint))
              ((negative? test-value)
               (search f midpoint pos-point))
              (else midpoint))))))


(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

; Finding fixed points of functions
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

; EX 1.35
; To show that the golden ratio ϕ is a fixed point of the transformation x ↦ 1 + 1/x:
; x = 1 + 1/x is equivalent to x^2 = x + 1 which is the equation for the golden ratio
; we can also derive the explicit formula for ϕ from this equation by recasting as a quadratic
; and solving with the quadratic formula (discarding the negative root

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

; EX 1.36
; modify fixed-point so it prints the sequence of approximations it generates then find a solution of x^x = 1000 by find a fixed point of
; x ↦ log(1000)/log(x). compare the number of steps this takes with and without average damping with a tolerance of 0.00001
; without average damping - 34 steps
; with average damping - 10 steps
(define (fixed-point1.36 f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess number-of-guesses)
    (let ((next (f guess)))
      (begin (newline) (display number-of-guesses) (display ". ") (display guess) (display " ↦ ") (display next)
      (if (not (close-enough? guess next))
          (try next (inc number-of-guesses))))))
  (try first-guess 1))

(define (fixed-point-with-avg-damp f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess number-of-guesses)
    (let ((next (f guess)))
      (begin (newline) (display number-of-guesses) (display ". ") (display guess) (display " ↦ ") (display next)
      (if (not (close-enough? guess next))
          (try (average guess next) (inc number-of-guesses))))))
  (try first-guess 1))

; EX 1.37
; Background on paper
; define a procedure "cont-frac" such that evaluating (cont-frac n d k) computes the value of the k-term finite continued fraction. Check your procedure by
; approximating 1/ϕ
(define (cont-frac n d k)
  (define (cont-frac/a arg)
    (let ((n-arg (n arg))
          (d-arg (d arg)))
      (if (> arg k)
          0
          (/ n-arg (+ d-arg (cont-frac/a (inc arg)))))))
  (cont-frac/a 1))
; How large must you make k in order to get an approximation of 1/ϕ that is accurate to 4 decimals
; 12, found using the procedure below
(define (size-of-k? k)  
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (if (close-enough? (cont-frac (lambda (i) 1.0)
                                (lambda (i) 1.0)
                                k) 
                               (/ 1 (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)))
      k
      (size-of-k? (inc k))))
; write a cont-frac that generates an iterative process
(define (cont-frac-iter n d k)
  (define (iter in accum)
    (cond
      [(= in 0) accum]
      [else (iter (dec in) (/ (n in) (+ (d in) accum)))]))
  (iter k 0))

; EX 1.38
; write a program that uses cont-frac, Nsubi set to 1, and Dsubi set to the sequence 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ... to approximate e
(define (approx-e k)
    (+ 2 (cont-frac (lambda (x) 1.0)
               (lambda (x) 
                 (let ((modx3 (modulo x 3)))
                   (cond
                               [(or (= modx3 0)
                                    (= modx3 1)) 1]
                               [else (* 2 (ceiling (/ x 3)))])))
               k)))

; EX 1.39
; define a procedure (tan-cf x k) that computes an approximation to the tangent function based on J.H. Lambert's formula. k specifies the number of terms to compute
(define (tan-cf x k)
  (cont-frac (lambda (k)
               (if (= k 1) x (* -1 (* x x))))
             (lambda (k)
               (+ 1 (* 2 (- k 1))))
             k))