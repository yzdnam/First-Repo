#lang sicp

(define (cube n) (* n n n))

(define (even? n) (= (remainder n 2) 0))

(define (square n) (* n n))

(define (divides? a b) (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum term a next b)
  (if (> a b )
      0
      (+ (term a)
         (sum term (next a) next b))))

; EX 1.29
; Simpson's rule approximates the integral of a function, f, between a and b as:
; h/3(ysub0 + 4ysub1 + 2ysub2 + 4ysub3 + 2ysub4 + ... + 2ysub(n-2) + 4ysub(n-1) + ysubn)
; where h = (b - a)/n, for some even integer n, and ysubk = f(a + kh).

; Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral computed using Simpson's Rule.
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (ysubk k)
    (f (+ a (* k h))))
  (define (multiplier k)
    (cond
      [(or (= k 0)
           (= k n)) (ysubk k)]
      [(even? k) (* 2 (ysubk k))]
      [else (* 4 (ysubk k))]))
  (* (/ h 3) (sum multiplier 0 inc n)))


(define (integral f a b dx)
(define (add-dx x)
(+ x dx))
(* (sum f (+ a (/ dx 2.0)) add-dx b)
dx))

(integral cube 0 1 0.01)
(simpson cube 0 1 100)

; EX 1.30
; design a sum procedure that is performed iteratively
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (sum-cubes a b)
  (sum cube a inc b))
(define (sum-iter-cubes a b)
  (sum-iter cube a inc b))

(sum-cubes 1 10)
(sum-iter-cubes 1 10)

; EX 1.31
; write an analogous procedure to sum called product that returns the product of the values of a function at points over a given range
; define factorial and compute approximations to π using the formula
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

(define (pi-frac n)
  (* (/ n (+ n 1)) (/ (+ n 2) (+ n 1))))
(define (add2 n) (+ 2 n))

(* 4 (product pi-frac 2.0 add2 10000.0))

; EX 1.31 part b.
; write a product procedure that generates an iterative process
(define (prod-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (iter-factorial n)
  (prod-iter identity 1 inc n))

; EX 1.32 part a.
; write an accumulate function which abstracts the sum and product functions
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (abs-sum term a next b)
  (accumulate + 0 term a next b))

(define (abs-sum-cubes a b)
  (abs-sum cube a inc b))

; EX 1.32 part b.
; write an accumulate function that generates an iterative process
(define (accum-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (abs-sum.b term a next b)
  (accum-iter + 0 term a next b))

(define (abs-sum-cubes.b a b)
  (abs-sum.b cube a inc b))

(= (abs-sum-cubes 1 10) (abs-sum-cubes.b 1 10))

; EX 1.33
; write filtered-accumulate which takes the same arguments as accumulate together with an additional predicate of one argument that specifies the filter
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter (term a))
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
  (iter a null-value))

; EX 1.33 part a.
; express the sum of the squares of the prime numbers in the interval a to b using filtered-accumulate
(define (sqrt-prime? a)
  (prime? (sqrt a)))
(define (primes-sqrd-sum a b)
  (cond
    [(<= a 2) (+ 4 (filtered-accumulate sqrt-prime? + 0 square 3 add2 b))]
    [(even? a)
     (filtered-accumulate sqrt-prime? + 0 square (inc a) add2 b)]
    [else
     (filtered-accumulate sqrt-prime? + 0 square a add2 b)]))

; EX 1.33 part b.
; express the product of all positive integers less than n that are relatively prime to n using filtered-accumulate
(define (rel-primes-product n)
  (define (rel-prime-w-n? a)
    (= (gcd n a) 1))
  (filtered-accumulate rel-prime-w-n? * 1 identity 1 inc (- n 1)))

