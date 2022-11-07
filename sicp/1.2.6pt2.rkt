#lang sicp

(define (square n) (* n n))
(define (even? n) (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

; EX 1.27
; write a procedure that demonstrates that the Carmichael numbers really do fool the Fermat test
; more explicitly, the procedure tests whether a^n is congruent to a modulo n for every a < n
(define (ex1.27 n)
  (define (ex1.27/a a)
    (cond
      ((= a n) #t)
      ((= (expmod a n n) a) (ex1.27/a (inc a)))
      (else #f)))
  (ex1.27/a 1))
; -----

(define (fermat-test n)
(define (try-it a)
 (= (expmod a n n) a))
(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
(cond ((= times 0) true)
((fermat-test n) (fast-prime? n (- times 1)))
(else false)))

; EX 1.28
; write a procedure that implements the Miller-Rabin Test for testing the primality of a number
(define (m-rexpmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (m-rexpmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (m-rexpmod base (- exp 1) m))
          m))))

(define (m-r n)
