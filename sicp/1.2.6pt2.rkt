#lang sicp

(define (square n) (* n n))
(define (even? n) (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

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

; EX 1.28
        