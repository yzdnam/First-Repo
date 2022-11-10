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
; Number Number Number -> Number
; calculates the first number to the power of the second number, modulo the third number
; whenever we perform the squaring step in m-rexpmod, we check to see if we have discovered a “nontrivial square root of
; 1 modulo n,” that is, a number not equal to 1 or (- n 1) whose square is equal to 1 modulo n 
(define (m-rexpmod base exp m)
  (cond
    [(= exp 0) 1]
    [(even? exp)
     (cond
       [(= (expmod (fast-expt base exp) 2 m) (- m 1))
        (remainder (square (expmod base (/ exp 2) m)) m)]
       [(= (expmod (fast-expt base exp) 2 m) 1)
        (remainder (square (m-rexpmod base (/ exp 2) m)) m)]
       [else 0])]
    [else
     (remainder
       (* base (expmod base (- exp 1) m))
       m)]))

(define (m-r n)
  (define (try-itm-r a times)
    (cond
      [(> times 0)
                  (if (= (m-rexpmod a (- n 1) n) 0)
                      (begin (newline) (begin (display "not prime ") (display n)))
                      (try-itm-r (+ 1 (random (- n 1))) (dec times)))]
      [else (begin (newline) (begin (display "prime ") (display n)))]))
  (try-itm-r (+ 1 (random (- n 1))) 10))

(m-r 3)
(m-r 5)
(m-r 7)
(m-r 11)
(m-r 13)
(m-r 17)
(m-r 19)
(m-r 23)
(m-r 29)
(m-r 31)
(m-r 37)
(m-r 41)
(m-r 43)
(m-r 47)
(m-r 53)
(m-r 59)
(m-r 61)
(m-r 67)
(m-r 71)
(m-r 73)
(m-r 79)
(m-r 83)
(m-r 89) (m-r 97)
(m-r 15)
(m-r 21)
(m-r 561)
(m-r 1105)
(m-r 1729)
(m-r 2465)