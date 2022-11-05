#lang sicp

; EX 1.21 - use the smallest-divisor procedure to find the smallest divisor for the following numbers:
; 199 - 199
; 1999 - 1999
; 19999 - 7

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
(start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; EX 1.22 - write a procedure, "search-for-primes", that checks the primality of consecutive odd integers in a specified range using "timed-prime-test".
; test various inputs with the procedure and determine whether the results are compatible with the notion that programs on this machine run in time proportional
; to the number of steps required for the computation
(define (search-for-primes >than0 how-many0)
  (define (search-for-primes/a >than how-many lop)
    (cond
      []