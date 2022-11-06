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

(define (tpt1 n)
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
(define (search-for-primes >than range)
  (define check# (inc >than))
  (cond
    [(<= range 2) (tpt1 check#)]
    [(not (even? >than)) (search-for-primes (inc >than) (dec range))]
    [else (begin (tpt1 check#) (search-for-primes (+ >than 2) (- range 2)))]))

; Prime # - Time to find
; 1009 - 5 ms
; 1013 - 4 ms
; 1019 - 4 ms

; 10007 - 7 ms
; 10009 - 9 ms
; 10037 - 8 ms

; 100003 - 30 ms
; 100019 - 28 ms
; 100043 - 28 ms

; 1000003 - 84 ms
; 1000033 - 85 ms
; 1000037 - 85 ms

; The above results are not compatible with the notion that programs on this machine run in time proportional to the number of steps required for the computation

; EX 1.23
; define the procedure "next" that returns 3 if its input is equal to 2 and otherwise returns its input plus 2
(define (next n)
  (cond
    [(= n 2) 3]
    [else (+ n 2)]))
; modify the "smallest-divisor" procedure to use (next test-divisor) instead of (+ test-divisor 1)
(define (smallest-divisor.v2 n) (find-divisor.v2 n 2))

(define (find-divisor.v2 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor.v2 n (next test-divisor)))))
; use "timed-prime-test" with this modified version of "smallest-divisor" on the 12 primes found in EX 1.22
(define (prime?.v2 n)
  (= n (smallest-divisor.v2 n)))

(define (tpt2 n)
  (newline)
  (display n)
(start-prime-test.v2 n (runtime)))
(define (start-prime-test.v2 n start-time)
  (if (prime?.v2 n)
      (report-prime (- (runtime) start-time))))

; Prime # - Time to find with "timed-prime-test.v2"
; 1009 - 5 ms
; 1013 - 4 ms
; 1019 - 4 ms

; 10007 - 7 ms
; 10009 - 9 ms
; 10037 - 8 ms

; 100003 - 30 ms
; 100019 - 28 ms
; 100043 - 28 ms

; 1000003 - 84 ms
; 1000033 - 85 ms
; 1000037 - 85 ms

; the 2nd versions of timed-prime-test does not run twice as fast as the first version even though the number of test steps is halved
; This can be explained by the fact that the "next" procedure causes additional primitive operations to be performed per test step. more data would need to be
; collected to determine a ratio of the speeds of the two algorithms. 

; v3 of timed-prime-test was an attempt to test the given prime number 1000 times and display the average time for the process to run to completion.
; this attempt failed. "compile-times" returns a list of very similar numbers except for the last one on the list. The last number on the list is the first
; time "start-prime-test" is called. My hypothesis is that there is a yet to be investigated mechanism in this machine that optimizes the execution of
; a procedure that is called multiple times during the execution of another procedure.
(define (tpt3 n)
  (newline)
  (display n)
(define (compile-times counter lot n)
  (cond
    [(= counter 0) lot]
    [else (compile-times (dec counter) (cons (start-prime-test.v3 n (runtime)) lot) n)]))
(report-prime (list-avg (compile-times 1000 '()))))
(define (start-prime-test.v3 n start-time)
  (if (prime?.v2 n)
      (- (runtime) start-time)))
(define (list-avg loi)
  (define (list-avg/a loi count)
    (cond
      [(eqv? (cdr loi) '()) (/ (car loi) count)]
      [else (list-avg/a (cons (+ (car loi) (car (cdr loi))) (cdr (cdr loi))) (inc count))]))
  (list-avg/a loi 1))
(define (compile-times counter lot n)
  (cond
    [(= counter 0) lot]
    [else (compile-times (dec counter) (cons (start-prime-test.v3 n (runtime)) lot) n)]))

; EX 1.24
; conduct the same experiment as in EX 1.23 except with "fast-prime" as the prime number test
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

(define (fermat-test n)
(define (try-it a)
 (= (expmod a n n) a))
(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
(cond ((= times 0) true)
((fermat-test n) (fast-prime? n (- times 1)))
(else false)))

(define (tpt4 n)
  (newline)
  (display n)
(start-prime-test.v4 n (runtime)))
(define (start-prime-test.v4 n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

; Although the Fermat test has θ(log n) growth, testing primes near 1,000,000 does not take (log 1000) times as long as testing primes near 1000.
; my hypothesis is that the processor is powerful enough to complete all operations in one cycle hence the times measured are those of a processor cycle

; EX 1.25
; alternative expmod?

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod.v2 base exp m)
  (remainder (fast-expt base exp) m))

; They return the same results for the tested inputs.

; EX 1.26
; explain why expmod.v3 is a θ(n) process instead of a θ(log n) process like expmod and expmod.v2
(define (expmod.v3 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod.v3 base (/ exp 2) m)
                       (expmod.v3 base (/ exp 2) m))
                    m))
        (else
         (remainder (* base
                       (expmod.v3 base (- exp 1) m))
                    m))))
; expmod and expmod.v2 will call "square" θ(log n) times
; expmod.v3 will expand to apply *
(define (fe n)
  (+ n (* 3 (floor (/ (log n) (log 2))))))