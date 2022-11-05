#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; EX 1.16
; Design a procedure that evolves an iterative exponentiation process that uses successive squaring and uses a logarithmic number of steps, as does fast-expt.
(define (expt-iter b0 n0)
  (define (expt-iter/a b n a steps)
    (cond
      [(= n 0) a]
      [(even? n) (expt-iter/a (square b) (/ n 2) a (inc steps))]
      [else (expt-iter/a b (- n 1) (* b a) (inc steps))]))
  (expt-iter/a b0 n0 1 0))

(= (expt-iter 10 1000) (fast-expt 10 1000))

; EX 1.17
; design a multiplication procedure analgous to fast-expt that uses the operations double and halve, and a logarithmic number of steps, assuming our language can
; only add, not multiply
(define (halve number)
  (/ number 2))

(define (double number)
  (* number 2))

(define (fast-* cand er)
  (cond ((= er 0) 0)
        ((even? er) (double (fast-* cand (halve er))))
        (else (+ cand (fast-* cand (- er 1))))))

; EX 1.18
; combine the results of EX 1.16 and 1.17 to devise a procedure that generates an iterative process for multiplying two integers in terms of adding,
; double, and halving and uses a logarithmic number of steps
(define (*-iter cand0 er0)
  (define (*-iter/a cand er a steps)
    (cond
      [(= er 0) a]
      [(even? er) (*-iter/a (double cand) (halve er) a (inc steps))]
      [else (*-iter/a cand (- er 1) (+ cand a) (inc steps))]))
  (*-iter/a cand0 er0 0 0))

(= (fast-* 7 8) (*-iter 7 8))

; EX 1.19
(define (fib-ex n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q)) ; compute p′
                   (+ (* 2 p q) (square q)) ; compute q′
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(= (fib 10) (fib-ex 10))