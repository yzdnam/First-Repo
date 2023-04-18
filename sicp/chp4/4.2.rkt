#lang sicp

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

; EX 4.25
(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

;(* 5 (f 4))
;(* 5 (* 4 (* 3 (* 2
; attempting to evaluate the version of factorial above in an applicative order language will result in a loop because the
; usual-value argument will continue decrementing n indefinitely as the evaluator attempts to complete its evaluation causing the
; exceptional-value condition to never be reached by the evaluator

; the definition will work in a normal order language because the usual-value will only be evaluated if n fulfills the condition
; during each call to factorial

; EX 4.26
; show how to implement "unless" as a derived expression such as cond or let.
; see 4.1.rkt, line 579
(define (f x)
  (unless (= (remainder x 2) 0)
    x
      (error "x is even")))
   ; x))

(define select-y '(#t #f #t #t)) 
 (define xs '(1 3 5 7)) 
 (define ys '(2 4 6 8)) 
 (define selected (map unless select-y xs ys)) 