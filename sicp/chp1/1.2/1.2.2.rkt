#lang sicp

; Challenge
; write a procedure that computes the number of ways to make change for a given amount of cents given pennies, nickels, dimes, quarters, and half-dollars
; by means of an iterative process:
; Solution in 1.2.2wkspce.rkt file

(define (count-change amount) (cc amount (list 1 5 10 25 50)))
(define (cc amount kinds-of-coins)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (eqv? kinds-of-coins '())) 0]
        [else (+ (cc amount (cdr kinds-of-coins))
                 (cc (- amount (car kinds-of-coins)) kinds-of-coins))]))

; [List-of Number] Number -> Number
(define (make-change loc0 amt)
  ; 
  (define (make-change/a orig-loc int-loc curr-loc local-amt ways)
    (cond
      ; all coins in original list iterated through and current list of coins incremented through with exact change being made; return results plus 1
      [(and (eqv? orig-loc '()) (eqv? local-amt 0))
         (inc ways)]
      ; all coins in original list iterated through and current list of coins incremented through with exact change NOT being made; return results
      [(and (eqv? orig-loc '()) (eqv? curr-loc '()))
         ways]
      ; deduction of next type of coin resulted in correct change; add 1 to ways to make change then recompute value with next coin from original list added
      ; to the list used to deduct from the amount to be changed
      [(eqv? local-amt 0) (make-change/a (cdr orig-loc) (cons (car orig-loc) int-loc) (cons (car orig-loc) int-loc) (- amt (car orig-loc)) (inc ways))]
      ; deduction of next type of coin from current list of coins did not make correct change or current list of coins incremented through; restart deducting coins
      ; from original amount to be changed with next coin from original list added to the deducting list
      [(or (< local-amt 0) (eqv? curr-loc '())) (make-change/a (cdr orig-loc) (cons (car orig-loc) int-loc) (cons (car orig-loc) int-loc) amt ways)]
      ; if next type of coin greater than amount to be changed, use rest of list of coins, otherwise, deduct next type of coin from amount to be changed
      [else (if (> (car curr-loc) local-amt) (make-change orig-loc int-loc (cdr curr-loc) local-amt ways)
                (make-change/a orig-loc int-loc curr-loc (- local-amt (car curr-loc)) ways))]))
  (make-change/a loc0 '() '() amt 0))

(make-change (list 1 5) 10)

;(define (make-change.v2 amt)
;  (define (make-change/a hd quarter dime nickel penny local-amt ways)
;    (cond
;      [(eqv? local-amt amt) (make-change/a hd quarter dime nickel penny 0 (inc ways))]
;      [(> amt penny) (make-change/a hd quarter dime nickel (inc penny) (inc penny) ways)]
;      [(> amt nickel) (make-change/a hd quarter dime (+ nickel 5)

; EX 1.11
; f(n) = n if n<3 or f(n-1)+2f(n-2)+3f(n-3) if n>=3
; write a procedure that computes f by means of a recursive process. write a procedure that computes f by means of an iterative process:

(define (f-recursive n)
  (cond
    [(< n 3) n]
    [(+ (f-recursive (- n 1)) (* 2 (f-recursive (- n 2))) (* 3 (f-recursive (- n 3))))]))

(define (f-iter n)
  (define (f-iter/a x y z count)
    (cond
      [(= count 0) z]
      [else (f-iter/a (+ x (* 2 y) (* 3 z)) x y (- count 1))])) 
  (f-iter/a 2 1 0 n))

; EX 1.12
; returns the nth row of Pascal's triangle
(define (p-tri n)
  (cond
    [(eqv? n 1) 1]
    [(eqv? n 2) `(,(p-tri (- n 1)) ,(p-tri (- n 1)))]
    [else (append '(1) (condense-row (p-tri (- n 1))) '(1))]))

; [List-of Number] -> [List-of Number]
; Takes a list of length n and returns a list of length n-1 by adding each number next to each other and returning the results in the new list
(define (condense-row lon0)
  (define (condense-row/a lon a)
    (cond
      [(eqv? (cdr lon) '()) a]
      [else (condense-row/a (cdr lon) (cons (+ (car lon) (car (cdr lon))) a))]))
  (condense-row/a lon0 '()))
  
; EX 1.13 on paper
    