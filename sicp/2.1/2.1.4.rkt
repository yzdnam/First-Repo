#lang sicp

(define (make-interval a b) (cons a b))
; EX 2.7
; define selectors, upper-bound and lower-bound, for an interval
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

; EX 2.8
; define a subtraction procedure for intervals called sub-interval
; reasoning analogous to the reasoning used in defining add-interval follows:
; the min value the difference could be is the difference between the first lower bound and the second upper bound and the max value it could be is the difference
; between the second lower bound and the first upper bound
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
      (error "Cannot divide by an interval that spans 0")
      (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

; EX 2.9
; show that the width of the sum or difference of two intervals is a function only of the widths of the intervals being added (or subtracted). Give examples to show
; that this is not true for multiplication or division.
; solution on paper

; EX 2.10
; added a condition check to div-interval checking if the divisor interval spans 0

; EX 2.11
; write a mul-interval procedure that uses the signs of the endpoints of the intervals to break the procedure into nine cases, only one of which requires more than
; two multiplications
(define (mul-interval2.11 x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (cond
    [(and (>= (lower-bound x) 0) (>= (upper-bound x) 0) (>= (lower-bound y) 0) (>= (upper-bound y) 0))
     (make-interval p1 p4)]
    [(and (>= (lower-bound x) 0) (>= (upper-bound x) 0) (< (lower-bound y) 0) (>= (upper-bound y) 0))
     (make-interval p3 p4)]
    [(and (>= (lower-bound x) 0) (>= (upper-bound x) 0) (< (lower-bound y) 0) (< (upper-bound y) 0))
     (make-interval p3 p2)]
    [(and (< (lower-bound x) 0) (>= (upper-bound x) 0) (>= (lower-bound y) 0) (>= (upper-bound y) 0))
     (make-interval p2 p4)]
    [(and (< (lower-bound x) 0) (>= (upper-bound x) 0) (< (lower-bound y) 0) (>= (upper-bound y) 0))
     (make-interval (min p2 p3) (max p1 p4))]
    [(and (< (lower-bound x) 0) (>= (upper-bound x) 0) (< (lower-bound y) 0) (< (upper-bound y) 0))
     (make-interval p3 p1)]
    [(and (< (lower-bound x) 0) (< (upper-bound x) 0) (>= (lower-bound y) 0) (>= (upper-bound y) 0))
     (make-interval p2 p3)]
    [(and (< (lower-bound x) 0) (< (upper-bound x) 0) (< (lower-bound y) 0) (>= (upper-bound y) 0))
     (make-interval p2 p3)]
    [(and (< (lower-bound x) 0) (< (upper-bound x) 0) (< (lower-bound y) 0) (< (upper-bound y) 0))
     (make-interval p4 p1)])))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; EX 2.12
; define a selector, percent, that produces the percentage tolerance for a given interval. the percentage tolerance is defined as the ratio of the width of the
; interval to the midpoint of the interval
(define (percent i)
  (* 100 (/ (width i) (center i))))

; define a constructor, make-center-percent, that takes a (c)enter and a (p)ercentage tolerance and the produces the desired interval
(define (make-center-percent c p)
  (let ((width (* c (/ p 100.0))))
  (make-interval (- c width) (+ c width))))

; EX 2.13
; Show that under the assumption of small percentage tolerances, there is a simple formula for the approximate percentage tolerance of the product of two intervals
; in terms of the tolerances of the factors. Assume all numbers are positive.
; solution on paper

(define i (make-center-percent 10 0.5))
(define j (make-center-percent 10 0.4))
(percent (mul-interval i j))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
      one (add-interval (div-interval one r1)
                        (div-interval one r2)))))

(define one-int (make-interval 1 1))

(define (int-eqv? x y)
  (and (= (upper-bound x) (upper-bound y))
       (= (lower-bound x) (lower-bound y))))

; EX 2.14
; Demonstrate and investigate why par1 and par2 produce different answers
(int-eqv? (par1 i j) (par2 i j))

; EX 2.15
; a formula to compute intervals using the system included in this file will produce tighter error bounds if it can be written in such a form that no variable that
; represents an uncertain number is repeated
; with this knowledge, determine which of the par programs is "better" and why