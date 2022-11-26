#lang sicp

(define (sq n) (* n n))

(define (average n1 n2) (/ (+ n1 n2) 2))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; EX 2.1
; define a version of make-rat that handles both positive and negative arguments
(define (make-rat2.1 n d)
  (let ((g (gcd (abs n) (abs d))))
    (cond
      [(< n 0) (cons (/ n g) (/ d g))]
      [(< d 0) (cons (/ (* -1 n) g) (/ (abs d) g))]
      [else (cons (/ n g) (/ d g))])))

; EX 2.2
; specify a constructor "make-point" and selectors "x-point" and "y-point" that define the representation of a point on a coordinate plane as a pair of numbers
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

; distance between two points
(define (distance x y)
  (sqrt (+ (sq (- (x-point y) (x-point x))) (sq (- (y-point y) (y-point x))))))

; define a constructor, "make-segment", and selectors, "start-segment and "end-segment", that the define the representation of segments in terms of points
(define (make-segment sp ep) (cons sp ep))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

; using the new selectors and constructors, define a procedure, "midpoint-segment", that takes a line segment as argument and returns its midpoint
(define (midpoint-segment seg)
  (make-point (average (x-point (start-segment seg)) (x-point (end-segment seg)))
              (average (y-point (start-segment seg)) (y-point (end-segment seg)))))

; length of a segment
(define (seg-length seg)
  (distance (start-segment seg) (end-segment seg)))

; to test the procedure, use print-point
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; EX 2.3
; implement a representation for rectangles in a plane
; a rectangle is a list of 4 segments, a, b, c, d where:
; (end-segment a) = (start-segment b)
; (end-segment b) = (start-segment c)
; (end-segment c) = (start-segment d)
; (end-segment d) = (start-segment a)
(define (make-rectangle a b c d)
  (if (and (and (= (end-segment a) (start-segment b))
                (= (end-segment b) (start-segment c))
                (= (end-segment c) (start-segment d))
                (= (end-segment d) (start-segment a)))
           (and (= (seg-length a) (seg-length c))
                (= (seg-length b) (seg-length d))))
      (cons a (cons b (cons c d)))
      (error "Segments do not make a rectangle")))

; selectors for the segments of a rectangle
(define (side-a rect)
  (car rect))
(define (side-b rect)
  (car (cdr rect)))
(define (side-c rect)
  (car (cdr (cdr rect))))
(define (side-d rect)
  (car (cdr (cdr (cdr rect)))))

; computes the perimeter of a given rectangle
(define (rect-perim rect)
  (+ (seg-length (side-a rect)) (seg-length (side-b rect)) (seg-length (side-c rect)) (seg-length (side-d rect))))

; computes the area of a given rectangle
(define (rect-area rect)
  (* (seg-length (side-a rect)) (seg-length (side-b rect))))

; implement an alternate representation of a rectangle in a plane
; a rectangle is a list of 4 points that, in sequence, create 4 segments that meet the criteria for a rectangle
(define (make-rectangle-alt a b c d)
  (if (and (= (seg-length (make-segment a b)) (seg-length (make-segment c d)))
           (= (seg-length (make-segment b c)) (seg-length (make-segment d a))))
      (cons (make-segment a b) (cons (make-segment b c) (cons (make-segment c d) (make-segment d a))))
      (error "Points in the given order do not make a rectangle")))

  