;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |28|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ϵ 0.01)
(define (poly x) (* (- x 2) (- x 4)))
; EX 455
; [Number -> Number] Number -> Number
; maps the given function, f, and number, r1, to the slope of f at r1, obtained by using two close points as arguments to the function, both a distance of ϵ from r1 on both sides,
; and their outputs with f
(define (slope f r1)
  (cond
    [(number? f) 0]
    [else
      (* (/ 1 (* ϵ 2)) (- (f (+ r1 ϵ)) (f (- r1 ϵ))))]))

(check-expect (slope (lambda (x) (* 2 x)) 0) 2)
(check-expect (slope 1 4) 0)

; EX 456
; [Number -> Number] Number -> Number
; maps f and r1 to the root of the tangent running through (r1, (f r1))
(define (root-of-tangent f r1)
  (- r1 (/ (f r1) (slope f r1))))


; [Number -> Number] Number -> Number
; finds a number r such that (<= (abs (f r)) ε)
 
(check-within (newton poly 1) 2 ϵ)
(check-within (newton poly 3.5) 4 ϵ)
 
(define (newton f r1)
  (cond
    [(<= (abs (f r1)) ϵ) r1]
    [else (newton f (root-of-tangent f r1))]))

; EX 457
; Number Number -> Number
; computes how many months it takes to double a given amount of money when a savings account pays interest at a fixed rate on a monthly basis
(define (double-amount princ int)
  (local ((define monthly-rate (/ int 12))
          (define (compound bal mos)
            (cond
              [(>= bal (* 2 princ)) mos]
              [else (compound (+ bal (* bal monthly-rate)) (add1 mos))])))
    (compound princ 0)))

(define (double-amt-improved int)
  (local ((define monthly-rate (/ int 12))
          (define (compound mos)
            (cond
              [(>= (expt (+ monthly-rate 1) mos) 2) mos]
              [else (compound (add1 mos))])))
    (compound 0)))

(define ε 0.1)
 
; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds 
 
;(check-within (integrate (lambda (x) 20) 12 22) 200 ϵ)
;(check-within (integrate (lambda (x) (* 2 x)) 0 10) 100 ϵ)
;(check-within (integrate (lambda (x) (* 3 (sqr x))) 0 10)
;              1000
;              ϵ)
 
(define (integrate f a b) #i0.0)

; EX 458
; design a function that uses the kepler method to determine the area under the graph of a function
(check-within (integrate-kepler (lambda (x) 20) 12 22) 200 ϵ)
(check-within (integrate-kepler (lambda (x) (* 2 x)) 0 10) 100 ϵ)
;(check-within (integrate-kepler (lambda (x) (* 3 (sqr x))) 0 10) ; fails by 125
;              1000
;              ϵ)
 
(define (integrate-kepler f a b)
  (local ((define mid (/ (+ a b) 2))
          (define (trap-area a b)
            (* (/ 1 2) (- b a) (+ (f a) (f b)))))
    (+ (trap-area a mid) (trap-area mid b))))

; EX 459
; design a function that determines the area under the graph of a function using the rectangles method
(check-within (integrate-rectangles (lambda (x) 20) 12 22) 200 ϵ)
(check-within (integrate-rectangles (lambda (x) (* 2 x)) 0 10) 100 ϵ)
(check-within (integrate-rectangles (lambda (x) (* 3 (sqr x))) 0 10) 
              1000
              ϵ)

(define R 159) ; cross-over point for (* 3 (sqr x))) and an ϵ of 0.01
(define (integrate-rectangles f a b)
  (local ((define rect-width (/ (- b a) R))
          (define rect-wid-midpoint (/ rect-width 2))
          (define (accum-rect-areas i)
            (local ((define rect-x-midpoint (+ a (* rect-width i) rect-wid-midpoint)))
              (cond
                [(equal? i R) 0]
                [else
                 (+ (* rect-width (f rect-x-midpoint))
                    (accum-rect-areas (add1 i)))]))))
    (accum-rect-areas 0)))

; EX 460
; integrates a function f between the boundaries a and b using a divide-and-conquer strategy. use kepler's method when the
; interval is sufficiently small
(check-within (integrate-dc (lambda (x) 20) 12 22) 200 ϵ)
(check-within (integrate-dc (lambda (x) (* 2 x)) 0 10) 100 ϵ)
(check-within (integrate-dc (lambda (x) (* 3 (sqr x))) 0 10) 
              1000
              ϵ)

(define (integrate-dc f a b)
  (local ((define mid (/ (+ a b) 2))
          (define integrate-combo f a b) ; TODO recursively divide the interval until it is small THEN integrate using kepler's
                                         ; method
    (+ (integrate-combo f a mid) (integrate-combo f mid b))