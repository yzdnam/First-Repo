;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |27|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define SMALL 4) ; a size measure in terms of pixels 
 
(define small-triangle (triangle SMALL 'outline 'red))
 
; Number -> Image
; generative creates Sierpinski Δ of size side by generating
; one for (/ side 2) and placing one copy above two copies
 
(check-expect (sierpinski SMALL) small-triangle)
(check-expect (sierpinski (* 2 SMALL))
              (above small-triangle
                     (beside small-triangle small-triangle)))
 
(define (sierpinski side)
  (cond
    [(<= side SMALL) (triangle side 'outline 'red)]
    [else
     (local ((define half-sized (sierpinski (/ side 2))))
       (above half-sized (beside half-sized half-sized)))]))

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous 
; (2) (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in 
; one of the two halves, picks according to (2)
(define ε 0.1)
(define (find-root f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
      (local ((define mid (/ (+ left right) 2))
              (define f@mid (f mid)))
        (cond
          [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
           (find-root f left mid)]
          [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
           (find-root f mid right)]))]))

; EX 445
; use poly to formulate a check-satisfied test for find-root
; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

(check-satisfied (find-root poly 3 6) close-to-2-or-4?)

(define (close-to-2-or-4? x)
  (or (<= (abs (- x 2)) 0.1)
      (<= (abs (- x 4)) 0.1)))

; EX 446 experiment with different values for \epsilon

; EX 447 use find-root with poly and an interval that contains both roots
; any interval containing both roots of poly will have a left and right limit that are both positive which will cause cond to signal an error because
; all conditions will be false. this aligns with one of the conditions for the intermediate value theorem, namely that, given an interval of [a,b],
; f(a) and f(b) must be on opposite sides of x if the theorem is to hold for the function (the other condition being that the function must be continuous)

; EX 448 find-root terminates for all (continuous) f, left, and right for which the assumption holds because the difference
; between (f left) or (f right) and (f@mid) will approach zero after every call of the function

; EX 449 re-write find-root so it doesn't compute the value of f for each boundary value more than once and design
; a helper function that consumes (f left) and (f right) at each recursive stage, saving computational steps during the
; execution of the function
(define (find-root.449 f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
      (local ((define mid (/ (+ left right) 2))
              (define f@mid (f mid))
              (define f-left (f left))
              (define f-right (f right))
              (define (find-root-helper left right f-left f-right)
                
        (cond
          [(or (<= f-left 0 f@mid) (<= f@mid 0 f-left))
           (find-root.449 f left mid)]
          [(or (<= f@mid 0 f-right) (<= f-right 0 f@mid))
           (find-root.449 f mid right)]))]))

(check-satisfied (find-root.449 poly 3 6) close-to-2-or-4?)
