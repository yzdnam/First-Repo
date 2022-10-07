;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |33.3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; EX 525 - design the helper functions for add-sierpinski

(define TOO-SMALL-THRESH 3)

; Image Posn Posn Posn -> Image 
; adds the black triangle a, b, c to scene
(define (add-triangle scene a b c) (scene+line (scene+line (scene+line scene
                                                                       (posn-x a) (posn-y a) (posn-x b) (posn-y b) "black")
                                                           (posn-x b) (posn-y b) (posn-x c) (posn-y c) "black")
                                               (posn-x c) (posn-y c) (posn-x a) (posn-y a) "black"))
 
; Posn Posn Posn -> Boolean 
; is the triangle a, b, c too small to be divided
(define (too-small? a b c)
  (local ((define (distance a b)
            (sqrt (+ (sqr (- (posn-x a) (posn-x b))) (sqr (- (posn-y a) (posn-y b)))))))
    (or
     (< (distance a b) TOO-SMALL-THRESH)
     (< (distance b c) TOO-SMALL-THRESH)
     (< (distance c a) TOO-SMALL-THRESH))))
 
; Posn Posn -> Posn 
; determines the midpoint between a and b
(define (mid-point a b)
  (make-posn (/ (+ (posn-x a) (posn-x b)) 2) (/ (+ (posn-y a) (posn-y b)) 2)))

; Image Posn Posn Posn -> Image 
; generative adds the triangle (a, b, c) to scene0, 
; subdivides it into three triangles by taking the 
; midpoints of its sides; stop if (a, b, c) is too small
; accumulator the function accumulates the triangles of scene0
(define (add-sierpinski scene0 a b c)
  (cond
    [(too-small? a b c) scene0]
    [else
      (local
        ((define scene1 (add-triangle scene0 a b c))
         (define mid-a-b (mid-point a b))
         (define mid-b-c (mid-point b c))
         (define mid-c-a (mid-point c a))
         (define scene2
           (add-sierpinski scene1 a mid-a-b mid-c-a))
         (define scene3
           (add-sierpinski scene2 b mid-b-c mid-a-b)))
      ; —IN—
     (add-sierpinski scene3 c mid-c-a mid-b-c))]))

(define MT (empty-scene 400 400))
(define A (make-posn 200  50))
(define B (make-posn  27 350))
(define C (make-posn 373 350))

; EX 526 - design circle-pt

(define CENTER (make-posn 200 200))
(define RADIUS 200) ; the radius in pixels 
 
; Number -> Posn
; determines the point on the circle with CENTER 
; and RADIUS whose angle is 
 
; examples
; what are the x and y coordinates of the desired 
; point, when given: 120/360, 240/360, 360/360
 
(define (circle-pt degrees cent-x cent-y len)
  (local ((define (radian-factor degrees)
            (* (/ degrees 360) (* 2 pi)))
          (define circle-x (+ cent-x (* len (cos (radian-factor degrees)))))
          (define circle-y (+ cent-y (* len (sin (radian-factor degrees))))))
    (make-posn circle-x circle-y)))

; EX 527 - design add-savannah
; Image Number Number Number Number -> Image
; four numbers consumed are the x-coord of a line's base point, the y-coord of a line's base point, the length of the line, and the angle of the line
(define THRESHOLD 5)
(define (add-savannah scene0 a b len ang)
  (cond
    [(<= len THRESHOLD) scene0]
    [else
     (local
       ((define end-pt (circle-pt ang a b len))
        (define scene1 (scene+line scene0 a b (posn-x end-pt) (posn-y end-pt) "red"))
        (define new-start1 (circle-pt ang a b (/ len 3)))
        (define new-start2 (circle-pt ang a b (/ (* 2 len) 3)))
        (define scene2
          (add-savannah scene1 (posn-x new-start1) (posn-y new-start1) (* (/ 2 3) len) (- ang 12))))
     (add-savannah scene2 (posn-x new-start2) (posn-y new-start2) (* (/ 4 5) len) (+ ang 10)))]))
       
(add-savannah MT 200 300 100 270)

; IMG posn posn posn -> IMG
; draws a curve from point a to c from the perspective of point b
(define (smooth-curve scene0 a b c)
  (cond
    [(too-small? a b c) (scene+line scene0 (posn-x a) (posn-y a) (posn-x c) (posn-y c) "red")]
    [else
     (local ((define a-b-mid (mid-point a b))
             (define b-c-mid (mid-point b c))
             (define a-b-c-mid (mid-point a-b-mid b-c-mid))
             (define first-half-curve (smooth-curve scene0 a a-b-mid a-b-c-mid)))
       (smooth-curve first-half-curve a-b-c-mid b-c-mid c))]))
(smooth-curve MT (make-posn 100 100) (make-posn 200 100) (make-posn 150 200))
  