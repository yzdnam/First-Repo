#lang sicp
(#%require sicp-pict)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

; EX 2.44
; Define the procedure up-split, similar to right-split, to be used by corner-split
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

; EX 2.45
; right-split and up-split can be expressed as instances of a general splitting operation. Define a procedure split with the property that evaluating:
; (define right-split (split beside below))
; (define up-split (split below beside))
; produces procedures right-split and up-split with the same behaviors as the ones already defined
(define (split primary-op subs-op)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split primary-op subs-op) painter (- n 1))))
          (primary-op painter (subs-op smaller smaller))))))

(define right-split-abs (split beside below))
(define up-split-abs (split below beside))

;(define (frame-coord-map frame)
;  (lambda (v)
;    (add-vect
;     (origin-frame frame)
;     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
;               (scale-vect (ycor-vect v) (edge2-frame frame))))))

; EX 2.46
; implement a data abstraction for vectors by defining a constructor, make-vect, and corresponding selectors, xcor-vect and ycor-vect
; implement procedures add-vect, sub-vect, and scale-vect
(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1) (xcor-vect vect2))
             (+ (ycor-vect vect1) (ycor-vect vect2))))

(define (sub-vect vect1 vect2)
  (make-vect (- (xcor-vect vect1) (xcor-vect vect2))
             (- (ycor-vect vect1) (ycor-vect vect2))))

(define (scale-vect scalar vect)
  (make-vect (* scalar (xcor-vect vect)) (* scalar (ycor-vect vect))))

; EX 2.47
; a frame consists of 3 vectors
; for each of the following two possible constructors for frames, supply the appropriate selectors to produce an implementation for frames
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

(define (make-frame-alt origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-alt frame)
  (car frame))
(define (edge1-frame-alt frame)
  (car (cdr frame)))
(define (edge2-frame-alt frame)
  (cdr (cdr frame)))

; segments->painter does not work with #lang sicp
; see continuation file of 2.2.4 for an implementation of a segments->painter procedure using racket's graphic libraries
;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame)
;         (start-segment segment))
;        ((frame-coord-map frame)
;         (end-segment segment))))
;     segment-list)))

; EX 2.48
; a line segment can be represented as a pair of vectors - the vector running from the origin to the start-point of the segment and the vector running from the
; origin to the end-point of the segment.
; use the vector representation from EX 2.46 to define a representation for segments with a constructor, make-segment, and selectors start-segment and end-segment
(define (make-segment sp-vect ep-vect)
  (cons sp-vect ep-vect))

(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

; EX 2.49 in pt2
