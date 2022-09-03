;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |16.5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [list-of Posn] -> [list-of Posn]
; adds 3 to the x-coordinates of each Posn in the given list
(define (add-3-to-all lops)
  (local (
          ;Posn -> Posn
          ;adds 3 the x-coordinate of a Posn
          (define (add-3-to-one pos)
            (make-posn (+ 3 (posn-x pos)) (posn-y pos))))
    (map add-3-to-one lops)))

(check-expect
 (add-3-to-all
   (list (make-posn 3 1) (make-posn 0 0)))
 (list (make-posn 6 1) (make-posn 3 0)))

; [List-of Posn] -> [List-of Posn]
; elminates all posns with y-coords larger than 100 from the given list
(define (remove>100y lops)
  (local (;Posn -> Boolean
          (define (y<100? pos)
            (cond
              [(< (posn-y pos) 100) #true]
              [else #false])))
    (filter y<100? lops)))

(check-expect (remove>100y (list (make-posn 20 101) (make-posn 20 98))) (list (make-posn 20 98)))

; [List-of Posn] Posn -> Boolean
; determines whether any of a list of Posns is close to some given position
; where "close means a distance of 5 pixels or less
(define (any-close? lop pt)
  (local (; Posn  -> Number
          ; returns the distance in pixels between the given point and the point from the parent function
          (define (distance p1)
            (sqrt (+ (sqr (- (posn-x pt) (posn-x p1))) (sqr (- (posn-y pt) (posn-y p1))))))

          ; Posn -> Boolean
          ; determines whether a given position is at a distance of 5 pixels or less from the point given in the parent function
          (define (close? p1)
            (<= (distance p1) 5)))
    (ormap close? lop)))

(check-expect (any-close? (list (make-posn 1 1) (make-posn 20 20)) (make-posn 2 2)) #true)
(check-expect (any-close? (list (make-posn 1 1) (make-posn 20 20)) (make-posn 102 2)) #false)