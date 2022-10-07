;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chp9.5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 80) ; distances in terms of pixels 
(define WIDTH 100)
(define XSHOTS (/ WIDTH 2))
 
; graphical constants 
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 3 "solid" "red"))

; A List-of-shots is one of: 
; – '()
; – (cons Shot List-of-shots)
; interpretation the collection of shots fired

; A Shot is a Number.
; interpretation represents the shot's y-coordinate

; A ShotWorld is List-of-numbers. 
; interpretation each number on such a list
;   represents the y-coordinate of a shot

; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick alt-tock]
    [on-key keyh]
    [to-draw to-image]))

; ShotWorld -> ShotWorld 
; moves each shot up by one pixel 
(define (tock w)
  (cond
    [(empty? w) '()]
    [else (cons (sub1 (first w)) (tock (rest w)))]))

(check-expect (tock (cons 2 (cons 3 (cons 4 '())))) (cons 1 (cons 2 (cons 3 '()))))

; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world if the space bar is hit 
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))

(check-expect (keyh (cons 2 (cons 3 '())) " ") (cons HEIGHT (cons 2 (cons 3 '()))))

; ShotWorld -> Image 
; adds each shot y on w at (XSHOTS,y} to BACKGROUND
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w)
                       (to-image (rest w)))]))

(check-expect (to-image (cons 9 '()))
              (place-image SHOT XSHOTS 9 BACKGROUND))
(check-expect (to-image (cons 9 (cons 1 '())))
              (place-image SHOT XSHOTS 9 (place-image SHOT XSHOTS 1 BACKGROUND)))

; EX 158
; ShotWorld -> Shotworld
; moves each shot up by one pixel.
; deletes shot from world that moves off scene.
(define (alt-tock w)
  (cond
    [(empty? w) '()]
    [else (cond
            [(>= (sub1 (first w)) 0) (cons (sub1 (first w)) (alt-tock (rest w)))]
            [else (alt-tock (rest w))])]))

(check-expect (alt-tock (cons 2 (cons 3 (cons 4 '())))) (cons 1 (cons 2 (cons 3 '()))))
(check-expect (alt-tock (cons 4 (cons 3 (cons 0 '())))) (cons 3 (cons 2 '())))
(check-expect (alt-tock (cons 4 (cons 0 (cons 3 '())))) (cons 3 (cons 2 '())))
(check-expect (alt-tock (cons 0 (cons 4 (cons 3 '())))) (cons 3 (cons 2 '())))

(define BALLOON (circle 2 "solid" "red"))
(define HALL-HEIGHT 10)
(define HALL-WIDTH 10)
(define HALL-AREA (* HALL-HEIGHT HALL-WIDTH))

; EX 152
; Number image -> image
; consumes a number and an image and produces
; a vertical arrangement of n copies of the image.

(check-expect (col 2 (square 10 "solid" "black")) (above (square 10 "solid" "black") (square 10 "solid" "black")))
(check-expect (col 0 (square 10 "solid" "black")) (square 0 "solid" "black"))

(define (col n img)
  (cond
    [(zero? n) (square 0 "solid" "black")]
    [(= 1 n) img]
    [else (above img (col (sub1 n) img))]))

(define (row n img)
  (cond
    [(zero? n) (square 0 "solid" "black")]
    [(= 1 n) img]
    [else (beside img (row (sub1 n) img))]))

(define GRID (square 10 "outline" "black"))

; Number Number -> image
; consumes two numbers, overlays a grid of squares over an
; empty scene both with dimensions corresponding to the inputs.
(check-expect (create-hall 2 2) (overlay (row 2 (col 2 GRID)) (empty-scene 4 4)))

(define (create-hall x y)
  (overlay (row x (col y GRID)) (empty-scene (* x y) (* y x))))

(define HALL (create-hall HALL-WIDTH HALL-HEIGHT))

; EX 159
; create a function, "riot", that consumes how many balloons the students want to throw
; and shows one balloon dropping after another at a rate of one per second. the function
; produces the list of Posns where the balloons hit.

(define-struct pair [balloon# lob])
; A Pair is a structure (make-pair N List-of-posns)
; A List-of-posns is one of: 
; – '()
; – (cons Posn List-of-posns)
; interpretation (make-pair n lob) means n balloons 
; must yet be thrown and added to lob

; number -> List-of-posns
; see EX 159 description
(define (riot n)
  (big-bang (make-pair n '())
    [on-tick throw 1]
    [to-draw render]
    [stop-when end?]))

; pair -> pair
; accepts a pair. subtracts 1 from balloon#, creates
; a random posn, adds it to lob, and returns the new pair.
(define (throw in-pair)
  (cond
    [(zero? (pair-balloon# in-pair)) in-pair]
    [else (make-pair (sub1 (pair-balloon# in-pair)) (cons (make-posn (random (+ 1 HALL-AREA)) (random (+ 1 HALL-AREA))) (pair-lob in-pair)))]))

(check-random (throw (make-pair 5 (cons (make-posn 2 3) '())))
              (make-pair 4 (cons (make-posn (random (+ 1 HALL-AREA)) (random (+ 1 HALL-AREA))) (cons (make-posn 2 3) '()))))

; pair -> image
; adds balloons to HALL image based on the given pair
(define (render in-pair)
  (add-balloons (pair-lob in-pair)))

(check-expect (render (make-pair 4 (cons (make-posn 2 3) '()))) (place-image BALLOON 2 3 HALL))

; pair -> Boolean
; checks balloon# of a given pair.
; signals to big-bang either #true if balloon# equals 0
; #false if balloon# is greater than 0
(define (end? in-pair)
  (if (zero? (pair-balloon# in-pair)) #true #false))

(check-expect (end? (make-pair 1 '())) #false)
(check-expect (end? (make-pair 0 '())) #true)

; list-of-posns -> image
; produces an image of a 10x10 grid with red dots added
; at positions on the grid specified by the input
(define (add-balloons lops)
  (cond
    [(empty? lops) HALL]
    [else (add-balloon (first lops) (add-balloons (rest lops)))]))

(check-expect (add-balloons (cons (make-posn 2 2) (cons (make-posn 3 4)'())))
              (place-image BALLOON 2 2 (place-image BALLOON 3 4 HALL)))

; posn -> image
; adds a red dot to a given grid based on a given posn. returns the grid.
(define (add-balloon inp img)
  (place-image BALLOON (posn-x inp) (posn-y inp) img))

