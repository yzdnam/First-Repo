;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chp16ex278) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define RADIUS 5)
(define DIAMETER (* 2 RADIUS))
(define WORM (circle RADIUS "solid" "red"))

(define UNIT-SIZE 30)
(define HEIGHT (* UNIT-SIZE DIAMETER))
(define WIDTH (* UNIT-SIZE DIAMETER))
(define SCENE (empty-scene WIDTH HEIGHT))

(define TEXT-SIZE (* 2 DIAMETER))

(define FOOD (circle RADIUS "solid" "green"))

(define-struct worm-game [food lopsns direction])
; a direction is a string
; a posn is a structure composed of 2 numbers
; food is a posn

; a worm-with-tail is one of the following:
; - (cons worm '())
; - (cons worm worm-with-tail) 

; a worm is a posn

; posn posn -> number
; returns distance between two points
(define (dist p1 p2)
  (sqrt (+ (sqr (- (posn-x p2) (posn-x p1))) (sqr (- (posn-y p2) (posn-y p1))))))

(define EXAMPLE-WORM (list (make-posn 140 150) (make-posn 150 150)))

; number -> worm-game
; given number determines how fast the worm moves in the game
(define (worm-main x)
  (measure-worm (big-bang (make-worm-game
                           (make-posn (random WIDTH) (random HEIGHT))
                           (list
                             (make-posn (/ WIDTH 2) (/ HEIGHT 2)))
                           "down")
    [to-draw render]
    [on-key handle-key]
    [on-tick move x]
    [stop-when hit-something-bad? render-end]
    )))

; worm-game -> number
; returns the length of the worm after the world program is shutdown
(define (measure-worm game)
  (length (worm-game-lopsns game)))

; Posn -> Posn 
; creates a random posn
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create p)
  (local (
          ; Posn Posn -> Posn 
          ; generative recursion 
          ; uses the input to food-create to generate a different and random posn
          (define (food-check-create p candidate)
            (if (equal? p candidate) (food-create p) candidate)))
    
    (food-check-create
      p (make-posn (* DIAMETER (random UNIT-SIZE)) (* DIAMETER (random UNIT-SIZE))))))
 
 
; Posn -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

; worm-game -> image
; draws game based on new state
(define (render game)
  (local (
          ; list-of-posns -> image
          ; renders a list-of-posns as segments of a worm on the scene
          (define render-list
            (local (
                    (define (render-segment psn base)
                      (place-image WORM (posn-x psn) (posn-y psn) base)))
              (foldr render-segment SCENE (worm-game-lopsns game)))))
          
  (place-image FOOD (posn-x (worm-game-food game)) (posn-y (worm-game-food game)) render-list)))


;(check-expect (render-list (list (make-posn 140 150) (make-posn 150 150)))
;              (place-image WORM 140 150 (place-image WORM 150 150 SCENE)))

;(check-expect (render-list (list (make-posn 140 150) ))
;              (place-image WORM 140 150 SCENE))

; worm-game -> image
; draws game based on new state if game is over
(define (render-end game)
  (local (
          ; worm-game -> image
          ; draws game based on new state if game is over
          (define render-self-end
            (place-image (text "worm ate self!" TEXT-SIZE "red") (* 0.25 WIDTH) (* 0.75 HEIGHT)
                         (render game))))
  (cond
    [(hit-something-bad? game)
     (place-image (text "worm hit border" TEXT-SIZE "red") (* 0.25 WIDTH) (* 0.75 HEIGHT)
                  (render game))]
    [else
     render-self-end])))

; worm-game -> boolean
; returns #true if the worm has hit a border
; #false if not
(define (hit-something-bad? game)
  (cond
    [(or
      (< (posn-x (first (worm-game-lopsns game))) 0)
      (< (posn-y (first (worm-game-lopsns game))) 0)
      (> (posn-x (first (worm-game-lopsns game))) WIDTH)
      (> (posn-y (first (worm-game-lopsns game))) HEIGHT)
      (member? (first (worm-game-lopsns game)) (rest (worm-game-lopsns game))))
     #true]
    [else #false]))

; worm-game string -> worm-game
; creates new game state based on keys pressed
(define (handle-key game key)
  (cond
    [(equal? key "left") (chg-direc game "left")]
    [(equal? key "right") (chg-direc game "right")]
    [(equal? key "up") (chg-direc game "up")]
    [(equal? key "down") (chg-direc game "down")]
    [else game]))

; worm-game string -> worm-game
; changes direction of given worm-game to given string
(define (chg-direc game str)
  (make-worm-game (worm-game-food game) (worm-game-lopsns game) str))

; worm-game -> worm-game
; moves the worm based on it's direction
(define (move game)
  (local (; x and y are how much the worm needs to move along either axis
          ; direc is the string signifying direction
          (define (handle-direction x y direc append-func)
            (cond
              [(equal? (length (worm-game-lopsns game)) 1)
               (make-worm-game (worm-game-food game) (list (make-posn (- (posn-x (first (worm-game-lopsns game))) x) (- (posn-y (first (worm-game-lopsns game))) y))) direc)]
              [else (make-worm-game (worm-game-food game) (remove-last (append-func (worm-game-lopsns game))) direc)])))
  (cond
    [(<= (dist (first (worm-game-lopsns game)) (worm-game-food game)) DIAMETER)
     (make-worm-game (food-create (worm-game-food game)) (grow-worm game) (worm-game-direction game))]
    [(equal? (worm-game-direction game) "left") (handle-direction DIAMETER 0 "left" append-segment-left)]
    [(equal? (worm-game-direction game) "right") (handle-direction (* DIAMETER -1) 0 "right" append-segment-right)]
    [(equal? (worm-game-direction game) "up") (handle-direction 0 DIAMETER "up" append-segment-up)]
    [(equal? (worm-game-direction game) "down") (handle-direction 0 (* DIAMETER -1) "down" append-segment-down)])))

(check-expect (move (make-worm-game (make-posn 10 10) (list (make-posn 140 150) (make-posn 150 150)) "down"))
              (make-worm-game (make-posn 10 10) (list (make-posn 140 160) (make-posn 140 150)) "down"))


; game -> lopsns
; adds a segment to the worms tail where it's head was one clock-tick before
(define (grow-worm game)
  (cond
    [(equal? (worm-game-direction game) "left")
     (append-segment-left (worm-game-lopsns game))]
    [(equal? (worm-game-direction game) "right")
     (append-segment-right (worm-game-lopsns game))]
    [(equal? (worm-game-direction game) "up")
     (append-segment-up (worm-game-lopsns game))]
    [(equal? (worm-game-direction game) "down")
     (append-segment-down (worm-game-lopsns game))]))

; lopsns -> lopsns
; removes the last posn from a given lopsns
(define (remove-last lopsns)
  (reverse (rest (reverse lopsns))))

(check-expect (remove-last (list (make-posn 140 150) (make-posn 150 150))) (list (make-posn 140 150)))

; lopsns -> lopsns
; appends a new posn to a list of posns that is a diameter's worth of pixels to the left of the newest posn
(define (append-segment-left lopsns)
   (cons (make-posn (- (posn-x (first lopsns)) DIAMETER) (posn-y (first lopsns))) lopsns))

(check-expect (append-segment-left (list (make-posn 140 150) (make-posn 150 150))) (list (make-posn 130 150) (make-posn 140 150) (make-posn 150 150)))
(check-expect (append-segment-left (list (make-posn 140 150))) (list (make-posn 130 150) (make-posn 140 150) ))


; analogous to above function for moving right
(define (append-segment-right lopsns)
    (cons (make-posn (+ (posn-x (first lopsns)) DIAMETER) (posn-y (first lopsns))) lopsns))

(check-expect (append-segment-right (list (make-posn 140 150) (make-posn 150 150))) (list (make-posn 150 150) (make-posn 140 150) (make-posn 150 150)))

; analogous to above function for moving up
(define (append-segment-up lopsns)
    (cons (make-posn (posn-x(first lopsns)) (- (posn-y (first lopsns)) DIAMETER)) lopsns))

(check-expect (append-segment-up (list (make-posn 140 150) (make-posn 150 150))) (list (make-posn 140 140) (make-posn 140 150) (make-posn 150 150)))

; analogous to above function for moving
(define (append-segment-down lopsns)
    (cons (make-posn (posn-x(first lopsns)) (+ (posn-y (first lopsns)) DIAMETER)) lopsns))
