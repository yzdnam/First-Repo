;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |12.5|) (read-case-sensitive #t) (teachpacks ((lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "itunes.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define RADIUS 5)
(define DIAMETER (* 2 RADIUS))
(define WORM (circle RADIUS "solid" "red"))

(define HEIGHT (* 30 DIAMETER))
(define WIDTH (* 30 DIAMETER))
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

(define EXAMPLE-WORM (list (make-posn 140 150) (make-posn 150 150)))

; number -> worm-game
; given number determines how fast the worm moves in the game
(define (worm-main x)
  (measure-worm (big-bang (make-worm-game (make-posn (random WIDTH) (random HEIGHT))
                            (list
                                  (make-posn (- (/ WIDTH 2) (* 2 DIAMETER)) (/ HEIGHT 2))
                                  (make-posn (- (/ WIDTH 2) DIAMETER) (/ HEIGHT 2))
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
  (food-check-create
     p (make-posn (random WIDTH) (random HEIGHT))))
 
; Posn Posn -> Posn 
; generative recursion 
; uses the input to food-create to generate a different and random posn
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))
 
; Posn -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

; worm-game -> image
; draws game based on new state
(define (render game)
  (place-image FOOD (posn-x (worm-game-food game)) (posn-y (worm-game-food game)) (render-list (worm-game-lopsns game))))

; list-of-posns -> image
; renders a list-of-posns as segments of a worm on the scene
(define (render-list lopsns)
  (cond
    [(empty? (rest lopsns))
     (place-image WORM (posn-x (first lopsns)) (posn-y (first lopsns)) SCENE)]
    [else
     (place-image WORM (posn-x (first lopsns)) (posn-y (first lopsns)) (render-list (rest lopsns)))]))

(check-expect (render-list (list (make-posn 140 150) (make-posn 150 150)))
              (place-image WORM 140 150 (place-image WORM 150 150 SCENE)))

; worm-game -> image
; draws game based on new state if game is over
(define (render-end game)
  (cond
    [(or
      (< (posn-x (first (worm-game-lopsns game))) 0)
      (< (posn-y (first (worm-game-lopsns game))) 0)
      (> (posn-x (first (worm-game-lopsns game))) WIDTH)
      (> (posn-y (first (worm-game-lopsns game))) HEIGHT))
       (place-image (text "worm hit border" TEXT-SIZE "red") (* 0.25 WIDTH) (* 0.75 HEIGHT)
                    (render game))]
    [else
     (render-self-end game)]))

; worm-game -> image
; draws game based on new state if game is over
(define (render-self-end game)
  (place-image (text "worm ate self!" TEXT-SIZE "red") (* 0.25 WIDTH) (* 0.75 HEIGHT)
               (render game)))

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
  (cond
    [(<= (dist (first (worm-game-lopsns game)) (worm-game-food game)) DIAMETER)
     (make-worm-game (food-create (worm-game-food game)) (grow-worm game) (worm-game-direction game))]
    [(equal? (worm-game-direction game) "left")
     (make-worm-game (worm-game-food game) (remove-last (append-segment-left (worm-game-lopsns game))) "left")]
    [(equal? (worm-game-direction game) "right")
     (make-worm-game (worm-game-food game) (remove-last (append-segment-right (worm-game-lopsns game))) "right")]
    [(equal? (worm-game-direction game) "up")
     (make-worm-game (worm-game-food game) (remove-last (append-segment-up (worm-game-lopsns game))) "up")]
    [(equal? (worm-game-direction game) "down")
     (make-worm-game (worm-game-food game) (remove-last (append-segment-down (worm-game-lopsns game))) "down")]))

(check-expect (move (make-worm-game (make-posn 10 10) (list (make-posn 140 150) (make-posn 150 150)) "down"))
              (make-worm-game (make-posn 10 10) (list (make-posn 140 160) (make-posn 140 150)) "down"))

; posn posn -> number
; returns distance between two points
(define (dist p1 p2)
  (sqrt (+ (sqr (- (posn-x p2) (posn-x p1))) (sqr (- (posn-y p2) (posn-y p1))))))

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
  (cond
    [(empty? (rest lopsns)) (list (make-posn (- (posn-x (first lopsns)) DIAMETER) (posn-y (first lopsns))))]
    [else (cons (make-posn (- (posn-x (first lopsns)) DIAMETER) (posn-y (first lopsns))) lopsns)]))

(check-expect (append-segment-left (list (make-posn 140 150) (make-posn 150 150))) (list (make-posn 130 150) (make-posn 140 150) (make-posn 150 150)))

; analogous to above function for moving right
(define (append-segment-right lopsns)
  (cond
    [(empty? (rest lopsns)) (list (make-posn (+ (posn-x (first lopsns)) DIAMETER) (posn-y (first lopsns))))]
    [else (cons (make-posn (+ (posn-x (first lopsns)) DIAMETER) (posn-y (first lopsns))) lopsns)]))

(check-expect (append-segment-right (list (make-posn 140 150) (make-posn 150 150))) (list (make-posn 150 150) (make-posn 140 150) (make-posn 150 150)))

; analogous to above function for moving up
(define (append-segment-up lopsns)
  (cond
    [(empty? (rest lopsns)) (list (make-posn (posn-x (first lopsns)) (- (posn-y (first lopsns)) DIAMETER)))]
    [else (cons (make-posn (posn-x(first lopsns)) (- (posn-y (first lopsns)) DIAMETER)) lopsns)]))

(check-expect (append-segment-up (list (make-posn 140 150) (make-posn 150 150))) (list (make-posn 140 140) (make-posn 140 150) (make-posn 150 150)))

; analogous to above function for moving
(define (append-segment-down lopsns)
  (cond
    [(empty? (rest lopsns)) (list (make-posn (posn-x (first lopsns)) (+ (posn-y (first lopsns)) DIAMETER)))]
    [else (cons (make-posn (posn-x(first lopsns)) (+ (posn-y (first lopsns)) DIAMETER)) lopsns)]))
