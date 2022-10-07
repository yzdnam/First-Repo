;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |12.7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DEFINITIONS AND STRUCTURES

(define HEIGHT 200)
(define WIDTH 200)
(define SCREEN (empty-scene WIDTH HEIGHT))

(define TANK-WIDTH 20)
(define TANK-HEIGHT 25)
(define TANK-BODY (rectangle TANK-WIDTH 10 "solid" "green"))
(define TANK-TURRET (rectangle 5 TANK-HEIGHT "solid" "green"))
(define TANK (overlay/align "middle" "bottom" TANK-TURRET TANK-BODY))
(define UFO-WIDTH 20)
(define UFO (overlay (circle 5 "solid" "green") (rectangle UFO-WIDTH 3 "solid" "green")))
(define MISSILE (triangle 10 "solid" "black"))

(define TANK-Y (- HEIGHT 10))
(define TANK-SPEED 5)
(define UFO-SPEED 3)
(define UFO-JUMP-RANGE 30)
(define MISSILE-SPEED (* 2.1 UFO-SPEED))
(define DETONATION-PROXIMITY 10)
(define TANK-START-POSN 30)

(define TEXT-SIZE (/ HEIGHT 10))

(define-struct sigs [ufos timer tank missiles])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank Missiles)
; interpretation represents the complete state of a
; space invader game

; ufos is a list of posns
; timer is a number that countsdown every clock tick,
; once it reaches 0, another ufo spawns
 
; A Missiles is one of: 
; – '()
; – (cons Posn Missles)
; interpretation '() means the missile is in the tank;
; Posn says each missile is at that location

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 

; si-main
; starts game
; accepts two numbers, the first is how many seconds a clock tick takes
; the second number determines how long it takes for another ufo to spawn
(define (si-main x)
  (big-bang (make-sigs
             (make-posn (random WIDTH) 0)
             (make-tank TANK-START-POSN TANK-SPEED)
             '())
    [on-key si-control]
    [on-tick si-move x]
    [to-draw si-render.v2]
    [stop-when si-game-over? si-render-final]))

; SIGS.v2 -> Image 
; renders the given game state on top of BACKGROUND 
(define (si-render.v2 s)
  (render-tank
    (sigs-tank s)
    (render-ufo (sigs-ufo s)
                (missile-render.v2 (sigs-missiles s)
                                   SCREEN))))

; si-render-final
; game over screen, same as render except with "game over" on the screen
(define (si-render-final x)
  (cond
   [(landed? (sigs-ufo x)) (render-text "GAME OVER" "red" (si-render.v2 x))]
   [(check-if-hit (sigs-missiles x) (sigs-ufo x))
     (render-text "YOU WIN" "green" (si-render.v2 x))]
                 ))

; missiles ufo -> boolean
; checks if a missle has hit the ufo
(define (check-if-hit missi uf)
  (cond
    [(empty? missi) #false]
    [else (cond
            [(<= (distance uf (first missi)) DETONATION-PROXIMITY) #true]
            [else (check-if-hit (rest missi) uf)])]))

; string string Image -> Image
; adds string to given image
(define (render-text word color im)
  (place-image/align (text word TEXT-SIZE color) (/ HEIGHT 2) (/ HEIGHT 2) "center" "center" im)
  )

; landed?
; UFO -> Boolean
; returns True if UFO has landed
(define (landed? ufo)
  (if (>= (posn-y ufo) HEIGHT) #true #false)
  )

; distance formula
; Posn Posn -> Number
(define (distance x y)
 ( sqrt (+ (sqr (- (posn-x x) (posn-x y)))  (sqr (-(posn-y x) (posn-y y)))) )
  )

; Tank Image -> Image 
; adds t to the given image im
(define (render-tank t im)
  (place-image TANK (tank-loc t) TANK-Y im)
  )

; UFO Image -> Image 
; adds u to the given image im
(define (render-ufo u im)
   (place-image UFO (posn-x u) (posn-y u) im)
  )

; MissileOrNot Image -> Image 
; adds an image of missile m to scene s

;(check-expect (missile-render.v2 (make-posn 32 (- HEIGHT TANK-HEIGHT 10)) SCREEN)
;              (place-image MISSILE 32 (- HEIGHT TANK-HEIGHT 10) SCREEN))

(define (missile-render.v2 m s)
  (cond
    [(empty? m) s]
    [else (place-image MISSILE (posn-x (first m)) (posn-y (first m)) (missile-render.v2 (rest m) s))]))

; GAME-OVER?
; SIGS -> Boolean
; function fed to the stop-when handler to determine if the program has met
; end-state criteria
; game is over if one of the following conditions is met:
; -UFO lands on earth
; -missile is within detonation proximity of UFO
(define (si-game-over? game)
 (cond
  [(landed? (sigs-ufo game)) #true]
  [(check-if-hit (sigs-missiles game) (sigs-ufo game)) #true]
  [else #false]
   )              
  )

; si-move
; SIGS -> SIGS
; called on-tick; moves the missile and tank in straight lines at constant speeds.
; the UFO moves in straight line and makes small random jumps to the left or right
(define (si-move w)
 (make-sigs
       (move-ufo (sigs-ufo w))
       (move-tank (sigs-tank w))
       (move-missile (sigs-missiles w))
    ))

; move-ufo
; UFO -> UFO
; moves ufo a given amount; helper function for si-move-proper
(define (move-ufo u)
  (make-posn (ufo-jump (random WIDTH) (posn-x u) UFO-JUMP-RANGE) (+ UFO-SPEED (posn-y u)))
  )

; ufo-jump
; Number Number Number -> Number
; return number within a given interval from the input number
; x will be random
; w will be the ufo's current x-coordinate
; range will be the limit of the ufo's jump range
(define (ufo-jump x w range)
  (cond
    [(<= (abs (- x w)) range) x]
    [(and (> (abs (- x w)) range) (> x w) (< (+ w range) (- WIDTH UFO-WIDTH))) (+ w range)]
    [(and (> (abs (- x w)) range) (> x w) (>= (+ w range) (- WIDTH UFO-WIDTH))) (- WIDTH UFO-WIDTH)]
    [(and (> (abs (- x w)) range) (< x w)(> (- w range) UFO-WIDTH)) (- w range)]
    [(and (> (abs (- x w)) range) (< x w)(<= (- w range) UFO-WIDTH)) UFO-WIDTH]
    )
  )

; move-tank
; Tank -> Tank
; moves tank; helper function for si-move-proper
(define (move-tank t)
  (cond
    [(or (>= (tank-loc t)(- WIDTH TANK-WIDTH)) (<= (tank-loc t) TANK-WIDTH))
      (make-tank (- (tank-loc t) (tank-vel t)) (* -1 (tank-vel t)))]
    [else (make-tank (+ (tank-vel t) (tank-loc t)) (tank-vel t))]
  )
)

; move-missile
; Missile -> Missile
; moves missile; helper function for si-move-proper
(define (move-missile m)
  (cond
    [(empty? m) '()]
    [else (cons (make-posn (posn-x (first m)) (- (posn-y (first m)) MISSILE-SPEED)) (move-missile (rest m)))]
  ))

(check-expect (move-missile (list (make-posn 10 10) (make-posn 12 12))) (list (make-posn 10 (- 10 MISSILE-SPEED)) (make-posn 12 (- 12 MISSILE-SPEED))))

; si-control
; SIGS KeyEvent -> SIGS
; reacts to "left" "right" and "space"
; "left" makes the tank move left
; "right" makes the tank move right
; "space" shoots the missle
(define (si-control game ke)
      (cond
       [(equal? ke "left")
        (make-sigs
         (sigs-ufo game)
         (make-tank (tank-loc (sigs-tank game)) (cond
                                      [(> (tank-vel (sigs-tank game)) 0) (* -1 (tank-vel (sigs-tank game)))]
                                      [else (tank-vel (sigs-tank game))]
                                      ))
         (sigs-missiles game)
         )]
       [(equal? ke "right")
        (make-sigs
         (sigs-ufo game)
         (make-tank (tank-loc (sigs-tank game)) (cond
                                      [(< (tank-vel (sigs-tank game)) 0) (* -1 (tank-vel (sigs-tank game)))]
                                      [else (tank-vel (sigs-tank game))]
                                      ))
         (sigs-missiles game)
         )]
       [(equal? ke " ")
        (make-sigs
         (sigs-ufo game)
         (sigs-tank game)
         (cons (make-posn (tank-loc (sigs-tank game)) (- HEIGHT TANK-HEIGHT)) (sigs-missiles game)))]
       [else game]
        )
    )

