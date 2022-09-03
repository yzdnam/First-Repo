;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chp16ex277) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define TANK-Y (- HEIGHT 10))
(define TANK-SPEED 5)
(define TANK-START-POSN 30)

(define UFO-WIDTH 20)
(define UFO (overlay (circle 5 "solid" "green") (rectangle UFO-WIDTH 3 "solid" "green")))
(define UFO-SPEED 3)
(define UFO-JUMP-RANGE 30)

(define MISSILE (triangle 10 "solid" "black"))
(define MISSILE-SPEED (* 2.1 UFO-SPEED))
(define DETONATION-PROXIMITY 10)

(define CHARGE (overlay (rectangle 15 10 "solid" "red") (rectangle 17 12 "solid" "black")))

(define TEXT-SIZE (/ HEIGHT 10))

(define-struct sigs [ufo charge tank missiles])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank Missiles)
; interpretation represents the complete state of a
; space invader game

; ufos is a list of posns
; timer is a number that countsdown every clock tick,
; once it reaches 0, another ufo spawns

; a charge is either #false or a posn
 
; A Missiles is one of: 
; – '()
; – (cons Posn Missles)
; interpretation '() means the missile is in the tank;
; Posn says each missile is at that location

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, TANK-Y) and the tank's speed: dx pixels/tick

; distance formula
; Posn Posn -> Number
(define (distance x y)
 (sqrt (+ (sqr (- (posn-x x) (posn-x y)))  (sqr (-(posn-y x) (posn-y y))))))

; si-main
; starts game
; the inputted number is how many seconds a clock tick takes
(define (si-main x)
  (big-bang (make-sigs
             (make-posn (random WIDTH) 0)
             #false
             (make-tank TANK-START-POSN TANK-SPEED)
             '())
    [on-key si-control]
    [on-tick si-move x]
    [to-draw si-render.v2]
    [stop-when si-game-over? si-render-final]))

; SIGS.v2 -> Image 
; renders the given game state on top of BACKGROUND 
(define (si-render.v2 game)
  (local (
          ; Tank Image -> Image 
          (define (render-tank t im)
            (place-image TANK (tank-loc t) TANK-Y im))
          
          ; UFO Image -> Image 
          (define (render-ufo u im)
            (place-image UFO (posn-x u) (posn-y u) im))

          ; Charge Image -> Image    
          (define (charge-render chrg im)
            (cond
              [(boolean? chrg) (place-image empty-image 0 0 im)]
              [else (place-image CHARGE (posn-x chrg) (posn-y chrg) im)]))
          
          ; MissileOrNot Image -> Image 
          ; adds an image of missile m to scene s
          (define (missile-render.v2 lom im)
            (local (;Posn Image -> Image
                    (define (render-one one-miss base)
                      (place-image MISSILE (posn-x one-miss) (posn-y one-miss) base)))
              (foldr render-one im lom))))
          
    (render-tank (sigs-tank game)
    (render-ufo (sigs-ufo game)
    (missile-render.v2 (sigs-missiles game)
    (charge-render (sigs-charge game)
        SCREEN))))))

; si-render-final
; game over screen, same as render except with "game over" on the screen
(define (si-render-final game)
  (local (
          ; returns True if UFO has landed
          (define landed?
            (if (>= (posn-y (sigs-ufo game)) HEIGHT) #true #false))
          
          ; true if X has come within DETONATION PROXIMITY OF Y
          (define chrg-hit?
            (cond
              [(posn? (sigs-charge game))
               (<= (distance (sigs-charge game) (make-posn (tank-loc (sigs-tank game)) TANK-Y)) DETONATION-PROXIMITY)]
              [else #false]))
          
          ; true if a missle has hit the ufo
          (define ufo-hit?
            (local (
                    (define (any-hit? x)
                      (<= (distance (sigs-ufo game) x) DETONATION-PROXIMITY)))
              (ormap any-hit? (sigs-missiles game))))
          
          ; string string Image -> Image
          ; adds string to given image
          (define (render-text word color im)
            (place-image/align (text word TEXT-SIZE color) (/ HEIGHT 2) (/ HEIGHT 2) "center" "center" im)))
          
    (cond
     [(or landed?
          chrg-hit?)
          (render-text "GAME OVER" "red" (si-render.v2 game))]
     [ufo-hit?
      (render-text "YOU WIN" "green" (si-render.v2 game))])))

; GAME-OVER?
; SIGS -> Boolean
; function fed to the stop-when handler to determine if the program has met
; end-state criteria
; game is over if one of the following conditions is met:
; -UFO lands on earth
; -missile is within detonation proximity of UFO
(define (si-game-over? game)
  (local (
          ; returns True if UFO has landed
          (define landed?
            (if (>= (posn-y (sigs-ufo game)) HEIGHT) #true #false))
          
          ; true if X has come within DETONATION PROXIMITY OF Y
          (define chrg-hit?
            (cond
              [(posn? (sigs-charge game))
               (<= (distance (sigs-charge game) (make-posn (tank-loc (sigs-tank game)) TANK-Y)) DETONATION-PROXIMITY)]
              [else #false]))
          
          ; true if a missle has hit the ufo
          (define ufo-hit?
            (local (
                    (define (any-hit? x)
                      (<= (distance (sigs-ufo game) x) DETONATION-PROXIMITY)))
              (ormap any-hit? (sigs-missiles game)))))
    (or
     landed?
     ufo-hit?
     chrg-hit?)))

; si-move
; SIGS -> SIGS
; called on-tick; moves the missile and tank in straight lines at constant speeds.
; the UFO moves in straight line and makes small random jumps to the left or right
(define (si-move game)
  (local (
          ; generate-charge
          ; sigs-charge -> sigs-charge
          (define move-or-gen-chrg
            (cond
              [(or
                (boolean? (sigs-charge game))
                (>= (posn-y (sigs-charge game)) HEIGHT)
                (local (; checks if a missle has hit the ufo
                        (define chrg-hit-missile?
                          (local (
                                  (define (any-hit? x)
                                    (<= (distance (sigs-charge game) x) DETONATION-PROXIMITY)))
                            (ormap any-hit? (sigs-missiles game)))))
                  chrg-hit-missile?))
                (sigs-ufo game)]
              [else (make-posn (posn-x (sigs-charge game)) (+ (posn-y (sigs-charge game)) MISSILE-SPEED))]))
          
          ; moves ufo a given amount
          (define move-ufo
            (make-posn
             (local (
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
                         [(and (> (abs (- x w)) range) (< x w)(<= (- w range) UFO-WIDTH)) UFO-WIDTH])))
               (ufo-jump (random WIDTH) (posn-x (sigs-ufo game)) UFO-JUMP-RANGE))
             (+ UFO-SPEED (posn-y (sigs-ufo game)))))

          ; moves tank
          (define move-tank
            (cond
              [(or (>= (tank-loc (sigs-tank game))(- WIDTH TANK-WIDTH)) (<= (tank-loc (sigs-tank game)) TANK-WIDTH))
               (make-tank (- (tank-loc (sigs-tank game)) (tank-vel (sigs-tank game))) (* -1 (tank-vel (sigs-tank game))))]
              [else (make-tank (+ (tank-vel (sigs-tank game)) (tank-loc (sigs-tank game))) (tank-vel (sigs-tank game)))]))

          ; moves missile
          (define move-missiles
            (local (
                    (define (move-one-missile one)
                      (make-posn (posn-x one) (- (posn-y one) MISSILE-SPEED))))
              (map move-one-missile (sigs-missiles game)))))

 (make-sigs
       move-ufo
       move-or-gen-chrg
       move-tank
       move-missiles)))

; si-control
; SIGS KeyEvent -> SIGS
; reacts to "left" "right" and "space"
; "left" makes the tank move left
; "right" makes the tank move right
; "space" shoots the missle
(define (si-control game ke)
  (local (
          (define (chg-tank-direc cmp)
            (cond
              [(cmp (tank-vel (sigs-tank game)) 0) (* -1 (tank-vel (sigs-tank game)))]
              [else (tank-vel (sigs-tank game))]))

          (define (chg-game-tank-direct cmp)
            (make-sigs
              (sigs-ufo game)
              (sigs-charge game)
              (make-tank
                (tank-loc (sigs-tank game)) (chg-tank-direc cmp))
              (sigs-missiles game))))
    
      (cond
       [(equal? ke "left")
         (chg-game-tank-direct >)]
       [(equal? ke "right")
         (chg-game-tank-direct <)]
       [(equal? ke " ")
        (make-sigs
         (sigs-ufo game)
         (sigs-charge game)
         (sigs-tank game)
         (cons (make-posn (tank-loc (sigs-tank game)) (- HEIGHT TANK-HEIGHT)) (sigs-missiles game)))]
       [else game])))