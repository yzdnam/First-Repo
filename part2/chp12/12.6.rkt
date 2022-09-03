;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |12.6|) (read-case-sensitive #t) (teachpacks ((lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "itunes.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 10) ; # of blocks, horizontally 
(define SIZE 10) ; blocks are squares
(define HEIGHT 10)
(define SCENE-SIZE (* WIDTH SIZE))
(define SCENE-HEIGHT (* HEIGHT SIZE))
(define SCENE (empty-scene SCENE-SIZE SCENE-HEIGHT))
(define FLOOR (- SCENE-HEIGHT (/ SIZE 2)))
 
 
(define BLOCK ; red squares with black rims
  (overlay
    (square (- SIZE 1) "solid" "red")
    (square SIZE "outline" "black")))

(define-struct tetris [block landscape])
(define-struct block [x y])
 
; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of: 
; – '() 
; – (cons Block Landscape)
; A Block is a structure:
;   (make-block N N)
 
; interpretations
; (make-block x y) depicts a block whose left 
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting

(define landscape0 (list))
(define block-dropping (list (make-block 4.5 4.5)))
(define tetris0 (make-tetris (make-block 5.5 5.5) (list)))
(define block-landed (make-block 0.5 (- HEIGHT 0.5)))
(define block-on-block (make-block 0.5 (- HEIGHT 1.5)))
(define tetris-scene (make-tetris (make-block 5.5 5.5) (list block-landed block-on-block)))

; number -> tetris
(define (tetris-main x)
  (big-bang (make-tetris (make-block (+ (random (- WIDTH 1)) 0.5) 0.5) (list))
    [to-draw render]
    [on-tick drop x]
    [on-key handle-key]
    [stop-when done? render-end]
    ))

; block -> block 
; creates a random block different from the one used as an input
(define (block-create p)
  (block-check-create
     p (make-block (+ (random (- WIDTH 1)) 0.5) 0.5)))
 
; Posn Posn -> Posn 
; generative recursion 
; uses the input to block-create to generate a different and random block
(define (block-check-create p candidate)
  (if (equal? (block-x p) (block-x candidate)) (block-create p) candidate))

; tetris -> image
(define (render-end game)
  (place-image (text "GAME OVER!" 15 "green") (/ SCENE-SIZE 2) (/ SCENE-HEIGHT 2) (render game)))

; tetris -> image
(define (render game)
  (place-image BLOCK (* SIZE (block-x (tetris-block game))) (* SIZE (block-y (tetris-block game))) (render-landscape (tetris-landscape game))))

; tetris-landscape -> image
(define (render-landscape lscape)
  (cond
    [(empty? lscape) SCENE]
    [else
     (place-image BLOCK (* SIZE (block-x (first lscape))) (* SIZE (block-y (first lscape))) (render-landscape (rest lscape)))]))

; tetris -> tetris
; moves blocks down by the size of a block every clock-tick
; generates a new block when a block comes to a rest on the bottom of the scene
(define (drop game)
  (cond
    [(or (check-land-other (tetris-block game) (tetris-landscape game))
         (not (< (* SIZE (block-y (tetris-block game))) FLOOR)))
     (make-tetris (block-create (tetris-block game)) (cons (tetris-block game) (tetris-landscape game)))]
    [else (make-tetris (make-block (block-x (tetris-block game)) (+ (block-y (tetris-block game)) 1)) (tetris-landscape game))]))

; block landscape -> boolean
; returns true if a block has landed on another block
(define (check-land-other blk lscape)
  (cond
    [(empty? lscape) #false]
    [else (cond
            [(equal? (block-x blk) (block-x (first lscape))) (cond
                                                               [(equal? (- (block-y (first lscape)) 1) (block-y blk)) #true]
                                                               [else (check-land-other blk (rest lscape))])]
            [else (check-land-other blk (rest lscape))])]))

(check-expect (check-land-other (make-block 5.5 2.5) (list (make-block 5.5 3.5))) #true)

; tetris key -> tetris
; key handler
(define (handle-key game key)
  (cond
    [(equal? key "left") (cond
                           [(or (member? (tetris-block (move-block-left game)) (tetris-landscape game))
                                (< (block-x (tetris-block (move-block-left game))) 0))
                            game]
                           [else (move-block-left game)])]
    [(equal? key "right") (cond
                           [(or (member? (tetris-block (move-block-right game)) (tetris-landscape game))
                                (> (block-x (tetris-block (move-block-right game))) WIDTH))
                            game]
                           [else (move-block-right game)])]
    [else game]))

; tetris -> tetris
; moves block left
(define (move-block-left game)
  (make-tetris (make-block (- (block-x (tetris-block game)) 1) (block-y (tetris-block game))) (tetris-landscape game)))

; tetris -> tetris
; moves block right
(define (move-block-right game)
  (make-tetris (make-block (+ (block-x (tetris-block game)) 1) (block-y (tetris-block game))) (tetris-landscape game)))

; tetris -> boolean
; returns true if the landscape has reached the top of the canvas
; otherwise, it returns false
(define (done? game)
  (cond
    [(empty? (tetris-landscape game)) #false]
    [(< (block-y (first (tetris-landscape game))) 1) #true]
    [else #false]))