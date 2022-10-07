;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chp9.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; N String -> List-of-strings 
; creates a list of n copies of s
 
(check-expect (copier 0 "hello") '())
(check-expect (copier 2 "hello")
              (cons "hello" (cons "hello" '())))
 
(define (copier n s)
  (cond
    [(zero? n) '()]
    [(positive? n) (cons s (copier (sub1 n) s))]))

(define (copier.v2 n s)
  (cond
    [(zero? n) '()]
    [else (cons s (copier.v2 (sub1 n) s))]))

; EX 150
; N -> Number
; computes (+ n pi) without using +
 
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
 
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))

; Number Number -> Number
; computes (+ n x) without using +
 
(check-expect (add 3 5) 8)
(check-expect (add 0 2) 2)
(check-expect (add 2 0) 2)
 
(define (add n x)
  (cond
    [(zero? n) x]
    [else (add1 (add (sub1 n) x))]))

; EX 151
; Number Number -> Number
; computes (* n x) without using *

(check-expect (multiply 3 5) 15)
(check-expect (multiply 0 2) 0)
(check-expect (multiply 2 0) 0)

(define (multiply n x)
  (cond
    [(and (> n 1) (> x 1)) (add n (multiply n (sub1 x)))]
    [(= n 1) x]
    [(= x 1) n]
    [(or (zero? n) (zero? x)) 0]))

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

; EX 153
; Number Number -> image
; consumes two numbers, overlays a grid of squares over an
; empty scene both with dimensions corresponding to the inputs.
(check-expect (create-hall 2 2) (overlay (row 2 (col 2 GRID)) (empty-scene 4 4)))

(define (create-hall x y)
  (overlay (row x (col y GRID)) (empty-scene (* x y) (* y x))))

(define BALLOON (circle 2 "solid" "red"))
(define HALL (create-hall 10 10))

; list-of-posns -> image
; produces an image of a 10x10 grid with red dots added
; at positions on the grid specified by the input
(check-expect (add-balloons (cons (make-posn 2 2) (cons (make-posn 3 4)'())))
              (place-image BALLOON 2 2 (place-image BALLOON 3 4 HALL)))

(define (add-balloons lops)
  (cond
    [(empty? lops) HALL]
    [else (add-balloon (first lops) (add-balloons (rest lops)))]))

; posn -> image
; adds a red dot to a given grid based on a given posn. returns the grid.
(define (add-balloon inp img)
  (place-image BALLOON (posn-x inp) (posn-y inp) img))

(define-struct layer [color doll])

; An RD (short for Russian doll) is one of: 
; – String 
; – (make-layer String RD)

; RD -> Number
; how many dolls are part of an-rd
(define (depth an-rd)
  (cond
    [(string? an-rd) 1]
    [else (+ (depth (layer-doll an-rd)) 1)]))

(check-expect (depth "red") 1)
(check-expect
  (depth
   (make-layer "yellow" (make-layer "green" "red")))
  3)

; EX 154
; RD -> string
; consumes a russian doll and produces a string of all the colors,
; separated by a comma, and a space
(check-expect (colors (make-layer "yellow" (make-layer "green" "red")))
              "yellow, green, red")

(define (colors an-rd)
  (cond
    [(string? an-rd) an-rd]
    [else (string-append (string-append (layer-color an-rd) ", ") (colors (layer-doll an-rd)))]))

; EX 155
; RD -> string
; consumes an RD and produces the color of the innermost doll
(check-expect (inner (make-layer "yellow" (make-layer "green" "red")))
              "red")

(define (inner an-rd)
  (cond
    [(string? an-rd) an-rd]
    [else (inner (layer-doll an-rd))]))                                                                                                                  