;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |15.1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; Number -> [List-of Number]
; tabulates the given function between n and 0
; (incl.) in a list
(define (tabulate n func)
  (cond
    [(= n 0) (list (func 0))]
    [else
     (cons
      (func n)
      (tabulate (sub1 n) func))]))

(define (abstr-tab-sin n)
  (tabulate n sin))

(define (abstr-tab-sqrt n)
  (tabulate n sqrt))

; [List-of Number] -> Number
;computes the cumulative given primative operation on the numbers
; in a given list
(define (fold1 l op)
  (cond
    [(empty? l) 0]
    [else
     (op (first l)
         (fold1 (rest l) op))]))

(define (fold2 l base op)
  (cond
    [(empty? l) base]
    [else
     (op (first l)
         (fold2 (rest l) base op))]))

; [List-of Number] -> Number
(define (fold2-product l)
  (fold2 l 1 *))

; [List-of Posn] -> Image
(define (fold2-image* l)
  (fold2 l emt place-dot))

; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))

; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))