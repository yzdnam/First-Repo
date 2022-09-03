;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define width 20)

(for/list ([width 10]
           [height width])
  (list width height))

;(for*/list ([width 10]
;            [height width])
;  (list width height))

; list -> list-of pairs
; produces a list of the same items in the given list paired with their relative
; indexes
(define (enumerate lot)
  (local (
          (define index-list (build-list (length lot) (lambda (i) i))))
    (map list lot index-list)))

(for*/list ([i 2] [j '(a b)])
  (list i j))

; list list -> list-of pairs
; produces pairs of all items from the two given lists
(define (cross l1 l2)
  (local (; X X -> list-of pair
          (define (create-pair x y)
            (list x y) ))
    (foldr append '() (map (lambda (l1i) (map (lambda (l2i) (create-pair l1i l2i)) l2)) l1))))

(check-expect (cross (list 1 2) (list 3 4)) (list (list 1 3) (list 1 4) (list 2 3) (list 2 4)))