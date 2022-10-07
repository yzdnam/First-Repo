;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname chp16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; EX 257
; constructs a list by applying f to the numbers between 0 and (- n 1)
(define (build-l*st f n)
  (cond
    [(equal? n 0) '()]
    [else (add-at-end (f (sub1 n)) (build-l*st f (sub1 n)))]))

(define (add-at-end n l)
  (reverse (cons n (reverse l))))


; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im
(define (render-line img p1 p2)
  (scene+line img (posn-x p1) (posn-y p1) (posn-x p2) (posn-y p2) "red"))

; EX 258
; Image Polygon -> Image 
; adds a corner of p to img
(define (render-poly img p)
 
(local ( 
        ; NELoP -> Image
        ; connects the Posns in p in an image
        (define (connect-dots p)
          (cond
            [(empty? (rest p)) img]
            [else (render-line (connect-dots (rest p))
                               (first p)
                               (second p))]))
        ; Polygon -> Posn
        ; extracts the last item from p
        (define last
          (cond
            [(empty? (rest (rest (rest p)))) (third p)]
            [else (last (rest p))])))
  
  (render-line (connect-dots p) (first p) last)))
  
(define triangle-p
  (list
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 30 20)))
	
(define square-p
  (list
    (make-posn 10 10)
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 10 20)))

; a plain background image 
(define MT (empty-scene 50 50))

(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)

; EX 261
; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (cond
       [(<= (IR-price (first an-inv)) 1.0)
        (cons (first an-inv) (extract1 (rest an-inv)))]
       [else (extract1 (rest an-inv))])]))

(define (extract1* an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
       (local ((define extract1-from-rest (extract1* (rest an-inv))))
     (cond
       [(<= (IR-price (first an-inv)) 1.0)
        (cons (first an-inv) (extract1 (rest an-inv)))]
       [else (extract1 (rest an-inv))]))]))

(define SAMPLE-INV (list (make-IR "a" 20) (make-IR "b" 0.99) (make-IR "c" 12) (make-IR "d" 0.85)))