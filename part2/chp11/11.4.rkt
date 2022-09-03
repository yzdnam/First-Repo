;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |11.4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)

; a Polygon is also one of: 
; – (cons Posn (cons Posn (cons Posn '()))) 
; – (cons Posn Polygon)

; a plain background image 
(define MT (empty-scene 50 50))
 
; Image Polygon -> Image
; renders the given polygon p into img
(define (render-poly img p)
  (connect-dots img p (first p)))

; EX 193 alternate render-poly 1
;(define (render-poly-alt img p)
;  (connect-dots img (cons (last p) p)))

; EX 193 alternate render-poly 2
;(define (render-poly-alt2 img p)
;  (connect-dots img (add-at-end p)))

; polygon -> polygon
; copies the first posn in a polygon and adds it to the
; end of the polygon
(define (add-at-end p)
  (append p (list (first p))))

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

(check-expect
  (render-poly MT triangle-p)
  (scene+line
    (scene+line
      (scene+line MT 20 10 20 20 "red")
      20 20 30 20 "red")
    30 20 20 10 "red"))

(check-expect
  (render-poly MT square-p)
  (scene+line
    (scene+line
      (scene+line
        (scene+line MT 10 10 20 10 "red")
        20 10 20 20 "red")
      20 20 10 20 "red")
    10 20 10 10 "red"))

; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im
(define (render-line img p q)
  (scene+line
   img
   (posn-x p) (posn-y p) (posn-x q) (posn-y q)
   "red"))

(check-expect (render-line MT (make-posn 10 10) (make-posn 20 20)) (scene+line MT 10 10 20 20 "red"))

; An NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)

; Image NELoP -> Image 
; connects the dots in p by rendering lines in img
; and terminating the line at point q
(define (connect-dots.v1 img p)
  (cond
    [(empty? (rest p)) img]
    [else (render-line (connect-dots.v1 img (rest p))
                       (first p)
                       (second p))]))

(define (connect-dots img p q)
  (cond
    [(empty? (rest p)) (render-line
                        MT
                        (first p)
                        q)]
    [else
     (render-line
       (connect-dots img (rest p) q)
       (first p)
       (second p))]))

(check-expect (connect-dots.v1 MT triangle-p)
              (scene+line
               (scene+line MT 20 20 30 20 "red")
               20 10 20 20 "red"))

; EX 191
; second example of render-poly adapted to connect-dots
(check-expect (connect-dots.v1 MT square-p)
              (scene+line
               (scene+line
                (scene+line MT 20 20 10 20 "red")
                20 10 20 20 "red")
               10 10 20 10 "red"))
                            
; NELoP -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))


    