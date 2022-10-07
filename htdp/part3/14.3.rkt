;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |14.3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define (f x) (x 10))

; function function -> boolean
; given two functions from numbers to numbers, the function
; determines whether the two produce the same results for 1.2, 3,
; and -5.775
(define (function=at-1.2-3-and-5.775 f1 f2)
  (cond
    [(and
      (equal? (f1 1.2) (f2 1.2))
      (equal? (f1 3) (f2 3))
      (equal? (f1 -5.775) (f2 -5.775)))
     #true]
    [else #false]))

(define (func1 x)
  1)

(define (func2 x)
  1)

(define (func3 x)
  (sqr x))

(check-expect (function=at-1.2-3-and-5.775 func1 func2) #true)
(check-expect (function=at-1.2-3-and-5.775 func1 func3) #false)