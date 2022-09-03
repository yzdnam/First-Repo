;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chp9.6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; List-of-string String -> N
; determines how often s occurs in los
(define (count los s)
  (cond
    [(empty? los) 0]
    [else (cond
            [(equal? (first los) s) (+ 1 (count (rest los) s))]
            [else (count (rest los) s)])]))

(check-expect (count (cons "f" (cons "f" (cons "u" (cons "f" '())))) "f") 3)
(check-expect (count (cons "f" (cons "f" (cons "u" (cons "f" '())))) "u") 1)

; A Son.L is one of: 
; – empty 
; – (cons Number Son.L)
; 
; Son is used when it 
; applies to Son.L and Son.R

; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
; 
; Constraint If s is a Son.R, 
; no number occurs twice in s

(define es '())

; Number Son.L -> Son.L
; removes x from s 
(define s1.L
  (cons 1 (cons 1 '())))
 
(check-expect
  (set-.L 1 s1.L) es)
 
(define (set-.L x s)
  (remove-all x s))

; Number Son.R -> Son.R
; removes x from s
(define s1.R
  (cons 1 '()))
 
(check-expect
  (set-.R 1 s1.R) es)
 
(define (set-.R x s)
  (remove x s))

; Number Son.L -> Son.L
; adds x to s 
 
(check-expect
  (set+.L 1 s1.L) (cons 1 (cons 1 (cons 1 '()))))
 
(define (set+.L x s)
  (cons x s))

; Number Son.R -> Son.R
; if x does not already exist in Son.R, adds
; x to Son.R. If x already exists in Son.R.
; returns Son.R unmodified
 
(check-expect
  (set+.R 1 s1.R) s1.R)
 
(define (set+.R x s)
  (cond
    [(member? x s) s]
    [else (cons x s)]))