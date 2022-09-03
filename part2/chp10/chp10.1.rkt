;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chp10.1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define RATE 12)

(check-expect (wage* (cons 1 (cons 2 (cons 3 (cons 4 '()))))) (cons 12 (cons 24 (cons 36 (cons 48 '())))))
(check-error (wage* (cons 10 (cons 20 (cons 101 '())))) "Hours for an employee exceeds 100. Check input")

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [else (cond
            [(<= (first whrs) 100) (cons (wage (first whrs)) (wage* (rest whrs)))]
            [else (error "Hours for an employee exceeds 100. Check input")])]))
 
; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* RATE h))

; EX 163
; list -> list
; converts a list of measurements in Fahrenheit
; to a list of Celcius measurements

(check-within (convertFC (cons 32 (cons 100 '()))) (cons 0 (cons 37.8 '())) 0.1)
                                   
(define (convertFC lof)
  (cond
    [(empty? lof) '()]
    [else (cons (convert1FC (first lof)) (convertFC (rest lof)))]))

; number -> number
; converts a farenheit measurement to a celsius
; measurement
(define (convert1FC Fmeasure)
  (* (/ 5 9) (- Fmeasure 32)))

; EX 164
; list -> list
; convert a list of US$ amounts into a list of euro amounts
; using a given exchange rate

(check-expect (convert-euro* 0.1 (cons 10 (cons 9 '()))) (cons 1 (cons 0.9 '())))

(define (convert-euro* rate lods)
  (cond
    [(empty? lods) '()]
    [else (cons (* rate (first lods)) (convert-euro* rate (rest lods)))]))

; EX 165
; list-of-strings -> list-of-strings
; consumes two strings, called old and new, and a list of strings
; produces a new list of strings by substituting all occurrences of old with new.

(check-expect (substitute "fart" "shit" (cons "poop" (cons "hole" (cons "shit" '())))) (cons "poop" (cons "hole" (cons "fart" '()))))

(define (substitute new old los)
  (cond
    [(empty? los) '()]
    [else (cond
            [(equal? (first los) old) (cons new (substitute new old (rest los)))]
            [else (cons (first los) (substitute new old (rest los)))])]))