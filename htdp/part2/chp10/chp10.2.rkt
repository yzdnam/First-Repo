;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chp10.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct work [employee employee# rate hours])
; A (piece of) Work is a structure: 
;   (make-work String Number Number Number)
; interpretation (make-work n id r h) combines the name 
; with the employee number id, pay rate r and the number of hours h

; Low (short for list of works) is one of: 
; – '()
; – (cons Work Low)
; interpretation an instance of Low represents the 
; hours worked for a number of employees

; Low -> List-of-numbers
; computes the weekly wages for the given records

(check-expect
  (wage*.v2 (cons (make-work "Robby" 1 11.95 39) '()))
  (cons (* 11.95 39) '()))
 
(define (wage*.v2 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low) (cons (wage.v2 (first an-low))
                          (wage*.v2 (rest an-low)))]))
 
; Work -> Number
; computes the wage for the given work record w
(define (wage.v2 w)
  (* (work-rate w) (work-hours w)))

(define-struct paycheck [employee employee# amount])

; Low -> List-of-paychecks
;consumes a list of work records and computes a list of
;paychecks from it, one per record

(check-expect
  (wage*.v3 (cons (make-work "Robby" 1 11.95 39) '()))
  (cons (make-paycheck "Robby" 1 (* 11.95 39)) '()))
 
(define (wage*.v3 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low) (cons (make-paycheck (work-employee (first an-low)) (work-employee# (first an-low)) (wage.v2 (first an-low)))
                          (wage*.v3 (rest an-low)))]))

; EX 167
; list-of-posns -> number
; consumes a list of posns and produces the sum of all of its x-coordinates

(check-expect (sum (cons (make-posn 2 2) (cons (make-posn 3 4) (cons (make-posn 1 2) '())))) 6)

(define (sum lops)
  (cond
    [(empty? lops) 0]
    [else (+ (posn-x (first lops)) (sum (rest lops)))]))
                   
; EX 168
; list-of-posns -> list-of-posns
; adds 1 to each posn's y-coordinate in the given list of posns

(check-expect (translate (cons (make-posn 1 1) (cons (make-posn 2 3) '()))) (cons (make-posn 1 2) (cons (make-posn 2 4) '())))

(define (translate lops)
  (cond
    [(empty? lops) '()]
    [else (cons (make-posn (posn-x (first lops)) (+ 1 (posn-y (first lops)))) (translate (rest lops)))]))

; EX 169
; list-of-posns -> list-of-posns
; results contains posns whose x-coordinates are between 0 and 100
; and whose y-coordinates are between 0 and 200

(check-expect (legal (cons (make-posn 101 10) (cons (make-posn 90 90) (cons (make-posn 98 201) '()))))
              (cons (make-posn 90 90) '()))

(define (legal lops)
  (cond
    [(empty? lops) '()]
    [else (cond
            [(and (and (< 0 (posn-x (first lops))) (> 100 (posn-x (first lops))))
                  (and (< 0 (posn-y (first lops))) (> 200 (posn-y (first lops)))))
             (cons (first lops) (legal (rest lops)))]
            [else (legal (rest lops))])]))

; EX 170
(define-struct phone [area switch four])
; A Phone is a structure: 
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999. 
; A Four is a Number between 1000 and 9999.

; list-of-phones -> list-of-phones
; replaces all occurrences of area code 713 with 281

(check-expect (replace (cons (make-phone 713 111 1111) (cons (make-phone 111 111 1111) '())))
              (cons (make-phone 281 111 1111) (cons (make-phone 111 111 1111) '())))

(define (replace lops)
  (cond
    [(empty? lops) '()]
    [else (cond
            [(equal? (phone-area (first lops)) 713)
             (cons (make-phone 281 (phone-switch (first lops)) (phone-four (first lops))) (replace (rest lops)))]
            [else (cons (first lops) (replace (rest lops)))])]))