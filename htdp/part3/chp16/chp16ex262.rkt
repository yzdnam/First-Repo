;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chp16ex262) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; EX 262
; Number -> list-of-lists-of-numbers
; creates a diagonal square of 0s and 1s that is the given number of rows in height
(define (identityM n)
  (local (
          ; Number Number -> List
          ; creates a list of zeroes the first number given in length with the number
          ; one placed at the position specified by the second given number
          (define (create-row one-place)
            (build-list n (lambda (spot) (cond
                                           [(equal? (- one-place 1) spot) 1]
                                           [else 0])))))
          
    (map create-row (build-list n (lambda (n) (+ 1 n))))))

(check-expect (identityM 1) (list (list 1)))
(check-expect (identityM 3) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

; EX 306
; Number -> list-of-lists-of-numbers
; creates a diagonal square of 0s and 1s that is the given number of rows in height
(define (identityM-loop n)
  (local (
          ; Number Number -> List
          ; creates a list of zeroes the first number given in length with the number
          ; one placed at the position specified by the second given number
          (define (create-row one-place)
            (for/list ([i n]) (cond
                                [(equal? (- one-place 1) i) 1]
                                [else 0]))))
    (for/list ([i n])
      (create-row (+ 1 i)))))

(check-expect (identityM-loop 1) (list (list 1)))
(check-expect (identityM-loop 3) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))