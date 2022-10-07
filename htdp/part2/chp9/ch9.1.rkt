;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch9.1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; EX 138
; A List-of-amounts is one of: 
; – '()
; – (cons PositiveNumber List-of-amounts)

; List-of-amounts -> Number
; determines the sum of the amounts in a
; given List-of-amounts
(define (sum alist)
  (cond
    [(empty? alist) 0]
    [else (+ (sum (rest alist)) (first alist))]))

(check-expect (sum (cons 1 (cons 2 '()))) 3)
(check-expect (sum '()) 0)
(check-expect (sum (cons 5 '())) 5)

; EX 139
; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)

; List-of-numbers -> Boolean
; determines whether a list-of-numbers is a
; list-of-amounts. Returns true if the input
; is a list-of-amounts
(define (pos? alist)
  (cond
    [(empty? alist) #true]
    [else (cond
            [(> 0 (first alist)) #false]
            [else (pos? (rest alist))])]))

(check-expect (pos? (cons 1 (cons 2 '()))) #true)
(check-expect (pos? '()) #true)
(check-expect (pos? (cons -1 (cons 2 '()))) #false)
(check-expect (pos? (cons 2 (cons -1 '()))) #false)

; List-of-numbers -> Number
; determines the sum of the amounts in a given
; List-of-numbers. Signals an error if given list
; is not also a List-of-amounts
(define (checked-sum alist)
  (cond
    [(empty? alist) 0]
    [else (cond
            [(pos? alist)
                 (+ (sum (rest alist)) (first alist))]
            [else (error alist " is not a List-of-amounts")])]))

(check-expect (checked-sum (cons 1 (cons 2 '()))) 3)
(check-expect (checked-sum '()) 0)
(check-error (checked-sum (cons 1 (cons -2 '()))) "(cons 1 (cons -2 '())) is not a List-of-amounts")
(check-error (checked-sum (cons -2 (cons 2 '()))) "(cons -2 (cons 2 '())) is not a List-of-amounts")
                 
; EX 140
; List-of-Booleans -> Boolean
; consumes a list of Booleans. returns #true if all elements of the list
; are true. otherwise, it returns #false
(define (all-true alist)
  (cond
    [(empty? alist) #true]
    [else (if (first alist) (all-true (rest alist)) #false)]))

(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true (cons #false '())) #false)
(check-expect (all-true (cons #true (cons #false '()))) #false)

; EX 141
; List-of-string -> String
; concatenates all strings in l into one long string
 
(check-expect (cat '()) "")
(check-expect (cat (cons "a" (cons "b" '()))) "ab")
(check-expect
  (cat (cons "ab" (cons "cd" (cons "ef" '()))))
  "abcdef")
 
(define (cat l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) (cat (rest l)))]))

; EX 142
; list-of-images positive-number -> ImageOrFalse
; Consumes a list of images loi and a positive number n. It produces
; the first image on loi that is not an n by n square;
; if it cannot find such an image, it produces #false.
; ImageOrFalse is either an image or #false.
(define (ill-sized? loi n)
  (cond
    [(empty? loi) #false]
    [else (cond
            [(or (not (equal? (image-height (first loi)) n))
                 (not (equal? (image-width (first loi)) n)))
             (first loi)]
            [else (ill-sized? (rest loi) n)])]))

(check-expect (ill-sized? (cons (square 40 "solid" "black") (cons (square 30 "solid" "black") '())) 40)
       (square 30 "solid" "black"))