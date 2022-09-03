;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |17.2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; EX 267
; [List-of Number] -> [List-of Number]
; converts a list of US$ amoutns to euro amoutns based on a historical exchange rate
;(define (convert-euro loa)
;  (local (; Number -> Number
;          (define (convert-single-amt dollar)
;            (* dollar EXCHANGE-RATE)))
;    (map convert-single-amt loa)))

(define (convert-euro loa)
  (map (lambda (dollar-amount) (* dollar-amount EXCHANGE-RATE)) loa))

(define EXCHANGE-RATE 1.06)

(check-expect (convert-euro (list 1.10 1.00 0.97)) (list (* 1.10 1.06) (* 1.00 1.06) (* 0.97 1.06)))

; converts a list of fahrenheit measurements to a list celsius measurements
;(define (convertFC lot)
;  (local (; Number -> Number
;          (define (convert-single-tmp fahr)
;            (/ (* (- fahr 32) 5) 9)))
;    (map convert-single-tmp lot)))

(define (convertFC lot)
  (map (lambda (fahr-measurement) (/ (* (- fahr-measurement 32) 5) 9)) lot))

(check-expect (convertFC (list 32)) (list 0))

; [List-of Posn] -> [List-of [List-of Number]]
; converts a list of posns into a list of lists of pairs of numbers
;(define (translate lop)
;  (local (;Posn -> [List-of Number]
;          ;convert a posn into a list of 2 numbers
;          (define (convert-posn pos)
;            (list (posn-x pos) (posn-y pos))))
;    (map convert-posn lop)))

(define (translate lop)
  (map (lambda (pos) (list (posn-x pos) (posn-y pos))) lop))

(check-expect (translate (list (make-posn 1 1) (make-posn 2 2))) (list (list 1 1) (list 2 2)))

; EX 268
(define-struct ir [name descrip acq-price sales-price])
; [List-of ir] -> [List-of ir]
; sorts a list of inventory records by the difference between each of their acquisition and sales prices
;(define (sort-by-profit loi)
;  (local (; ir -> number
;          ; determines an ir's potential profit
;          (define (profit i)
;            (- (ir-sales-price i) (ir-acq-price i)))
          
          ; ir ir -> boolean
          ; determines whether the first ir has a greater profit margin than the second ir
;          (define (profit>? i1 i2)
;            (> (profit i1) (profit i2))))
;    (sort loi profit>?)))

(define (sort-by-profit loi)
  (sort loi (lambda (ir1 ir2) (> (- (ir-sales-price ir1) (ir-acq-price ir1)) (- (ir-sales-price ir2) (ir-acq-price ir2))))))

(check-expect (sort-by-profit (list (make-ir "second" "b" 10 22) (make-ir "first" "c" 10 30) (make-ir "third" "d" 10 12)))
              (list (make-ir "first" "c" 10 30) (make-ir "second" "b" 10 22) (make-ir "third" "d" 10 12)))

; EX 269
; number [list-of ir] -> [list-of ir]
; produces a list of ir whose sales price is below the given number
;(define (eliminate-expensive ua loi)
;  (local (; ir -> boolean
;          (define (check-ir i)
;            (< (ir-sales-price i) ua)))
;    (filter check-ir loi)))

(define (eliminate-expensive ua loi)
  (filter (lambda (inven-rec) (< (ir-sales-price inven-rec) ua)) loi))

(check-expect (eliminate-expensive 20 (list (make-ir "second" "b" 10 22) (make-ir "first" "c" 10 30) (make-ir "third" "d" 10 12)))
              (list (make-ir "third" "d" 10 12)))

; string [list-of ir] -> [list-of ir]
; produces a list of ir that do not use the given string as a name
;(define (recall ty loi)
;  (local (; ir -> boolean
;          (define (not-named-ty i)
;            (not (equal? (ir-name i) ty))))
;    (filter not-named-ty loi)))

(define (recall ty loi)
  (filter (lambda (toy) (not (equal? (ir-name toy) ty))) loi))

(check-expect (recall "second" (list (make-ir "second" "b" 10 22) (make-ir "first" "c" 10 30) (make-ir "third" "d" 10 12)))
              (list (make-ir "first" "c" 10 30) (make-ir "third" "d" 10 12)))

; [list-of string] [list-of string] -> [list-of string]
; returns a list of strings that are on both of the given lists
;(define (selection l1 l2)
;  (local (; string -> boolean
;          (define (in-list? str)
;            (member? str l1)))
;    (filter in-list? l2)))

(define (selection l1 l2)
  (filter (lambda (string-in-1) (member? string-in-1 l1)) l2))

(check-expect (selection (list 1 2 3 4 5) (list 3 5 6 9 0)) (list 3 5))

; EX 270
; creates the list (list 0 ... (- n 1)) for any natural number n
;(define (up-to n)
; (local (
;         (define (return-self n) n))
;   (build-list n return-self)))

(define (up-to n)
  (build-list n (lambda (n) n)))

; create list (list 1 ... n) for any natural number n
;(define (one-to n)
;  (build-list n add1))
(define (one-to n)
  (build-list n (lambda (n) (+ 1 n))))

; create list (list 1 1/2 ... 1/n) for any natural number n
;(define (one-over-list n)
;  (local (
;          (define (f x) (/ 1 (add1 x))))
;    (build-list n f)))
(define (one-over-list n)
  (build-list n (lambda (x) (/ 1 (add1 x)))))

; create the list of the first n even numbers
;(define (first-n-evens n)
;  (local (
;          (define (times2 x)
;            (+ (* 2 x) 2)))
;    (build-list n times2)))
(define (first-n-evens n)
  (build-list n (lambda (x) (+ (* 2 x) 2))))

; tabulates a function between n and 0 (incl.) in a list
(define (tabulate func n)
  (build-list n (lambda (n) (func n))))

; EX 271
; string [list-of string] -> boolean
;(define (find-name name lon)
;  (local (
;          (define (equal-or-extension? other-name)
;            (or (equal? name other-name)
;                (string-contains? name other-name))))
;    (ormap equal-or-extension? lon)))
(define (find-name name lon)
  (ormap (lambda (name-from-list)
           (or (equal? name name-from-list)
               (string-contains? name name-from-list))) lon))

(check-expect (find-name "lou" (list "fart" "foo" "bang")) #false)
(check-expect (find-name "lou" (list "fart" "foo" "louis")) #true)

;(define (all-start-with-a? lon)
;  (local (
;          (define (starts-with-a? name)
;            (equal? "a" (first (explode name)))))
;    (andmap starts-with-a? lon)))
(define (all-start-with-a? lon)
  (andmap (lambda (name) (equal? "a" (first (explode name)))) lon))                          

(check-expect (all-start-with-a? (list "alfred" "allman" "al" "alouis")) #true)
(check-expect (all-start-with-a? (list "alfred" "allman" "al" "louis")) #false)

; EX 272
;(define (append-from-foldr l1 l2)
;  (local (
;          (define (add-to-end item l)
;            (reverse (cons item (reverse l)))))
;    (foldr add-to-end l1 (reverse l2))))
(define (append-from-foldr l1 l2)
  (foldr (lambda (l1 l2) (reverse (cons l1 (reverse l2)))) l1 (reverse l2)))

(check-expect (append-from-foldr (list 1 2) (list 3 4)) (list 1 2 3 4))

(define (append-from-foldl l1 l2)
  (local (
          (define (add-to-end item l)
            (reverse (cons item (reverse l)))))
    (foldl add-to-end l1 l2)))

(check-expect (append-from-foldl (list 1 2) (list 3 4)) (list 1 2 3 4))

(define (sum-list l1)
  (foldr (lambda (x y) (+ x y)) 0 l1))

(check-expect (sum-list (list 1 2 3)) 6)

(define (cum-prod-list l1)
  (foldr * 1 l1))

(check-expect (cum-prod-list (list 2 3 4 5)) 120)

(define (horiz-compo-imgs loi)
  (foldr beside empty-image loi))

(define dot (square 10 "solid" "red"))

(define (vert-compo-imgs loi)
  (foldr above empty-image loi))

; EX 273
;(define (map-from-fold func lis)
;  (local (
;          (define (add-back item l)
;            (cons (func item) l)))
;    (foldr add-back '() lis)))
(define (map-from-fold func lis)
  (foldr (lambda (item lis) (cons (func item) lis)) '() lis))

(define (test-func x)
  (* 11 x))

(check-expect (map-from-fold test-func (list 1 2 3 4)) (list 11 22 33 44))


; EX 274
; PREFIXES AND SUFFIXES W/ ABSTRACTION
; list-of-1strings -> list-of-lists-of-1strings
; produces a list of lists of all prefixes to the given list
(define (prefixes lo1)
  (cond
    [(empty? lo1) lo1]
    [else (cons lo1 (prefixes (reverse (rest (reverse lo1)))))]))

(check-expect (prefixes-abs (list "a" "b" "c")) (list (list "a" "b" "c") (list "a" "b") (list "a")))
(check-expect (prefixes-abs (list "a" "b" "c" "d")) (list (list "a" "b" "c" "d") (list "a" "b" "c") (list "a" "b") (list "a") ))
(check-expect (prefixes-abs (list "a" "b" "c" "d" "e")) (list (list "a" "b" "c" "d" "e") (list "a" "b" "c" "d") (list "a" "b" "c") (list "a" "b") (list "a") ))

(define (prefixes-abs lo1)
  (local (
          (define (create-prefix ls n)
            (cond
              [(equal? (length ls) (+ n 1)) ls]
              [else (create-prefix (reverse (rest (reverse ls))) n)]))
          (define (given-prefixes n)
            (create-prefix lo1 n)))
    (reverse (build-list (length lo1) given-prefixes))))

; list-of-1strings -> list-of-lists-of-1strings
; produces a list of lists of all suffixes to the given list
(define (suffixes lo1)
  (cond
    [(empty? lo1) lo1]
    [else (cons lo1 (suffixes (rest lo1)))]))

(check-expect (suffixes-abs (list "a" "b" "c")) (list (list "a" "b" "c") (list "b" "c") (list "c") ))
(check-expect (suffixes-abs (list "a" "b" "c" "d")) (list (list "a" "b" "c" "d") (list "b" "c" "d") (list "c" "d") (list "d") ))
(check-expect (suffixes-abs (list "a" "b" "c" "d" "e")) (list (list "a" "b" "c" "d" "e") (list "b" "c" "d" "e") (list "c" "d" "e") (list "d" "e") (list "e") ))

(define (suffixes-abs lo1)
  (local (
          (define (create-suffix ls n)
            (cond
              [(equal? (length ls) (+ n 1)) ls]
              [else (create-suffix (rest ls) n)]))
          (define (given-suffixes n)
            (create-suffix lo1 n)))
    (reverse (build-list (length lo1) given-suffixes))))