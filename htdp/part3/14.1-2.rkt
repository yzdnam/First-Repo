;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 14.1-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

; EX 235
; Los -> Boolean
; does l contain "atom"
(define (contains-atom? l)
  (contains? "atom" l))

; EX 236
; Lon -> Lon
; adds 1 to each item on l
(define (add1* l)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (add1 (first l))
       (add1* (rest l)))]))

(check-expect (add1** (list 1 4 2)) (list 2 5 3))
(check-expect (add1** '()) '())

; Lon -> Lon
; adds 5 to each item on l
(define (plus5 l)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (+ (first l) 5)
       (plus5 (rest l)))]))

(check-expect (plus5* (list 1 4 2)) (list 6 9 7))
(check-expect (plus5* '()) '())

; number lon -> lon
(define (add* num lon)
  (cond
    [(empty? lon) '()]
    [else
     (cons
      (+ (first lon) num)
      (add* num (rest lon)))]))

(define (add1** l)
  (add* 1 l))

(define (plus5* l)
  (add* 5 l))

(define (subtract* num lon)
  (add* (* -1 num) lon))

(define (sub2 l)
  (subtract* 2 l))

(check-expect (sub2 (list 1 4 2)) (list -1 2 0))
(check-expect (sub2 '()) '())

; Number Number -> Boolean
; is the area of a square with side x larger than c
(define (squared>? x c)
  (> (* x x) c))

; operator Nelon -> Number
; determines either the largest or smallest number in a list depending on the given operator
(define (most opr l)
  (cond [(empty? (rest l))
         (first l)]
        [else
         (if (opr (first l)
                  (most opr (rest l)))
             (first l)
             (most opr (rest l)))]))

; operator Nelon -> Number
; returns the smallest number in a given list
(define (inf-1 l)
  (most < l))

; operator Nelon -> Number
; returns the largest number in a given list
(define (sup-1 l)
  (most > l))

; Nelon -> Number
; determines the smallest number in l using min
(define (most* max-or-min l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (max-or-min (first l) (most* max-or-min (rest l)))]))

(define (inf-2 l)
  (most* min l))

(define (sup-2 l)
  (most* max l))

; Nelon -> Number
; determines the smallest number on l
(define (inf.v2 l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest (inf.v2 (rest l))))
       (if (< (first l) smallest-in-rest)
           (first l)
           smallest-in-rest))]))

; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s 
; #false otherwise 
(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)
(define (occurs s los)
  (cond
    [(not (contains? s los)) #false]
    [else (cond
            [(equal? s (first los)) (rest los)]
            [else (occurs s (rest los))])]))
  