;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |28.3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]
 
(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))
 
(define S '(1 1 2)) ; a Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))

; EX 462
; SOE Solution -> Boolean
; #true if the given solution result in the left hand side of each equation in the SOE equalling the right hand side of each equation
(define (check-solution soe solu)
  (local ((define (plug-in eq solu)
            (foldr (lambda (x y z) (+ z (* x y))) 0 eq solu)))
    (andmap (lambda (x) (equal? (rhs x) (plug-in (lhs x) solu))) soe)))
(check-expect (check-solution M S) #true)

; EX 463 - check that the following SOE has the same solution as M
(define M.463
  (list (list 2 2 3 10)
        (list 0 3 9 21)
        (list 0 0 1  2)))
(check-expect (check-solution M.463 S) #true)

; EX 464 - check that the following SOE has the same solution as M
(define M.464
  (list (list 2  2  3  10)
        (list 0  3  9  21)
        (list 0 -3 -8 -19)))
(check-expect (check-solution M.464 S) #true)

; EX 465
; Equation Equation -> Equation
; subtracts a multiple of the second equation from the first, item by item, so that the resulting equation has a 0 in the first position. returns the rest of the list that results
; from the subtractions
(define (subtract eq1 eq2)
  (cond
    [(zero? (first eq1)) (rest eq1)]
    [else (subtract (foldr (lambda (x y z) (cons (- x y) z)) '() eq1 eq2) eq2)]))
(check-expect (subtract (list 4 1 -2 1) (list 2 2 3 10)) (list -3 -8 -19))