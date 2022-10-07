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
  (local ((define first-eq1 (first eq1))
          (define quotient (/ first-eq1 (first eq2))))
    (cond
      [(zero? (first eq2)) (error "First member of subtracting equation is zero. Terminating.")]
      [(zero? first-eq1) (rest eq1)]
      [else (rest (foldr (lambda (x y z) (cons (- x (* y quotient)) z)) '() eq1 eq2))])))

(check-expect (subtract (list 4 1 -2 1) (list 2 2 3 10)) (list -3 -8 -19))
(check-expect (subtract (list -3 -8 -19) (list 3 9 21)) (list 1 2))

; EX 466 - design triangulate which triangulates the given system of equations
; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix

; SOE -> TM
; triangulates the given system of equations 
(define (triangulate M)
  (local ((define base-eq (first M))
          (define soe-w-base-subbed (cons base-eq (map (lambda (x) (subtract x base-eq)) (rest M))))
          (define (tm? M)
            (cond
              [(empty? (rest M)) #true]
              [(equal? (length (first M)) (+ 1 (length (second M)))) (tm? (rest M))]
              [else #false])))
    (cond
      [(tm? M) M]
      [else (cons base-eq (triangulate (rest soe-w-base-subbed)))])))

(check-expect (triangulate M) (list (list 2 2  3 10)
                                    (list   3  9 21)
                                    (list      1  2)))

; EX 467 - revise triangulate so that it rotates the equations first to find one with a leading coefficient that is not 0 before it subtracts the first equation from the remaining
; ones
; EX 468 - add an error condition for when an SOE has all of its leading coefficients equal to 0
; SOE -> TM
(define (triangulate.467 M)
  (local ((define base-eq (first M))
          (define (tm? M)
            (cond
              [(empty? (rest M)) #true]
              [(equal? (length (first M)) (+ 1 (length (second M)))) (tm? (rest M))]
              [else #false]))
          (define (all-leading-0s? M)
            (andmap (lambda (x) (zero? (first x))) M)))
    (cond
      [(tm? M) M]
      [(all-leading-0s? M) (error "Every equation has a leading coefficient of 0. SOE is unsolvable. Terminating.")]
      [(zero? (first base-eq)) (triangulate.467 (append (rest M) (list base-eq)))]
      [else (local ((define soe-w-base-subbed (cons base-eq (map (lambda (x) (subtract x base-eq)) (rest M)))))
              (cons base-eq (triangulate.467 (rest soe-w-base-subbed))))])))

(check-expect (triangulate.467 (list (list 2  3  3 8)
                                     (list 2  3 -2 3)
                                     (list 4 -2  2 4))) (list (list 2  3  3   8)
                                                              (list   -8 -4 -12)
                                                              (list      -5  -5)))

(check-error (triangulate.467 (list (list 2 2 2 6)
                                    (list 2 2 4 8)
                                    (list 2 2 1 2))) "Every equation has a leading coefficient of 0. SOE is unsolvable. Terminating.")

; EX 469 - design a solve function which consumes a triangular SOE and produces a solution
; TM -> Solution
(define (solve TM)
  (cond
    [(empty? (rest TM)) (list (/ (rhs (first TM)) (first (lhs (first TM)))))]
    [else (local ((define cur-solutions-list (solve (rest TM)))
                  ; List List -> Number
                  ; accepts an equation with one variable not solved for and n other expressions. the other argument is a list of n solutions that correspond to the expressions
                  ; in the given equation.
                  ; multiplies the list of solutions already found with their corresponding coefficient in the given equation. solves for the one unsolved variable in the given
                  ; equation
                  (define (next-solution cur-eq solutions-list)
                    (local ((define coes-w-solved-vars (rest (lhs cur-eq)))
                            (define sum-of-coes-times-solved-vars (foldr (lambda (x y z) (+ (* x y) z)) 0 coes-w-solved-vars solutions-list)))
                      (/ (- (rhs cur-eq) sum-of-coes-times-solved-vars) (first cur-eq)))))
            (cons (next-solution (first TM) (solve (rest TM))) (solve (rest TM))))]))

(check-expect (solve (list (list 2  3  3   8)
                           (list   -8 -4 -12)
                           (list      -5  -5))) (list 1 1 1))

; solve designed using an existing abstraction and lambda.
(define (challenge-solve TM)
  (foldr (lambda (eq solutions) (cons (/ (- (rhs eq) (foldr (lambda (x y z) (+ (* x y) z)) 0 (rest (lhs eq)) solutions)) (first eq)) solutions))
         (list (/ (rhs (first (reverse TM))) (first (lhs (first (reverse TM))))))
         (reverse (rest (reverse TM)))))

(check-expect (challenge-solve (list (list 2  3  3   8)
                                     (list   -8 -4 -12)
                                     (list      -5  -5))) (list 1 1 1))

; EX 470 - combine the triangulate and solve functions
; SOE -> Solution
(define (gauss M)
  (solve (triangulate M)))

(check-expect (gauss M) S)