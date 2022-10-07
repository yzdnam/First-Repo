;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |23|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; EX 387
; [List-of Symbol] [List-of Number] -> [List-of Symbol-Number-Pair]
; produces all possible ordered pairs of symbols and numbers in the given lists
(define (cross los lon)
  (local ((define (one-symb-all-num symb lon)
            (cond
              [(empty? lon) '()]
              [else (cons (list symb (first lon)) (one-symb-all-num symb (rest lon)))])))
     (foldr append '() (map (lambda(x) (one-symb-all-num x lon)) los))))

(check-expect (cross '(a) '(1)) '((a 1)))
(check-expect (cross '(a b) '(1)) '((a 1) (b 1)))
(check-expect (cross '(a b c) '(1)) '((a 1) (b 1) (c 1)))
(check-expect (cross '(a) '(1 2)) '((a 1) (a 2)))
(check-expect (cross '(a b c) '(1 2)) '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))

; EX 388 - modify wages*.v2 to consume a list of employee structures and a list of work records
(define-struct employee-structure [name ssn rate])
(define-struct work-record [name hours])
(define-struct employee-wage [name wage])
; [List-of employee-structure] [List-of work-record] -> [List-of employee-wage]
; multiplies the corresponding items on 
; hours and wages/h 
; assume the two lists are of equal length
(check-expect (wages*.v2 (list (make-employee-structure "charles" 001 10) (make-employee-structure "shane" 002 11))
                         (list (make-work-record "charles" 2) (make-work-record "shane" 4)))
              (list (make-employee-wage "charles" 20) (make-employee-wage "shane" 44)))
(check-expect (wages*.v2 (list (make-employee-structure "chuck" 001 5.65)) (list (make-work-record "chuck" 40)))
              (list (make-employee-wage "chuck" 226.0)))
(define (wages*.v2 lostructs lowrecs)
  (cond
    [(empty? lowrecs) '()]
    [else
     (cons
       (weekly-wage (first lostructs) (first lowrecs))
       (wages*.v2 (rest lostructs) (rest lowrecs)))]))

; employee-structure work-record -> employee-wage
; computes the weekly wage from pay-rate and hours
(define (weekly-wage struct wrkrec)
  (make-employee-wage (employee-structure-name struct) (* (employee-structure-rate struct) (work-record-hours wrkrec))))

; EX 389
(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)
; [List-of String] [List-of String] -> [List-of PhoneRecord]
(check-expect (zip (list "chuck") (list "555-555-5555")) (list (make-phone-record "chuck" "555-555-5555")))
(check-expect (zip (list "chuck" "mark" "lou") (list "555-555-5555" "555-555-5554" "555-555-5553"))
              (list (make-phone-record "chuck" "555-555-5555") (make-phone-record "mark" "555-555-5554") (make-phone-record "lou" "555-555-5553")))
(define (zip lonames lonumbers)
  (cond
    [(empty? lonumbers) '()]
    [else
     (cons
      (phone-rec (first lonames) (first lonumbers))
      (zip (rest lonames) (rest lonumbers)))]))

; String String -> PhoneRecord
(define (phone-rec name number)
  (make-phone-record name number))

; N is one of: 
; – 0
; – (add1 N)

; [List-of Symbol] N -> Symbol
; extracts the nth symbol from l; 
; signals an error if there is no such symbol
(define (list-pick l n)
   (cond
    [(and (= n 0) (empty? l))
     (error "list too short")]
    [(and (> n 0) (empty? l))
     (error "list too short")]
    [(and (= n 0) (cons? l)) (first l)]
    [(and (> n 0) (cons? l)) (list-pick (rest l) (sub1 n))]))

(check-expect (list-pick '(a b c) 2) 'c)
(check-error (list-pick '() 0) "list too short")
(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error (list-pick '() 3) "list too short")

(define-struct branch [left right])
 
; EX 390
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
 
; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path.

; EX 392 - Simplify tree-pick
; TOS [List-of Direction] -> TOS
; Follows a TOS using the given list of directions
(define tos1 (make-branch (make-branch 'cone 'fog) (make-branch (make-branch 'fart 'pee) 'poop)))
(check-expect (tree-pick (make-branch (make-branch 'cone 'fog) (make-branch (make-branch 'fart 'pee) 'poop)) (list 'right 'left 'right)) 'pee)
(check-error (tree-pick 'fart (list 'right)) "no branch to follow. aborting...")
(check-expect (tree-pick tos1 '()) tos1)
(check-expect (tree-pick 'fart '()) 'fart)
(define (tree-pick tos lod)
  (cond
    [(empty? lod) tos]
    [(branch? tos) (local (; Direction -> TOS
                           (define (pick-branch direct)
                             (cond
                               [(equal? direct 'left) (branch-left tos)]
                               [(equal? direct 'right) (branch-right tos)])))
                     (tree-pick (pick-branch (first lod)) (rest lod)))]
    [(not (branch? tos)) (error "no branch to follow. aborting...")]))

; EX 391
; [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end
(define (replace-eol-with front end)
  (cond
    [(and (empty? front) (empty? end)) '()]
    [(and (empty? front) (cons? end)) end]
    [(empty? end) front]
    [(cons? end) (cons (first front) (replace-eol-with (rest front) end))]))

(check-expect (replace-eol-with (cons 1 '()) '(a))
              (cons 1 '(a)))
(check-expect (replace-eol-with
                (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))
(check-expect (replace-eol-with '() '(a b)) '(a b))

; EX 393
; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
; 
; Constraint If s is a Son.R, 
; no number occurs twice in s
; Son.R Son.R -> Son.R
; produces a set that contains the elements that occur in both given sets
(define (intersect l1 l2)
  (cond
    [(or (empty? l1) (empty? l2)) '()]
    [else (if (member? (first l1) l2)
              (cons (first l1) (intersect (rest l1) l2))
              (intersect (rest l1) l2))]))

(check-expect (intersect (list 1 2 3 4) (list 2 3 4)) (list 2 3 4))

; produces a set that contains all the elements occuring in either given set
(define (union l1 l2)
  (cond
    [(and (empty? l1) (empty? l2)) '()]
    [(empty? l1) l2]
    [(empty? l2) l1]
    [else (if (member? (first l1) l2)
              (union (rest l1) l2)
              (cons (first l1) (union (rest l1) l2)))]))

(check-expect (union (list 1 2 3 4) (list 2 3 4)) (list 1 2 3 4))

; [List-of Number] [List-of Number] -> [List-of Number]
; consumes two lists of numbers, sorted in ascending order and produces a single sorted list that contains all the numbers on both input lists
; a number occurs in the output as many times as it occurs on the two input lists together
(define (merge l1 l2)
  (cond
    [(and (empty? l1) (empty? l2)) '()]
    [(empty? l1) l2]
    [(empty? l2) l1]
    [else (if (< (first l1) (first l2))
              (cons (first l1) (merge (rest l1) l2))
              (cons (first l2) (merge l1 (rest l2))))]))

(check-expect (merge (list 1 2 3 4 5) (list 3 4 5 6)) (list 1 2 3 3 4 4 5 5 6))
(check-expect (merge (list 3 4 5 6) (list 1 2 3 4 5)) (list 1 2 3 3 4 4 5 5 6))

; EX 395
; [List-of X] N -> [List-of X]
; produces the first N items from the given list or all of the list if it is too short
(define (take l n)
  (cond
    [(or (= n 0) (empty? l)) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

(check-expect (take '() 5) '())
(check-expect (take (list 1 2 3 4 5) 8) (list 1 2 3 4 5))
(check-expect (take (list 1 2 3 4 5) 2) (list 1 2))

; [List-of X] N -> [List-of X]
; results in a list with the first N items removed or just '() if l is too short
(define (drop l n)
  (cond
    [(or (= n 0) (empty? l)) l]
    [else (drop (rest l) (sub1 n))]))

(check-expect (drop '() 4) '())
(check-expect (drop (list 1 2 3 4 5) 3) (list 4 5))
(check-expect (drop (list 1 2 3 4 5) 8) '())