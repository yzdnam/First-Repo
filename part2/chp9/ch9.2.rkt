;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(check-expect
  (average (cons 1 (cons 2 (cons 3 '())))) 2)

; List-of-temperatures -> Number
; computes the average temperature 
(define (average alot)
  (/ (sum alot) (how-many alot)))

(check-expect
  (sum (cons 1 (cons 2 (cons 3 '())))) 6)

; List-of-temperatures -> Number 
; adds up the temperatures on the given list 
(define (sum alot)
  (cond
    [(empty? alot) 0]
    [else (+ (first alot) (sum (rest alot)))]))
 
; List-of-temperatures -> Number 
; counts the temperatures on the given list 
(define (how-many alot)
  (cond
    [(empty? alot) 0]
    [else (+ (how-many (rest alot)) 1)]))

; EX 143
; NEList-of-temperatures -> Number
; computes the average temperature. Produces
; an informative error message when applied to
; '().
(define (checked-average alot)
  (cond
    [(empty? alot) (error alot" contains no temperatures. Aborting...")]
    [else (/ (ne-sum alot) (ne-how-many alot))]))

(check-error (checked-average '()) "'() contains no temperatures. Aborting...")

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures

; EX 144
; (sum x) and (how-many x) will not work on instances of NEList-of-temperatures
; because sum and how-many is called on '(), which is not an NEList-of-temperatures,
; within the their definitions.

; NEList-of-temperatures -> Number
; computes the sum of the given temperatures in non-empty list
(check-expect
  (ne-sum (cons 1 (cons 2 (cons 3 '())))) 6)
(define (ne-sum ne-l)
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [else (+ (first ne-l) (ne-sum (rest ne-l)))]))

; EX 145
; NEList-of-temperatures -> Boolean
; Produces #true if the temperatures are listed in descending order
; (1st>2nd>3rd...). Otherwise, it produces #false
(define (sorted>? ne-l)
  (cond
    [(empty? ne-l) (error ne-l" is an empty list. Aborting...")]
    [else (cond
            [(empty? (rest ne-l)) #true]
            [else (cond
                    [(> (first ne-l) (first (rest ne-l))) (sorted>? (rest ne-l))]
                    [else #false])])]))

(check-expect (sorted>? (cons 10 (cons 9 (cons 8 '())))) #true)
(check-expect (sorted>? (cons 8 (cons 9 '()))) #false)

; EX 146
; NEList-of-temperatures -> Number 
; counts the temperatures on the given non-empty list 
(define (ne-how-many alot)
  (cond
    [(empty? (rest alot)) (first alot)]
    [else (+ (ne-how-many (rest alot)) 1)]))

; An NEList-of-Booleans is one of: 
; – (cons boolean '())
; – (cons boolean NEList-of-Booleans)
; interpretation non-empty lists of boolean values

; EX 147
; NEList-of-Booleans -> Boolean
; consumes a non-empty list of Booleans. returns #true if all elements of the list
; are true. otherwise, it returns #false
(define (ne-all-true ne-list)
  (cond
    [(empty? (rest ne-list)) (first ne-list)]
    [else (if (first ne-list) (ne-all-true (rest ne-list)) #false)]))

(check-expect (ne-all-true (cons #true '())) #true)
(check-expect (ne-all-true (cons #true (cons #true '()))) #true)
(check-expect (ne-all-true (cons #false '())) #false)
(check-expect (ne-all-true (cons #true (cons #false '()))) #false)

; NEList-of-Booleans -> Boolean
; consumes a non-empty list of Booleans. returns #true if a single element of the list
; is true. otherwise, it returns #false
(define (ne-one-true ne-list)
  (cond
    [(empty? (rest ne-list)) (first ne-list)]
    [else (if (not (first ne-list)) (ne-one-true (rest ne-list)) #true)]))

(check-expect (ne-one-true (cons #true '())) #true)
(check-expect (ne-one-true (cons #true (cons #true '()))) #true)
(check-expect (ne-one-true (cons #false '())) #false)
(check-expect (ne-one-true (cons #true (cons #false '()))) #true)
(check-expect (ne-one-true (cons #false (cons #true '()))) #true)