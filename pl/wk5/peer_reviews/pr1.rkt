
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1.
;; Number Number Number -> listOfNumber
;; Produce list of number from low to high incremented by stride
;; Assume all arguments numbers
;; Assume stride is positive

(define (sequence low high stride)
  (if (> low high)
      null
      (append (list low) (sequence (+ low stride) high stride))))

;; 2.
;; listOfString String -> listOfString
;; Appends elements with suffix

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; 3
;; listofX Number -> X or error
;; return the ith element of the list
;; i is remainder produced when dividing n by list's length

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; 4
;; Stream Number -> ListOfX
;; returns a list holding the first n values produced by s in order
;; Assume n is non-negative

(define (stream-for-n-steps s n)
  (if (zero? n)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;; 5
;; Create a stream with naturals number where each 5 is negative

(define funny-number-stream
  (letrec ([g (lambda (y) (if (zero? (remainder y 5))
                              (- y)
                              y))]
           [f (lambda (x) (cons (g x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; 6
;; Create a stream that alternate betwee two strings "dan.jpg" and "dog.jpg"

(define dan-then-dog
  (lambda () (cons "dan.jpg" (lambda () (cons "dog.jpg" dan-then-dog)))))

;; 7
;; Stream -> Stream
;; produce (0.v) if stream produce v for its ith element

(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s)))))) 

;; 8
;; listofX listofY -> Stream
;; Make a stream out of the two list
;; Assume both list were non-empty

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;; 9
;; X Vector -> Pair or #f
;; Return first pair in vector that has value X
;; #f if has none

(define (vector-assoc v vec)
  (letrec ([vl (vector-length vec)]
           [f (lambda (n) (cond [(>= n vl) #f]
                                [(not (pair? (vector-ref vec n))) (f (+ n 1))]
                                [(equal? v (car (vector-ref vec n))) (vector-ref vec n)]
                                [#t (f (+ n 1))]))])
    (f 0)))

;; 10
;; listofX Number -> V -> X or f#
;; Return a function that takes one argument and works like assoc


(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [i 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (begin
                          (vector-set! memo i new-ans)
                          (if (>= i (- n 1))
                              (set! i 0)
                              (set! i (add1 i)))
                          new-ans)))))])
    f))