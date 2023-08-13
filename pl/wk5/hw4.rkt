#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1. takes 3 args low, high, and stride, all assumed to be numbers. further, assume stride is positive. produces a list from low to high (including low and
; possibly high) separated by stride and in sorted order
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; 2. takes a list of strings xs and a string suffix and returns a list of strings. each element of the output should be the corresponding element of the input
; appended with suffix
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

; 3. takes a list xs and a number n. if the number is negative, terminate the computation with (error "list-nth-mod: negative number"). Else if the list is empty
; terminate the computation with (error "list-nth-mod: empty list"). Else return the ith element of the list where we count from zero and i is the remainder produced
; when dividing n by the list's length.
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; 4. takes a stream s and a number n. returns a list holding the first n values produced by s in order. assume n is non-negative
(define (stream-for-n-steps s n)
  (let ([strm-pr (s)])
  (if (zero? n)
      null
      (cons (car strm-pr) (stream-for-n-steps (cdr strm-pr) (- n 1))))))

; 5. write a stream that is like a stream of natural numbers except numbers divisible by 5 are negated
(define funny-number-stream
  (letrec ([f (lambda (x) (cons x (lambda () (f (cond [(zero? (remainder (+ x 1) 5)) (* -1 (+ x 1))]
                                                      [(= -4 (remainder (+ x 1) 5)) (* -1 (- x 1))]
                                                      [#t (+ x 1)])))))])
    (lambda () (f 1))))

; 6. a stream where the elements alternate between the strings "dan.jpg" and "dog.jpg" (starting with "dan.jpg")
(define dan-then-dog
  (letrec ([f (lambda (x) (cons x (lambda () (f (if (equal? x "dan.jpg")
                                                    "dog.jpg"
                                                    "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

; 7. takes a stream s and returns another stream. if s would produce v for its ith element, then (stream-add-zero s) would produce the pair (0 . v) for its ith element
(define (stream-add-zero s)
  (let ([strm-pr (s)])
    (lambda () (cons (cons 0 (car strm-pr)) (stream-add-zero (cdr strm-pr))))))

; 8. takes two lists xs and ys and returns a stream. the lists may or may not be the same length, but assume they are both non-empty. the elements produced by the
; stream are pairs where the first part is from xs and the second part is from ys. the stream cycles forever through the lists
(define (cycle-lists xs ys)
  (letrec ([lst-pr (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                                     (lambda () (lst-pr (+ n 1)))))])
    (lambda () (lst-pr 0))))

; 9.takes a value v and a vector vec. behaves like assoc except it processes a vector instead of a list and allows vector elements not to be pairs in which case it
; skips them and it always takes exactly two arguments
(define (vector-assoc v vec)
  (letrec ([check-vec (lambda (n) (if (= n (vector-length vec))
                                   #f
                                   (if (and (vector-ref vec n) (equal? (car (vector-ref vec n)) v))
                                       (vector-ref vec n)
                                       (check-vec (+ n 1)))))])
    (check-vec 0)))

; 10. takes a list xs and a number n and returns a function that takes one argument v and returns the same thing that (assoc v xs) would return using a cache
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [ready-slot 0]
           [f (lambda (v)
                (let ([check-cache (vector-assoc v cache)])
                  (if check-cache
                      check-cache
                      (let ([check-lst (assoc v xs)])
                        (if check-lst
                            (begin (vector-set! cache ready-slot check-lst)
                                   (if (= ready-slot (- n 1))
                                       (set! ready-slot 0)
                                       (set! ready-slot (+ 1 ready-slot)))
                                   check-lst)
                            check-lst)))))])
    f))

; 11. a macro that evaluates e1 once and e2 until its result is greater than or equal to the result of the evaluation of e1. if the evaluation terminates, the result
; is true. assume e1 and e2 produce numbers
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([e e1])
       (letrec ([loop (lambda (it)
                        (if (>= (it) e)
                            #t
                            (loop it)))])
         (loop (lambda () e2))))]))
                                     