#lang racket

(require "hw4.rkt")

(define ones (lambda () (cons 1 ones)))
(define nats
  (letrec ([f (lambda (x) (lambda () (cons x (f (+ x 1)))))])
    (f 1)))

; 1. takes a list of numbers and evaluates to a list of numbers of the same length, where each element is obtained as follows: the first element should be the sum
; of the first and the last elements of the original list, the second one should be the sum of the second and second to last elements of the original list
(define (palindromic lon)
  (letrec ([len (length lon)]
           [loop (lambda (f l) (if (or (= f len) (< l 0))
                                   null
                                   (cons (+ (list-ref lon f) (list-ref lon l)) (loop (+ f 1) (- l 1)))))])
    (loop 0 (- (length lon) 1))))

(equal? (palindromic (list 1 2 4 8)) (list 9 6 6 9))

; 2. a fibonacci stream
(define fibonacci
  (letrec ([f (lambda (x y) (lambda () (cons x (lambda () (cons y (f (+ x y) (+ y (+ x y))))))))])
    (f 0 1)))

; 3. takes a function f and a stream s and applies f to the values of s in succession until f evaluates to #f
(define (stream-until f s)
  (let ([strm-pr (s)])
    (and (f (car strm-pr))
         (stream-until f (cdr s)))))

; 4.
(define (stream-map f s)
  (let ([strm-pr (s)])
    (lambda () (cons (f (car strm-pr)) (stream-map f (cdr strm-pr))))))

; 5. takes in two streams s1 and s2 and returns a stream that produces the pairs that result from the other two streams
(define (stream-zip s1 s2)
  (let ([strm-pr1 (s1)]
        [strm-pr2 (s2)])
    (lambda () (cons (cons (car strm-pr1) (car strm-pr2)) (stream-zip (cdr strm-pr1) (cdr strm-pr2))))))

; 6. thought experiment

; 7. takes a list of streams and produces a new stream that takes one element from each stream in sequence. so it will first produce the first value of the first
; stream, then the first value of the second stream and so on, and it will go back to the first stream when it reaches the end of the list
(define (interleave los0)
  (letrec ([cdr-all (lambda (los) (if (null? los) null
                                      (let ([first-cdr (cdr ((car los)))])
                                        (cons first-cdr (cdr-all (cdr los))))))]
           [loop (lambda (los next-los) 
                   (if (null? los)
                       (loop next-los (cdr-all next-los))
                       (let ([first-strm-pr ((car los))])
                         (lambda () (cons (car first-strm-pr) (loop (cdr los) next-los))))))])
    (loop los0 (cdr-all los0))))

; 8. takes an integer n and a stream s and returns a stream that produces the same values of s but packed in lists of n elements
(define (pack n s)
  (letrec ([loop (lambda (i strm) (if (<= i 0)
                                      (lambda () (cons null (pack n (cdr (strm)))))
                                      (letrec ([strm-pr (strm)]
                                               [rest-strm-car-thunk (loop (- i 1) (cdr strm-pr))])
                                        (lambda () (let ([rest-strm-car (rest-strm-car-thunk)])
                                                     (cons (cons (car strm-pr) (car rest-strm-car)) (cdr rest-strm-car)))))))])
    (loop n s)))

; 9. takes a number n, starts with n as an initial guess in the stream, and produces successive guesses applying Newton's Method
(define (sqrt-stream n)
  (letrec ([guess-eq (lambda (x) (let ([next-guess (* 0.5 (+ x (/ n x)))])
                                   (lambda () (cons x (guess-eq next-guess)))))])
    (guess-eq n)))

; 10. takes two numbers n and e and returns a number x such that x^2 is within e of n
(define (approx-sqrt n e)
  (letrec ([n-stream (sqrt-stream n)]
           [iter-stream (lambda (s) (let* ([strm-pr (s)]
                                           [ans (car strm-pr)])
                                      (if (< (abs (- (expt ans 2) n)) e)
                                          ans
                                          (iter-stream (cdr strm-pr)))))])
    (iter-stream n-stream)))

; 11. write a macro that has the following two forms:
; (perform e1 if e2)
; (perform e1 unless e2)
(define-syntax perform
  (syntax-rules (if unless)
    [(perform e1 if e2)
     (let ([e2-evald e2])
       (if e2-evald e1 e2-evald))]
    [(perform e1 unless e2)
     (let ([e2-evald e2])
       (if e2-evald e2-evald e1))]))

(perform "x" if (begin (print "a") #f))
(perform "x" unless (begin (print "a") #f))
    