;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require math/base)

(define-struct inex [mantissa sign exponent])
; An Inex is a structure: 
;   (make-inex N99 S N99)
; An S is one of:
; – 1
; – -1
; An N99 is an N between 0 and 99 (inclusive).
; N Number N -> Inex
; makes an instance of Inex after checking the arguments

(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))
 
; Inex -> Number
; converts an inex into its numeric equivalent 
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt
       10 (* (inex-sign an-inex) (inex-exponent an-inex)))))

(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))
(define max-mantissa (inex-mantissa MAX-POSITIVE))
(define max-exponent (inex-exponent MAX-POSITIVE))
(define base 10)

; EX 412
; Inex Inex -> Inex
; adds two Inex representations of numbers that have the same exponent. Must be able to deal with inputs that increase the exponent.
; Must signal its own error if the result is out of range
(define (inex+ inex1 inex2)
  (local ((define inex1-mantissa (inex-mantissa inex1))
          (define inex2-mantissa (inex-mantissa inex2))
          (define inex1-exponent (inex-exponent inex1))
          (define inex2-exponent (inex-exponent inex2))
          (define inex1-sign (inex-sign inex1))
          (define inex2-sign (inex-sign inex2))
          (define inex1-fullexp (* inex1-exponent inex1-sign))
          (define inex2-fullexp (* inex2-exponent inex2-sign))
          (define mantissa-sum (+ inex1-mantissa inex2-mantissa)))
    (cond
      [(equal? (* inex1-exponent inex1-sign) (* inex2-exponent inex2-sign)) (cond ; exponents are the same
                                                                              [(<= mantissa-sum max-mantissa) (create-inex mantissa-sum inex1-sign inex1-exponent)]
                                                                              [(and (> mantissa-sum max-mantissa) (equal? max-exponent inex1-exponent)) (error "Result is out of range")]
                                                                              [else (if (positive? inex1-sign) (create-inex (round (/ mantissa-sum base)) inex1-sign (add1 inex1-exponent))
                                                                                        (create-inex (round (/ mantissa-sum base)) inex1-sign (sub1 inex1-exponent)))])]
      [else ; exponents differ by 1
       (local ((define (inex+exp-diff1 bigger-inex smaller-inex)
                 (local ((define big-mantissa (inex-mantissa bigger-inex))
                         (define small-mantissa (inex-mantissa smaller-inex))
                         (define big-expo (inex-exponent bigger-inex))
                         (define small-expo (inex-exponent smaller-inex))
                         (define big-sign (inex-sign bigger-inex))
                         (define small-sign (inex-sign smaller-inex))
                         (define base-adjusted-mantissa-sum (+ (* big-mantissa base) small-mantissa))
                         )
                   (cond 
                     [(and (> (round (/ base-adjusted-mantissa-sum base)) max-mantissa) ; sum of mantissa's will increase exponent and exponent is already max'd ie (99^99 + 10^98)
                           (equal? max-exponent big-expo))
                      (error "Result is out of range")]
                     [(> (round (/ base-adjusted-mantissa-sum base)) max-mantissa) ; sum of mantissa's will increase exponent ie (99 + (10/10))
                      (create-inex (round (/ base-adjusted-mantissa-sum base)) big-sign (add1 big-expo))]
                     [(> base-adjusted-mantissa-sum max-mantissa) ; sum will remain in the larger operand's base ie (98 + (10/10))
                      (create-inex (round (/ base-adjusted-mantissa-sum base)) big-sign big-expo)]
                     [else (create-inex base-adjusted-mantissa-sum small-sign small-expo)])))) ; sum will remain in the smaller operand's base ie (1 + (1/10))
         (cond 
           [(< inex1-fullexp inex2-fullexp) (inex+exp-diff1 inex2 inex1)]
           [(> inex1-fullexp inex2-fullexp) (inex+exp-diff1 inex1 inex2)]))])))

(check-expect (inex+ (create-inex 55 1 0) (create-inex 55 1 0)) (create-inex 11 1 1))
(check-expect (inex+ (create-inex 56 1 0) (create-inex 56 1 0)) (create-inex 11 1 1))
(check-expect (inex+ (create-inex 1 1 0) (create-inex 1 -1 1)) (create-inex 11 -1 1))
(check-expect (inex+ (create-inex 98 1 0) (create-inex 10 -1 1)) (create-inex 99 1 0))

; EX 413
; inex inex -> inex
; multiplies two inex representations of numbers, including inputs that force an additional increase of the output's exponent
; must signal its own error if the result is out of range
(define (inex* inex1 inex2)
  (local ((define inex1-mantissa (inex-mantissa inex1))
          (define inex2-mantissa (inex-mantissa inex2))
          (define orig-in-true-mantissa (* inex1-mantissa inex2-mantissa))
          (define inex1-exponent (inex-exponent inex1))
          (define inex2-exponent (inex-exponent inex2))
          (define inex1-sign (inex-sign inex1))
          (define inex2-sign (inex-sign inex2))
          (define inex1-fullexp (* inex1-exponent inex1-sign))
          (define inex2-fullexp (* inex2-exponent inex2-sign))
          (define full-exp-sum (+ inex1-fullexp inex2-fullexp))

          ; returns amount the exponent will be increased in the product
          (define (determine-expo-increase true-mantissa n)
            (cond
              [(< true-mantissa max-mantissa) n]
              [else (determine-expo-increase (/ true-mantissa base) (add1 n))]))
          (define (determine-new-mantissa true-mantissa)
            (cond
              [(< true-mantissa max-mantissa) true-mantissa]
              [else (determine-new-mantissa (/ true-mantissa base))])))
    (create-inex (round (determine-new-mantissa orig-in-true-mantissa))
                 (if (positive? full-exp-sum)
                     1 -1)
                 (+ full-exp-sum
                    (determine-expo-increase orig-in-true-mantissa 0)))))

(check-expect (inex* (create-inex 2 1 4) (create-inex 8 1 10)) (create-inex 16 1 14))
(check-expect (inex* (create-inex 20 1 1) (create-inex  5 1 4)) (create-inex 10 1 6))
(check-expect (inex* (create-inex 27 -1 1) (create-inex  7 1 4)) (create-inex 19 1 4))

; EX 414
; Number -> Number
; adds up n copies of #i1/185
(define (add n)
  (cond
    [(zero? n) 0]
    [else (+ #i1/185 (add (sub1 n)))]))

(check-within (add 1) (/ 1 185) 0.0001)

; counts how often 1/185 can be subtracted from the argument until it is 0
(define (sub n)
  (local (
          (define (subtract-from-n n x)
            (cond
              [(zero? n) x]
              [else (subtract-from-n (- n 1/185) (add1 x))])))
    (subtract-from-n n 0)))

(check-expect (sub 0) 0)
(check-expect (sub 1/185) 1)

; EX 415
; determine the integer n such that (expt #i10.0 n) is an inexact number while (expt #i10.0 (+ n 1)) is approximated with +inf.0
; answer is 308 determined using the function below
; the function below determines the number of times 10 must be multiplied by itself to reach a number
(define (10power x initial goal)
  (cond
    [(equal? initial goal) x]
    [else (10power (+ x 1) (* initial #i10.0) goal)]))

; EX 416
; determine smallest integer n such that (expt #i10.0 n) is still an inexact ISL+ number and (expt #i10.0 (- n 1)) is approximated with 0
; answer is -323 determined using the function below
(define (power x initial goal)
  (cond
    [(equal? initial goal) x]
    [else (power (- x 1) (/ initial #i10.0) goal)]))

(define (abs-10power count-opr pwr-opr goal)
  (local ((define (count-operations x initial)
            (cond
              [(equal? initial goal) x]
              [else (count-operations (count-opr x 1) (pwr-opr initial #i10.0))])))
    (count-operations 1 #i10.0)))

(check-expect (abs-10power - / #i0.0) -324)

; EX 417 Evaluate (expt 1.001 1e-12) in Racket and in ISL+ and explain the results

; EX 418
; raises the first given number to the power of the second one
(define (my-expt x y)
  (cond
    [(zero? y) 1]
    [else (* x (my-expt x (sub1 y)))]))

(define inex-num (+ 1 #i1e-12))
(define exac-num (+ 1 1e-12))

(define JANUS
  (list 31.0
        #i2e+34
        #i-1.2345678901235e+80
        2749.0
        -2939234.0
        #i-2e+33
        #i3.2e+270
        17.0
        #i-2.4e+270
        #i4.2344294738446e+170
        1.0
        #i-8e+269
        0.0
        99.0))

; EX 420
(define (oscillate n)
  (local ((define (O i)
            (cond
              [(> i n) '()]
              [else
               (cons (expt #i-0.99 i) (O (+ i 1)))])))
    (O 1)))