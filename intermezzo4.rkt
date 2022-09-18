;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
          (define max-mantissa (inex-mantissa MAX-POSITIVE))
          (define max-exponent (inex-exponent MAX-POSITIVE))
          (define mantissa-sum (+ inex1-mantissa inex2-mantissa)))
    (cond
      [(equal? (* inex1-exponent inex1-sign) (* inex2-exponent inex2-sign)) (cond ; exponents are the same
                                                                              [(<= mantissa-sum max-mantissa) (create-inex mantissa-sum inex1-sign inex1-exponent)]
                                                                              [(and (> mantissa-sum max-mantissa) (equal? max-exponent inex1-exponent)) (error "Result is out of range")]
                                                                              [else (if (positive? inex1-sign) (create-inex (round (/ mantissa-sum base)) inex1-sign (add1 inex1-exponent))
                                                                                        (create-inex (round (/ mantissa-sum base)) inex1-sign (sub1 inex1-exponent)))])]
      [else
       (local ((define inex1-base-dropped-mantissa-sum (+ (* inex1-mantissa base) inex2-mantissa))
               (define inex2-base-dropped-mantissa-sum (+ (* inex2-mantissa base) inex1-mantissa))
               (define (inex+exp-diff1 bigger-inex smaller-inex)
                 (cond
                   [(and (> (round (/ (+ (* (inex-mantissa bigger-inex) base) (inex-mantissa smaller-inex)) base)) max-mantissa)
         (cond ; exponents differ by 1
           [(and (< inex1-fullexp inex2-fullexp)
                 (> (round (/ inex2-base-dropped-mantissa-sum base)) max-mantissa)
                 (equal? max-exponent inex2-exponent))
            (error "Result is out of range")]
           [(and (< inex1-fullexp inex2-fullexp)
                 (> (round (/ inex2-base-dropped-mantissa-sum base)) max-mantissa))
            (create-inex (round (/ inex2-base-dropped-mantissa-sum base)) inex2-sign (add1 inex2-exponent))]
           [(and (< inex1-fullexp inex2-fullexp)
                 (> inex2-base-dropped-mantissa-sum max-mantissa))
            (create-inex (round (/ inex2-base-dropped-mantissa-sum base)) inex2-sign inex2-exponent)]
           [(< inex1-fullexp inex2-fullexp) (create-inex inex2-base-dropped-mantissa-sum inex1-sign inex1-exponent)]
           [(and (> inex1-fullexp inex2-fullexp)
                 (> (round (/ inex1-base-dropped-mantissa-sum base)) max-mantissa)
                 (equal? max-exponent inex1-exponent))
            (error "Result is out of range")]
           [(and (> inex1-fullexp inex2-fullexp)
                 (> (round (/ inex1-base-dropped-mantissa-sum base)) max-mantissa))
            (create-inex (round (/ inex1-base-dropped-mantissa-sum base)) inex1-sign (add1 inex1-exponent))]
           [(and (> inex1-fullexp inex2-fullexp)
                 (> inex1-base-dropped-mantissa-sum max-mantissa))
            (create-inex (round (/ inex1-base-dropped-mantissa-sum base)) inex1-sign inex1-exponent)]
           [(> inex1-fullexp inex2-fullexp) (create-inex inex1-base-dropped-mantissa-sum inex2-sign inex2-exponent)]))])))

(check-expect (inex+ (create-inex 55 1 0) (create-inex 55 1 0)) (create-inex 11 1 1))
(check-expect (inex+ (create-inex 56 1 0) (create-inex 56 1 0)) (create-inex 11 1 1))
(check-expect (inex+ (create-inex 1 1 0) (create-inex 1 -1 1)) (create-inex 11 -1 1))
(check-expect (inex+ (create-inex 98 1 0) (create-inex 10 -1 1)) (create-inex 99 1 0))