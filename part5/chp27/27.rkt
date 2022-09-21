;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |27|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define SMALL 4) ; a size measure in terms of pixels 
 
(define small-triangle (triangle SMALL 'outline 'red))
 
; Number -> Image
; generative creates Sierpinski Δ of size side by generating
; one for (/ side 2) and placing one copy above two copies
 
(check-expect (sierpinski SMALL) small-triangle)
(check-expect (sierpinski (* 2 SMALL))
              (above small-triangle
                     (beside small-triangle small-triangle)))
 
(define (sierpinski side)
  (cond
    [(<= side SMALL) (triangle side 'outline 'red)]
    [else
     (local ((define half-sized (sierpinski (/ side 2))))
       (above half-sized (beside half-sized half-sized)))]))

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous 
; (2) (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in 
; one of the two halves, picks according to (2)
(define ε 0.1)
(define (find-root f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
      (local ((define mid (/ (+ left right) 2))
              (define f@mid (f mid)))
        (cond
          [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
           (find-root f left mid)]
          [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
           (find-root f mid right)]))]))

; EX 445
; use poly to formulate a check-satisfied test for find-root
; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

(check-satisfied (find-root poly 3 6) close-to-2-or-4?)

(define (close-to-2-or-4? x)
  (or (<= (abs (- x 2)) 0.1)
      (<= (abs (- x 4)) 0.1)))

; EX 446 experiment with different values for \epsilon

; EX 447 use find-root with poly and an interval that contains both roots
; any interval containing both roots of poly will have a left and right limit that are both positive which will cause cond to signal an error because
; all conditions will be false. this aligns with one of the conditions for the intermediate value theorem, namely that, given an interval of [a,b],
; f(a) and f(b) must be on opposite sides of x if the theorem is to hold for the function (the other condition being that the function must be continuous)

; EX 448 find-root terminates for all (continuous) f, left, and right for which the assumption holds because the difference
; between (f left) or (f right) and (f@mid) will approach zero after every call of the function.
; it takes ln(ϵ)/ln(1/2) calls of the function for (- right left) to be smaller than or equal to ϵ

; EX 449 improvement 1: re-write find-root so it doesn't compute the value of f for each boundary value more than once
; improvement 2: design a helper function that consumes (f left) and (f right) at each recursive stage, saving computational steps during the
; execution of the function
(define (find-root.449 f left right)
      (local ((define (find-root-helper f left right f-left f-right)
                (local (
                        (define mid (/ (+ left right) 2))
                        (define f@mid (f mid)))
                  (cond
                    [(<= (- right left) ε) left]
                    [else
                      (cond
                        [(or (<= f-left 0 f@mid) (<= f@mid 0 f-left))
                             (find-root-helper f left mid f-left f@mid)]
                        [(or (<= f@mid 0 f-right) (<= f-right 0 f@mid))
                             (find-root-helper f mid right f@mid f-right)])]))))
        (find-root-helper f left right (f left) (f right))))

(check-satisfied (find-root.449 poly 3 6) close-to-2-or-4?)

; EX 450 simplify find-root assuming the given function is not only continuous but also monotonically increasing.
; meaning if (< a b), then (<= (f a) (f b))
(define (find-root.450 f left right)
      (local ((define (find-root-helper f left right f-left f-right)
                (local (
                        (define mid (/ (+ left right) 2))
                        (define f@mid (f mid)))
                  (cond
                    [(<= (- right left) ε) left]
                    [else
                      (cond
                        [(<= f-left 0 f@mid)
                             (find-root-helper f left mid f-left f@mid)]
                        [(<= f@mid 0 f-right)
                             (find-root-helper f mid right f@mid f-right)])]))))
        (find-root-helper f left right (f left) (f right))))

(define (monotonic-func x)
  (- (expt x 2) 4))

(check-satisfied (find-root.450 monotonic-func 0 6) close-to-2-or-4?)

; EX 451
(define-struct table [length array])
; A Table is a structure:
;   (make-table N [N -> Number])

; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))

; Table -> N
; consumes a monotonically increasing table and finds the smallest index for a root of the given table
(define (find-linear tbl)
  (local ((define (root? ref)
            (<= -0.01 ref 0.01))
          (define (insert-index i)
            (cond
              [(root? (table-ref tbl i)) i]
              [(equal? i (table-length tbl)) (error "No root found")]
              [else (insert-index (add1 i))])))
    (insert-index 0)))
(check-expect (find-linear (make-table 6 monotonic-func)) 2)

; same as find-linear but using generative recursion by narrowing an interval down to the smallest possible size and then choosing the index
; termination: the interval that is used as an input to find-binary gets progressively smaller with each recursive call to the function until the function terminates once the
; interval reaches a pre-determined size
(define (find-binary tbl)
      (local ((define tbl-len (table-length tbl))
              (define (find-binary-helper f left right f-left f-right)
                (local (
                        (define mid (floor (/ (+ left right) 2)))
                        (define f@mid (f mid)))
                  (cond
                    [(equal? (- right left) 2) mid]
                    [else
                      (cond
                        [(<= f-left 0 f@mid)
                             (find-binary-helper f left mid f-left f@mid)]
                        [(<= f@mid 0 f-right)
                             (find-binary-helper f mid right f@mid f-right)])]))))
        (find-binary-helper (lambda (x) (table-ref tbl x)) 0 tbl-len (table-ref tbl 0) (table-ref tbl tbl-len))))
(check-expect (find-linear (make-table 6 monotonic-func)) 2)
; 1023 calls to find-linear to find the root in slot 1023 in a 1024 slot table
; 1024(1/2)^x = 2 => x = 9 calls to find-binary to find the same root in the same table